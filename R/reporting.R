##' This function reveives the results of a gene-set enrichment analysis and the original reference databases to generate plots and html code elements for the final report.
##'
##' This function is called by the cmapAnalysis function
##' @title gCMAPWeb reporting function
##' @param cmap.result CMAPResults object
##' @param reference eSet object, the full reference database, typically an NChannelSet or CMAPCollection object
##' @param reference.name character, the name of the reference database
##' @param annotation.db character, the name of the annotation package used to lookup gene identifiers
##' @param query a GeneSet, SignedGeneSet or ExpressionSet object used as query for the gene-set-enrichment analysis giving rise to 'cmap.result'
##' @param element character, the assayDataElementName to extract from NChannelSet objects
##' @param tmp_filename character, the name of the temporary output folder
##' @param title character, title of the report
##' @param max.results integer, maximum number of results to return
##' @param min.found integer, minimum number of gene set members that need to overlap with the user query. Gene sets with matching members less than min.found will not be returned as results.
##' @param max.padj numeric, maximum adjusted p-value for a gene set to be considered significantly similar. Results with adjusted p-values larger than max.padj will not be returned. 
##' @param gene.level.report logical, create gene-level reports ?
##' @param gene.level.plot logical, create gene-level plots ?
##' @param reportDirectory character, path to temporary output directory for this session. Default=tempdir()
##' @param excluded.cols character vector listing columns in the CMAPResults objects that should NOT be included in the report 
##' @param swap.colnames list, containing alternative names for CMAPResults columns.
##' @return a list of character strings, either containing html code snippets or paths to result files. These elements are used to brew the final result html page.
##' @author Thomas Sandmann
generate_report <- function( cmap.result, reference, reference.name, annotation.db,
                             element, query, tmp_filename, title="", 
                             max.results=getOption( "max.results", default=50),
                             min.found = getOption( "min.found", default=1), 
                             max.padj = getOption( "max.padj", default=0.1), 
                             gene.level.report=getOption( "gene.level.report", default=TRUE), 
                             gene.level.plot=getOption( "gene.level.plot", default=TRUE),
                             reportDirectory=tempdir(),
                             excluded.cols=getOption( "excluded.cols", 
                                                      default=c("geneScores", "signed", "pval", "UID", 
                                                                "z.shift", "log_fc.shift", "mod_fc.shift")),
                             swap.colnames=getOption( "swap.colnames", default=list(padj="FDR", nFound="Genes"))
                             ){
  
  ##--------- adjust parameters for single-gene queries
  if( inherits( query, "character")){
    min.found <- 1
    max.results <- Inf
    gene.level.report <- FALSE
    gene.level.plot <- FALSE
    excluded.cols <- union( excluded.cols, c("nSet", "Genes", "effect"))
    overview.main <- sprintf("Differential expression of\nEntrez id %s", query)
    up.label <- "Upregulated"
    down.label <- "Downregulated"
  } else {
    overview.main <- "Distribution of similarity scores"
    up.label <- "Correlated"
    down.label <- "Anticorrelated"
  }
  
  ##--------- create output directories  
  result.dir <- file.path( reportDirectory, "results", tmp_filename, reference.name)
  dir.create(result.dir, showWarnings=FALSE, recursive=TRUE)
  
  figure.dir <- file.path( result.dir, "figures")
  dir.create(figure.dir, showWarnings=FALSE, recursive=TRUE)
  
  ##---- remove results for sets with 0 genes in the universe (e.g. instances without signif. changes)
  cmap.result <- cmap.result[ nSet(cmap.result) > 0 ,]
  if( nrow( cmap.result) > 0){
    
    ## extract effect size for all instances
    effect.population <- effect( cmap.result )
    
    ##---- limit detailed reportes to top n results
    cmap.result <- cmap.result[ (nFound(cmap.result) >= min.found) &
                                !is.na(padj(cmap.result)) & 
                                (padj(cmap.result) < max.padj),] 
    if(nrow( cmap.result ) == 0){
      return(
        list(header=NULL,
             error=sprintf("<div class='alert alert-info span6'>No instances with FDR < %s were identified in this reference dataset.'</div>",
                           max.padj),
             overview=NULL,
             overview.legend=NULL,
             heatmap=NULL,
             heatmap.legend=NULL,
             header.details=NULL,
             table=NULL,
             legend=NULL,
             header.downloads=NULL,
             download=NULL
        )
      )
    } else {
      
      ## overview density plot
      overview.html <- create_overview_plot( effect.sample=na.omit(effect( cmap.result )[1:min( nrow(cmap.result),max.results)]), 
                                             effect.population=na.omit(effect.population), 
                                             file.name=file.path( figure.dir, "overview"), 
                                             reference.name=reference.name,
                                             main=overview.main, 
                                             xlab=varMetadata(cmap.result)["effect",], 
                                             url.base=NULL, up.label=up.label, 
                                             down.label=down.label)
      
      overview.legend.html <- create_overview_legend()
      
      cmap.result <- cmap.result[1:min(max.results, nrow( cmap.result)),] ## return up to max.results 
      top.results <- cmapTable( cmap.result )
      
      ## tab-delimited file
      selected.columns <- setdiff( colnames( top.results), excluded.cols) 
      tab.html <- create_tab( top.results[,selected.columns], 
                              result.dir, file.name="report.tab", url.base=reference.name)
      
      zip.html <- hwrite("Download complete report as zip archive",
                         link=file.path( "..", paste(tmp_filename, "zip", sep=".")),
                         br=TRUE)
      
      heatmap.html <- NULL
      heatmap.legend.html <- NULL
      scores <- NULL
      col.anno <- NULL
      ##------  gene-level reports
      if(gene.level.report ==TRUE){
        gene.level.reports  <- create_gene_report( cmap.result, query, 
                                                   url.base=reference.name,
                                                   result.dir=file.path(result.dir, "gene_reports"), 
                                                   figure.dir=figure.dir,
                                                   annotation.db=annotation.db,
                                                   reference.cmap=reference,
                                                   element=element)
        
        ## save intermediate files
        if( getOption( "save.intermediates", default=FALSE) == TRUE){
          save( gene.level.reports, file=file.path(tempdir(),"gene_level_reports.rdata"))
        }

        ## gene.level.reports contains three elements: 
        ## 1. nFound.url: a list of urls to the gene-level reports 
        ## 2. Image: a list of strings containing the html code for inserting small 
        ## figures into the html table 
        ## 3. gene.table: a data.frame with gene ids, gene scores, gene sign (if available)
        top.results$nFound.url <- sapply(gene.level.reports, function(x) x[["nFound.url"]] )
        top.results$Image <- sapply(gene.level.reports, function(x) x[["Image"]] )      
        
        ## create overview heatmap (for directional & non-directional queries) showing gene-level scores        
        if( getOption( "show.heatmap", default=TRUE) ==  TRUE){
          if( class( query ) %in% c("GeneSet", "SignedGeneSet")){
            
            scores <- t( sapply( gene.level.reports, function(x){
              if( "Diff. expr. score" %in% colnames( x$gene.table )){
                as.numeric( x$gene.table[,"Diff. expr. score"])
              } else {
                NULL
              }
            }))       
            if( !inherits( scores, "matrix")){
              scores <- matrix(scores, nrow=1)
            }
            
            col.anno <- lapply( gene.level.reports, function(x){
              if( "Original direction" %in% colnames( x$gene.table )){
                x$gene.table[,"Original direction"]
              } else {
                NULL
              }
            })[[1]]
            row.anno <- trend( cmap.result)
            file.name=file.path( figure.dir, "heatmap")

            if( getOption( "save.intermediates", default=FALSE) == TRUE){
              save( scores, col.anno, row.anno, reference.name, file=file.path(tempdir(),"heatmap_data.rdata"))
            }
            
            if( !is.null(scores) 
               && !all(is.null( unlist(scores ))) 
               && (sum( !is.na(scores)) > 0 )){
              heatmap.res <- try(
                ## returns list with image.html and dendrogram branch number
                cmapHeatmap( x=scores,
                             col.anno=col.anno,
                             row.anno=row.anno,
                             file.name=file.path( figure.dir, "heatmap"),
                             reference.name=reference.name,
                             url.base=NULL
              )
              , silent=TRUE)
             
              if( inherits( heatmap.res, "try-error")){
                heatmap.html <-NULL
                row.order <- NULL
              } else {
                heatmap.html <- heatmap.res[["image.html"]]
                if( !is.null( heatmap.res[["branches"]] )){
                  top.results <- data.frame( Branch=heatmap.res[["branches"]], 
                                             top.results, stringsAsFactors=FALSE )
                }
              }
              
              if( !is.null(heatmap.html )){
                heatmap.legend.html <- create_heatmap_legend()
              }
            } 
          }
        } 
      }  
      ## swap column names, e.g. rename padj to FDR
      for( col.name in names( swap.colnames)){
        if( col.name %in% colnames( top.results)){
          colnames( top.results)[ which(colnames( top.results) == col.name) ] <- swap.colnames[[col.name]]
        }
      }
      if( "LOR" %in% colnames( top.results)){
        top.results[,"LOR"] <- format(top.results[,"LOR"], scientific=FALSE, digits=3)
      }
      
      ## use columns with url suffix as links
      top.results <- addLinks( top.results, swap.colnames=swap.colnames)
      
      ## remove uniformative columns
      empty.cols <- colnames(top.results)[ which( apply( top.results, 2, function(x) all(is.na(x)))) ] 
      excluded.cols <- c( excluded.cols, empty.cols)
      selected.columns <- setdiff( colnames( top.results), excluded.cols) 
      top.results <- top.results[,selected.columns, drop=FALSE]
      
      ## data table
      table.html <- html_table( top.results, paste(reference.name, "table", sep="_") )
      
      legend.html <- create_legend( cmap.result, 
                                    reference.name, 
                                    swap.colnames=swap.colnames,
                                    keep=colnames(top.results))
      
      return(
        list( header=NULL,
              error=NULL,
              overview=overview.html,
              overview.legend=overview.legend.html,
              heatmap=heatmap.html,
              heatmap.legend=heatmap.legend.html,
              header.details="<legend><h4>Details</h4></legend>",
              table=table.html,
              legend=legend.html,
              header.downloads="<legend><h4>Downloads</h4></legend>",
              download=paste(tab.html, zip.html, sep="\n")
        ))
    }
    
  } else { ## empty results object
    return(
      list(header=NULL,
           error=sprintf("<div class='alert alert-info span6'>No instances with significant similarity (FDR < %s) were identified in this reference dataset.'</div>",
                           max.padj),
           overview=NULL,
           overview.legend=NULL,
           heatmap=NULL,
           heatmap.legend=NULL,
           header.details=NULL,
           table=NULL,
           legend=NULL,
           header.downloads=NULL,
           download=NULL
      ))
  }
}
##'  This function compiles a gene-level report from a CMAPResults object.
##'
##' Usually called by the generate_report function
##' @param cmap.result a CMAPResults object
##' @param query a GeneSet, SignedGeneSet or ExpressionSet object used as query for the gene-set-enrichment analysis giving rise to 'cmap.result'
##' @param result.dir character, path to gene_results folder in the per-session output directory
##' @param url.base character, the name of the reference database
##' @param reference.cmap eSet, the reference database used for the gCMAP analysis
##' @param figure.dir character, path to figure folder in the per-session output directory
##' @param element character, the assayDataElementName to extract from NChannelSet objects
##' @param annotation.db character, the name of the annotation package used to lookup gene identifiers
##' @param gene.level.plot, logical: should gene-level plots be included in the report ?
##' @return character string with the relative url to the report html, which is directly written to disk.
##' @author Thomas Sandmann
create_gene_report<-function (cmap.result, query, result.dir, url.base, reference.cmap, 
                              figure.dir, element = getOption("element", default = "z"), 
                              annotation.db = "org.Hs.eg.db", 
                              gene.level.plot = getOption("gene.level.plot", default = TRUE)
                              ) {
  if (!inherits(cmap.result, "CMAPResults")) {
    stop("create_gene_report: cmap.result is not a CMAPResults object")
  }
  if (nrow(cmap.result) == 0) {
    stop("CMAPResults object is empty.")
  }
  if (any(nFound(cmap.result) == 0)) {
    stop("CMAPResults object contains sets with nFound = 0")
  }
  dir.create(result.dir, showWarnings = FALSE, recursive = TRUE)
  gene.report.url <- lapply(1:nrow(cmap.result), function(i) {
    gene.sign <- gene.score <- score.table <- gene.set.members <- NA
    set.name <- as.character(set(cmap.result[i, ]))
    
    ## directional query
    if (inherits(query, "SignedGeneSet")) {
      gene.ids <- geneIds(query)
      gene.sign <- as.character(geneSign(query))
      if (!inherits(reference.cmap, "CMAPCollection")) {
        if (inherits(reference.cmap, "eSet")) {
          gene.ids.found <- which(gene.ids %in% featureNames(reference.cmap))
          gene.scores <- rep(NA, length(gene.ids))
          score.table <- sapply( 
            setdiff( assayDataElementNames( reference.cmap ), 
                     getOption( "excluded.cols", default=NULL)),
            function( score.col ) { 
              assayDataElement( reference.cmap, score.col)[,set.name, drop=FALSE]
              })
          row.names( score.table ) <- featureNames( reference.cmap)
          background.scores <- score.table[, element] 
          gene.scores[gene.ids.found] <- background.scores[gene.ids[gene.ids.found]]
          #          background.scores <- assayDataElement(reference.cmap, 
          #                                                element)[, set.name]
          score.table <- score.table[gene.ids[gene.ids.found],,drop=FALSE]
          gene.legend <- gene_density_chart_legend()
          gene.table.title <- "Results for your query genes"
        }
      }
    }
    
    ## non-directional query
    else if (inherits(query, "GeneSet")) {
      gene.ids <- geneIds(query)
      gene.sign <- NULL
      if (!inherits(reference.cmap, "CMAPCollection")) {
        if (inherits(reference.cmap, "eSet")) {
          gene.ids.found <- which(gene.ids %in% featureNames(reference.cmap))
          gene.scores <- rep(NA, length(gene.ids))
          score.table <- sapply( 
            setdiff( assayDataElementNames( reference.cmap ), 
                     getOption( "excluded.cols", default=NULL)),
            function( score.col ) { 
              assayDataElement( reference.cmap, score.col)[,set.name, drop=FALSE]
            })
          row.names( score.table ) <- featureNames( reference.cmap)
          background.scores <- score.table[, element] 
          gene.scores[gene.ids.found] <- background.scores[gene.ids[gene.ids.found]]
          score.table <- score.table[gene.ids[gene.ids.found],,drop=FALSE]
          gene.legend <- gene_density_chart_legend()
          gene.table.title <- "Results for your query genes"
        }
      }
      else {
        gene.scores <- rep(NA, length(gene.ids))
        gene.legend <- gene_pie_chart_legend()
        gene.table.title <- "Overlap of your query genes with this reference set"
      }
    }
    
    ## profile query
    else if (inherits(query, "ExpressionSet")) {
      gene.scores <- geneScores(cmap.result[i, ])[[1]]
      score.table <- data.frame( "Differential expression score" = gene.scores)
      colnames( score.table ) <- gsub( ".", " ", colnames( score.table), fixed=TRUE)
      gene.ids <- names(gene.scores)
      gene.sign <- attr(gene.scores, "sign")
      background.scores <- exprs(query)
      gene.legend <- gene_density_profile_legend()
      gene.table.title <- "Query scores for genes in this reference set"
    }
    name.column <- grep("Name", colnames(cmapTable(cmap.result)), 
                        ignore.case = TRUE)[1]
    set.name <- ifelse(!is.na(name.column), 
                       as.character(cmapTable(cmap.result)[i, name.column]), 
                       as.character(set(cmap.result)[i]))
    if (all(is.na(gene.scores))) {
      .gene_set.pie(n.set = nSet(cmap.result[i, ]), n.found = nFound(cmap.result[i, 
                                                                                 ]), set.name = set.name, n = i, figure.dir)
      gene.set.members <- names(geneScores(cmap.result[i, 
                                                       ])[[1]])
    } else {
      .gene_score_density(gene.scores = gene.scores, gene.signs = gene.sign, 
                          background.scores = background.scores, id = i, 
                          title = set.name, figure.directory = figure.dir)
    }
    figure.small <- ifelse(is.null(url.base), file.path("figures", 
                                                        paste("mini", i, "png", sep = ".")), file.path(url.base, 
                                                                                                       "figures", paste("mini", i, "png", sep = ".")))
    figure.large <- ifelse(is.null(url.base), file.path("figures", 
                                                        paste("large", i, "png", sep = ".")), file.path(url.base, 
                                                                                                        "figures", paste("large", i, "png", sep = ".")))
    Image <- hwriteImage(figure.small, link = figure.large, 
                         table = FALSE, height = 75)
    table_title <- paste("Gene-level results for", set.name, 
                         sep = " ")
    file.name <- paste("gene_report", i, sep = "_")
    gene.table <- retrieve_annotation(gene.ids, annotation.db)
    colnames(gene.table) <- c("EntrezId", "Symbol", "Name")
    if (!is.null(gene.sign)) {
      if (!all(is.na(gene.sign))) {
        gene.table["Original direction"] <- gene.sign
      }
    }
    ## add gene scores, if available
    if (!is.null(score.table)) {
      if (!all(is.na(score.table))) {
        
        gene.table <- merge( x=gene.table, 
                             y=signif( score.table, digits=2),
                             by.x="EntrezId",
                             by.y=0
                             )
      }
    }
    if (!is.null(gene.set.members)) {
      if (!all(is.na(gene.set.members))) {
        in.overlap <- ifelse(gene.table$EntrezId %in% 
                               gene.set.members, "yes", "no")
        gene.table["Query gene found in reference set"] <- in.overlap
      }
    }
    tab.html <- create_tab(gene.table, result.dir, file.name = paste(file.name, 
                                                                     "tab", sep = "."))
    gene.html <- gene.table
    gene.html$EntrezId <- paste("<a href=\"http://www.ncbi.nlm.nih.gov/gene/", 
                                gene.html$EntrezId, "\">", gene.html$EntrezId, "</a>", 
                                sep = "")
    html.table <- html_table(gene.html, table_id = "gene_table")
    
    overview.figure <- hwriteImage(file.path("..", "figures", 
                                             paste("large", i, "png", sep = ".")), table = FALSE, 
                                   br = TRUE, width = 500)
    sink(file = paste(file.path(result.dir, file.name), "html", 
                      sep = "."))
    html_header(url.base = "../..")
    body_first(url.base = "../..")
    cat(sprintf("<legend><h4>%s</h4></legend>", table_title))
    cat("<div class='span10'>")
    cat(overview.figure)
    cat("<div class='span6'>")
    cat(gene.legend, sep = "\n")
    cat("</div>")
    cat( "<div class='clearfix'></div>")
    cat(sprintf("<legend><h4>%s</h4></legend>", gene.table.title))

    ## add table legend
    if (!is.null(score.table)) {
      if (!all(is.na(score.table))) {
        legend.text <- create_gene_table_legend( colnames( score.table ), reference.cmap)
        cat( legend.text )
      }
    }
    #     cat( "<span class='label label-info pull-left' data-toggle='collapse' data-target='#figure_legend'>",
#          "Table legend</span>",
#          "<div class='span12'></div>",
#          "<div id='figure_legend' class='collapse span12'>",
#          sep="\n") 
#     cat( "<div class='span12'>")
#     cat( legend.text, "<br>", sep="\n")
#     cat("<br></div>")      
#     cat( "</div>")
    
    cat(html.table, sep = "\n")
    cat(tab.html)
    cat("</div>")
    cat("<br>")
    html_body_last(url.base = "../..")
    sink()
    if (!is.null(url.base)) {
      nFound.url <- file.path(url.base, "gene_reports", 
                              paste("gene_report_", i, ".html", sep = ""))
    }
    else {
      nFound.url <- file.path("gene_reports", paste("gene_report_", 
                                                    i, ".html", sep = ""))
    }
    return(list(nFound.url = nFound.url, Image = Image, gene.table = gene.table))
  })
  names(gene.report.url) <- as.character(set(cmap.result))
  return(gene.report.url)
}

##------- helper functions
##' This function exports results in tab-delimited format
##'
##' @title Create tab-delimited output
##' @param df data.frame
##' @param result.dir character, path to the output directory
##' @param url.base  character, the name of the reference database
##' @param file.name character, name of the report file
##' @return character string with the html code pointing to the download URL
##' @author Thomas Sandmann
create_tab <- function( df, result.dir, url.base=NULL, file.name){
  write.table(df, file=file.path( result.dir, file.name), quote=FALSE, sep="\t", row.names=FALSE)
  link <- ifelse( is.null(url.base), file.name, file.path( url.base, file.name))
  tab.html <- hwrite("Download above results as tab-delimited text file",
                     link=link,
                     br=TRUE)
  return( tab.html )
}
##'  This function exports the complete report in a zip file
##'
##' @title Export gCMAPWeb report as zip archive
##' @param tmp_filename character, name of the per-session result directory to be archived
##' @param out.dir character, output directory to save zip archive into
##' @return Nothing, the zip archive is written to disk.
##' @author Thomas Sandmann
create_zip <- function(tmp_filename, out.dir){
  try({
    curr.wd <- getwd()
    setwd(out.dir)
    system( sprintf("zip -r9X %s %s",tmp_filename, tmp_filename), ignore.stdout =TRUE)
    setwd( curr.wd )},
      silent=TRUE
  )
}

##' This function identifies column pairs from a data.frame that differ only in a string suffix (default: .url).
##' The column with the suffix is used to add http href tags to the other column and is then removed.
##'
##' To annotate columns with links to other urls, the urls must be included in the data.frame in a separate column matching the target column name but carrying the additional suffix in the column name. For example, to add links to the 'id' column, an additional id.url column can be used. Multiple columns can be processed simultaneously, if each of them has a matching url column.
##' @title Adding links to columns of a data.frame
##' @param df data.frame, must contain both target and url columns
##' @param pattern character, the suffix linking target and url columns. Default=c('.url')
##' @param swap.colnames list, containing alternative names for CMAPResults columns.
##' @return Data frame with href html tags in the target column(s)
##' @author Thomas Sandmann
addLinks <- function(df, pattern=".url$", swap.colnames=getOption( "swap.colnames", default=list(padj="FDR", nFound="Genes"))){
  ## make sure the original column.names are considered
  for( term in swap.colnames){
    if( term %in% colnames( df )){
      colnames( df )[which( colnames( df ) == term)] <- names( swap.colnames)[which( swap.colnames == term )]
    }
  }
  
  ## FIXME: this is only a temporary fix until our datasets have systematic colnames
  colnames( df ) <- sub( "Accession", "set.url", colnames( df ))
  
  ## identify url columns based on 'pattern'
  url.columns <- grep(pattern, colnames( df ), value=TRUE)
  if(length( url.columns ) > 0){
    ## assign url columns to matched annotation column
    names(url.columns) <- sub(pattern,"", url.columns)
    ## create links
    for( col in names( url.columns) ){
      if( col %in% colnames( df )){
        df[ col ] <- hwrite(as.character( df[, col ] ),
                            link=df[, url.columns[ col ] ],
                            table=FALSE
        )
        ## remove original href column
        df[, url.columns[ col ] ] <- NULL
      }
    }
  }
  
  ## reswap the colnames
  for( term in names(swap.colnames)){
    if( term %in% colnames( df )){
      colnames( df )[which( colnames( df ) == term)] <- swap.colnames[[term ]]
    }
  }
  
  return( df )
}

##' This function retrieves the official gene symbol and name for Entrez identifiers
##'
##' @title Gene symbol and name lookup
##' @param entrez character, Entrez identifier to look up
##' @param annotation.db character, name of the annotation package to use for lookup
##' @return data.frame with entrez, symbol and name columns
##' @author Thomas Sandmann
retrieve_annotation <-function(entrez, annotation.db){
  symbol <- unlist(AnnotationDbi::mget(entrez, getAnnMap("SYMBOL", annotation.db), ifnotfound=NA))
  name <- unlist(AnnotationDbi::mget(entrez, getAnnMap("GENENAME", annotation.db), ifnotfound=NA))
  converted.ids <- data.frame(entrez=entrez, symbol=symbol, name=name)
  return(converted.ids)
}

##' This function converts a CMAPResults object into a data.frame and adds the gene.level scores as an additional column
##'
##' @title CMAPResults data.frame with gene-level scores
##' @param cmap.result CMAPResults object
##' @return A data.frame with gene-level scores in column 'Scores'
##' @author Thomas Sandmann
add_geneScores_to_df <- function(cmap.result){
  results.raw <- cmapTable( cmap.result)
  if( !is.null( geneScores( cmap.result ) ) ){
    results.raw$Genes <- sapply( geneScores( cmap.result ), function( x ) {
      if( all(is.na( names(x) ))){ "NA"} else {paste( names(x) , collapse=",")                                 }
    })
    results.raw$Scores <- sapply( geneScores( cmap.result ), function( x ) {
      if( all(is.na( x  ))){ "NA"} else {paste( x , collapse=",")}
    })
    results.raw$geneScores <- NULL
  }
}
##' This function reads a CMAPResults object and returns the html code to display a legend for all columns that have more descriptive annotations in the varMetadata slot.
##' A standard set of columns is selected for display in the legend through the 'keep' parameter. In addition, all columns with annotations in the labelDescription field of the varMetadata slot are included if the field content differs from the column name.
##'
##' @param res CMAPResults object
##' @param reference.name character, ame of the reference database used
##' @param keep character, a vector with column names that are always included in the legend
##' @param swap.colnames  list, containing alternative names for CMAPResults columns.
##' @return character string with the html code element required to insert the legend into the html report
##' @author Thomas Sandmann
create_legend <- function( res, reference.name, 
                           keep=c("set", "trend", "FDR", "effect", "nSet","Genes"), 
                           swap.colnames=getOption( "swap.colnames", default=list(padj="FDR", nFound="Genes")) ){
  df <- varMetadata( res )
  
  ## remove rows without additional annotation
  informative <- sapply( 1:nrow(df), function(n) {
    row.names(df)[n] != df[n,"labelDescription"]
  })
  df <- df[informative,,drop=FALSE]
  if( nrow( df ) > 0){
    ## swap CMAPResult slot names to more human-readable form
    for( term in names( swap.colnames)){
      if( term %in% row.names( df)){
      row.names( df )[which( row.names( df ) == term)] <- swap.colnames[[term]]
      }
    }
    
    ## subset to requested varMetada rows
    df <- subset(df, subset=( row.names( df) %in% intersect(keep, row.names(df))))
    
    ## generate html code
    html.code <- paste("<dl class='dl-horizontal'>",
                       paste(
                         sapply( 1:nrow( df ), function( n ){
                           sprintf("<dt>%s</dt><dd>%s</dd>", 
                                   row.names( df )[n], 
                                   df[n,"labelDescription"] )
                         }), collapse=" " ),
                       "</dl>", collapse=" ")
    
    sprintf("<span class='label label-info pull-left' data-toggle='collapse' data-target='#%s'>Table legend</span><div id='%s' class='collapse span12'>%s</div><br>", paste(reference.name, "legend", sep="_"), paste(reference.name, "legend", sep="_"), html.code )
    
  } else {
    NULL
  }
}
