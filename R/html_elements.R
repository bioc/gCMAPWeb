##' Function to format a data.frame as an html table
##'
##' This function uses the thead and tbody tags required for the dataTables javascript to create interactive html tables.
##' @title Format data.frame as html table
##' @param df A data frame
##' @param table_id Character, specifying the id tag for the html table
##' @return Html code as a character string.
##' @author Thomas Sandmann
html_table <- function( df, table_id){
  
  NumCheck <- function(x){
    Result <- suppressWarnings(as.numeric(as.character(x)))
    if (any(!is.na(Result))) TRUE else FALSE
  }
  
  ## identify numeric columns and add class 'scientific' to the html table column
  numeric.columns <- colnames( df )[ sapply(df, NumCheck)]
  
  ## round exprs columns to 1 decimal
  exprs.columns <- intersect( numeric.columns, c("exprs"))
  if( length( exprs.columns ) > 0){
    for( col in exprs.columns){
      df[,col] <- round(as.numeric(as.character(df[,col])), digits=1)
    }
  }

  ## round effect columns to 2 decimals
  effect.columns <- intersect( numeric.columns, c("effect", "log_fc", "mod_fc"))
  if( length( effect.columns ) > 0){
    for( col in effect.columns){
      df[,col] <- round(as.numeric(as.character(df[,col])), digits=2)
    }
  }
  
  ## display p-values in scientific notation with 3 decimals
  p.vals <- intersect( c("padj", "FDR", "pval", "p", "p-value"), numeric.columns)
  if( length( p.vals ) > 0){
    for( col in p.vals){
      df[,col] <- format(as.numeric(df[,col]), scientific=TRUE, digits=3)
    }
  }
  
  ## identify image columns and add class 'nosort' to the html table column
  non.sortable <- c("Image")
  table.header <- sapply( colnames( df ), function( x ) { 
    if( x %in% numeric.columns ){
      sprintf("<th class='scientific'>%s</th>", x) 
    } else  if( x %in% non.sortable ){
      sprintf("<th class='nosort'>%s</th>", x) 
    } else {
      sprintf("<th>%s</th>", x) 
    }
  })
  table.content <- strsplit(hwrite( df, row.names=FALSE, col.names=FALSE), "\n")[[1]]
  table.content <- table.content[c(-1, -length(table.content))] ## remove table tags
  
  html.table <- c(
    "\n",
    ## start table
    sprintf("<table id='%s' class='table table-striped table-bordered'>", table_id),
    ## define table header
    paste("<thead>", "<tr>", sep="\n"),
    table.header,
    paste("</tr>","</thead>", sep="\n"),
    ## define table content
    "<tbody>",
    paste(table.content, collapse="\n"),
    "</tbody>",
    ## close table
    "</table>"
  )
  return(html.table)
}

##' Html header including link to the bootstrap cerulean css stylesheet
##'
##' @title Html header and bootstrap css stylesheet
##' @param url.base character, path to htdocs directory (optional)
##' @return Html code as a character string
##' @author Thomas Sandmann
html_header <- function( url.base=NULL ){
  cat( "<!DOCTYPE html>", "<html>","<head>", sep="\n")
  ## bootstrap css
  cat(
    ifelse( is.null(url.base), 
            "<link href='css/bootstrap.cerulean.min.css' rel='stylesheet'>",
            sprintf( "<link href='%s' rel='stylesheet'>", 
                     file.path(url.base, 'css', 'bootstrap.cerulean.min.css'))
    ), sep="\n"
  )
  
  ## fine-tuning the header
  cat(
    "<style>", 
    "body {",
    "padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */",
    "}",
    "TD{font-size: 9pt;}",
    "TH{font-size: 9pt;}",
    "</style>", 
    sep="\n"
  )
  
  ## bootstrap responsive css
  cat(
    ifelse( is.null(url.base), 
            "<link href='css/bootstrap-responsive.min.css' rel='stylesheet'>",
            sprintf( "<link href='%s' rel='stylesheet'>", file.path(url.base, 'css','bootstrap-responsive.min.css'))
    ), sep="\n"
  )
  
  ## dataTable integration with bootstrap
  if( getOption( "table.javascript", default=TRUE) == TRUE){
    cat(
      ifelse( is.null(url.base), 
              "<link href='css/DT_bootstrap.css' rel='stylesheet'>",
              sprintf( "<link href='%s' rel='stylesheet'>", file.path(url.base, 'css','DT_bootstrap.css'))
      ), sep="\n"
    )
  }
  
  ## special treatment of IE 9
  cat(
    "<!--[if lt IE 9]>",
    "<script src='http://html5shim.googlecode.com/svn/trunk/html5.js'></script>",
    "<![endif]-->"
  )
}
##' Closing html code, including references to jquery, bootstrap and dataTables javascript libraries
##'
##' @title Html footer
##' @param url.base character, path to htdocs directory (optional)
##' @return Html code as character string
##' @author Thomas Sandmann
html_body_last <- function(url.base=NULL){
  ## jquery
  cat(
    ifelse( is.null(url.base),
            "<script src='js/jquery-1.8.2.min.js'></script>",
            sprintf("<script src='%s'></script>", file.path(url.base, 'js', 'jquery-1.8.2.min.js'))
    ), sep="\n"
  )
  ## bootstrap JS
  cat(
    ifelse( is.null(url.base),
            "<script src='js/bootstrap.min.js'></script>",
            sprintf("<script src='%s'></script>", file.path(url.base, 'js', 'bootstrap.min.js'))
    ), sep="\n"
  )
  
  ## dataTables
  if( getOption( "table.javascript", default=TRUE) == TRUE){
    cat(
      ifelse( is.null(url.base),
              "<script src='js/jquery.dataTables.min.js'></script>",
              sprintf("<script src='%s'></script>", file.path(url.base, 'js', 'jquery.dataTables.min.js'))
      ), sep="\n"
    )
    cat(
      ifelse( is.null(url.base),
              "<script src='js/jquery.dataTables.plugins.js'></script>",
              sprintf("<script src='%s'></script>", file.path(url.base, 'js', 'jquery.dataTables.plugins.js'))
      ), sep="\n"
    )
    cat(
      ifelse( is.null(url.base),
              "<script src='js/DT_bootstrap.js'></script>",
              sprintf("<script src='%s'></script>", file.path(url.base, 'js', 'DT_bootstrap.js'))
      ), sep="\n"
    )
    
    ## activate jquery datatable function for all tabs
    cat("<script>",
        "$(document).ready(function() {",
        "  $('table').dataTable({",
        "    'aaSorting': [],",
        "    'aoColumnDefs': [{ 'sType': 'scientific', 'aTargets': [ 'scientific' ]},",
        "                     { 'bSortable': false, 'aTargets': [ 'nosort' ] }",
        "    ],",
        "  })",
        "});",      
        "</script>",
        sep="\n")  
  }
}

##' This function formats a data.frame with the results of the gene identifier conversion to html code, including an interactive dataTable.
##' The html page is saved to disk and the path to the output file is returned as a character string.
##'
##' @title Identifier conversion html output
##' @param df data.frame with old / new gene identifiers
##' @param result.dir character, path to result directory
##' @param file.name character, output filename
##' @param tmp_filename character, name of the session-specific subdirectory
##' @param url.base character, path to htdocs directory (optional)
##' @param message character, optional message to display above the result table
##' @return character, name of the output html file
##' @author Thomas Sandmann
conversion_html <- function( df, result.dir, file.name, tmp_filename, 
                             url.base=NULL, message=NULL
                            ){
  dir.create(result.dir, showWarnings=FALSE, recursive=TRUE)
  
  html.table <- html_table( df, "id_conversion" )
  
  tab.html <- create_tab( df, result.dir,
                          file.name=paste( file.name, "tab", sep="."))
  
  ## create output file
  sink(file = paste(file.path( result.dir, file.name ), "html", sep="."))
  html_header(url.base=".")
  brew::brew( file=system.file("htdocs/brew/body_first.html", package="gCMAPWeb"))
  cat( "<legend><h4>Identifier conversion</h4></legend>")
  cat( "<div class='span10'>")
  cat( message )
  cat( "<br>" )
  cat( html.table, sep="\n" )
  cat( tab.html )
  cat("</div>")
  cat( "<br>" )
  html_body_last(url.base=".")
  sink()
  
  conversion.html <- paste(file.name, "html", sep=".")
  return(conversion.html)
}

##' This function generates the html code required to generate the query type selection buttons on the main index.rhtml page
##'
##' @title Html code for the query type selection buttons on the gCMAPWeb index page
##' @param conf_data list, the configuration data as returned by the read_config_file function
##' @return Html code as character string.
##' @author Thomas Sandmann
inputType_buttons <- function( conf_data ){
  supported.inputTypes <- getOption( "supported.inputType", default=c("single", "non-directional", "directional", "profile"))

  cat("<div class='btn-group'>\n")
  if( "single" %in% supported.inputTypes){
    cat("<a href='single_input.rhtml' class='btn btn-primary btn-large' rel='popover' data-content='Lookup result for a single gene identifier.' data-original-title='Single gene query' data-placement='bottom' data-trigger='hover'><i class='icon-search icon-white'></i>",
        "Gene lookup",
        "</a>", sep="\n")
  }
  
  if( "non-directional" %in% supported.inputTypes){
    cat("<a href='set_input.rhtml' class='btn btn-primary btn-large' rel='popover' data-content='A single list of gene identifiers, potentially containing both up- and down-regulated genes.' data-original-title='Non-directional query' data-placement='bottom' data-trigger='hover'>",
        "Non-directional query",
        "</a>", sep="\n")
  }
  if("directional" %in% supported.inputTypes){
    cat("<a href='signed_input.rhtml' class='btn btn-primary btn-large' rel='popover' data-content='Separate lists for up- and down-regulated genes.' data-original-title='Directional query' data-placement='bottom' data-trigger='hover'>",
        "Directional query",
        "</a>", sep="\n")
  }
  if("profile" %in% supported.inputTypes){
    cat("<a href='profile_input.rhtml' class='btn btn-primary btn-large' rel='popover' data-content='Full results from a differential expression analysis, e.g. identifiers and z-scores for all genes represented on the microarray.' data-original-title='Expression profile' data-placement='bottom' data-trigger='hover'>
",
        "Profile query",
        "</a>", sep="\n")
  }
  cat("</div>\n")
}

##' This function generates the html code required to display radio buttons for all species specified in the gCMAPWeb configuration file
##'
##' @title Html code for species radio button selectors
##' @param conf_data list, the configuration data as returned by the read_config_file function
##' @return Html code as character string.
##' @author Thomas Sandmann
species_radio_html <- function( conf_data ){
  ## first species (checked)
  cat( "<label class='radio'>",
       sprintf("<input type=\"radio\" name=\"species\" id=\"species_%s\" value=\"%s\" checked onclick=\"toggleSpecies()\">
                 %s", 
               names(conf_data$species)[1], 
               names(conf_data$species)[1], 
               names(conf_data$species)[1]
       ), 
       "</label>", sep="\n"
  )
  if( length(conf_data$species) > 1){
    ## remaining species (unchecked)
    for( species.name in names(conf_data$species)[2:length(conf_data$species)] ){
      cat( "<label class='radio'>",
           sprintf("<input type=\"radio\" name=\"species\" id=\"species_%s\" value=\"%s\" onclick=\"toggleSpecies()\">
                 %s", species.name, species.name, species.name), 
           "</label>", sep="\n"
      )
    }
  }  
}

##' This function generates the html code required to display radio buttons for the supported identifier types specified in the gCMAPWeb configuration file
##'
##' @title Html code for identifier type radio button selectors
##' @param conf_data list, the configuration data as returned by the read_config_file function
##' @param single, logical, use singular nouns for radiobutton labels ?
##' @return Html code as character string.
##' @author Thomas Sandmann
identifier_radio_html <- function( conf_data, single=FALSE ){
  supported.idTypes <- getOption( "supported.idType", 
                                  default=c("symbol", "entrez", "probe"))
  if( single == FALSE){ 
    plural <- "s"
  } else {
    plural <- ""
  }
  if( "entrez" %in% supported.idTypes){
    cat("<label class='radio'  rel='popover' data-content='Gene identifier assigned by NCBI's entrez database, e.g. 7157' data-placement='right' data-trigger='hover'>",
        "<input type='radio' name='idType' value='entrez' checked onclick='togglePlatforms()'>",
        sprintf("Entrez identifier%s",plural),
        "</label>", sep="\n")
  }
  if( "symbol" %in% supported.idTypes){
    cat("<label class='radio' rel='popover' data-content='Offical gene symbol, e.g. TP53' data-placement='right' data-trigger='hover'>",
        "<input type='radio' name='idType' value='symbol' onclick='togglePlatforms()'>",
        sprintf("Gene symbol%s",plural),
        "</label>", sep="\n")
  }
  if("probe" %in% supported.idTypes){
    cat("<label class='radio' rel='popover' data-content='Probe identifiers specified by the microarray manufacturer, e.g. 211300_s_at' data-placement='right' data-trigger='hover'>",
        "<input type='radio' name='idType' value='probe' onclick='togglePlatforms()'>",
        sprintf("Probe identifier",plural),
        "</label>", sep="\n")
  }
}


##' This function generates the html code required to display radio buttons for all array platforms specified in the gCMAPWeb configuration file
##'
##' @title Html code for platform radio button selectors
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @return Html code as character string.
##' @author Thomas Sandmann
platform_radio_html <- function( conf_data ){
  all.platforms <- sapply( conf_data$species, "[[", "platforms")
  for( species.name in names( all.platforms )){ ## species.name, e.g. "human", "mouse"
    cat( sprintf( "<select name='platform' species='%s' class='span2'>\n", species.name))
    for( platform in all.platforms[[species.name]]) {
      cat( sprintf( "<option rel=\"popover\" data-content=\"%s\" data-placement=\"right\" data-trigger=\"hover\">
                    %s\n", platform, platform))
      cat( "</option>\n")
    }
    cat("</select>")
  } 
}
##' This function generates the html code required to display radio buttons for the reference datasets specified in the gCMAPWeb configuration file
##'
##' @title Html code for reference cmap radio button selectors
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @param reference.cmaps list containing all reference cmaps as eSet-like objects (e.g. NChannelSet, CMAPCollection)
##' @return Html code as character string.
##' @author Thomas Sandmann
reference_radio_html <- function( conf_data, reference.cmaps ){
  all.cmaps <- lapply( conf_data$species, "[[", "cmaps")
  for( species.name in names( all.cmaps )){
    for( reference.cmap in names( all.cmaps[[ species.name ]])){
      cmap <- reference.cmaps[[ reference.cmap ]]
      ## check if supported query types have been specified in the eSet
      if( !is.null( cmap@experimentData@other$supported.query )){
        supported.query <- cmap@experimentData@other$supported.query
      } else {     
        ## assign supported query types based on object class
        if( inherits( cmap, "NChannelSet") | inherits( cmap, "ExpressionSet")){
          ## Quantitative reference datasets support all four query types
          supported.query <- c("single", "unsigned", "directional", "profile")          
        } else if( inherits( cmap, "CMAPCollection")){
          if( all( signed( cmap ))){
            ## signed CMAPCollections support profile searches
            supported.query <- c("unsigned", "profile")
          } else{
            ## unsigned CMAPCollections support only unsigned searches
            supported.query <- c("unsigned")
          }
        }
      }
      data.content <- ifelse( abstract( cmap ) != "", 
                              abstract( cmap ), ## read abstract slot if available
                              paste( ncol( cmap ), "experiments")
      )
      data.name <- ifelse( experimentData( cmap )@title != "", 
                           experimentData( cmap )@title, ## read notes slot if available
                           reference.cmap
      )
      
      cat( sprintf( "<label class=\"checkbox\" name=\"cmaps\" species=\"%s\" ", species.name))
      cat( sprintf( "supported_query_type=\"%s\" ", paste( supported.query, collapse=" ")))
      if( !is.null( data.content )){
        cat( sprintf( "rel=\"popover\" data-content=\"%s\" data-original-title=\"%s\" ", data.content, data.name))
      }
      cat("data-placement=\"right\" data-trigger=\"hover\">\n")
      cat( sprintf( "<input type=\"checkbox\" name=\"cmaps\" species=\"%s\" ", species.name))
      
      cat( sprintf( "supported_query_type=\"%s\" ", paste( supported.query, collapse=" ")))
      cat( sprintf( "value=\"%s\" cmap=\"single_cmap\">\n", reference.cmap))
      cat( sprintf("%s </label>\n", data.name))
    } 
  }
}

##' Javascript code to hide html elements for all but the first species listed in the configuration file
##'
##' @title Javascript code to hide html elements
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @return javascript call as character string
##' @author Thomas Sandmann
hide_species_js <- function( conf_data ){
  if( length( names(conf_data$species) ) >1){
    for( species.name in names(conf_data$species)[2:length(conf_data$species)] ){
      cat( sprintf( "$('[species=\"%s\"]').prop('disabled',true);\n", species.name))
      cat( sprintf( "$('[species=\"%s\"]').hide();\n", species.name))
    }
  }
}

##' Html code for generating and example query button on the single gene query submission page
##'
##' Content can be provided / modified through the global parameters
##' single.gene.example.popover ( text for popover help )
##' single.gene.example (gene identifiers)
##' @return Character string with html code
##' @author Thomas Sandmann
single_input_example <- function(){
  single.gene.example.popover <- getOption("single.gene.example.popover", default="A human Entrez identifier")
  gene.ids <- getOption("single.gene.example", default="81168")
  if( length( gene.ids) > 0){
    
    cat( sprintf("<a class=\"btn btn-mini pull-right\" id=\"show_example\" rel=\"popover\" data-content=\"%s\" data-placement=\"right\" data-trigger=\"hover\" onclick=\"$('[name=query_data]').val('%s')\">\n", single.gene.example.popover, gene.ids))
    cat( "<i class=\"icon-info-sign\"></i> Example query</a>" )
  }
}

##' Html code for generating and example query button on the signed gene set submission page
##'
##' Content can be provided / modified through the global parameters
##' signed.input.example.popover ( text for popover help )
##' signed.input.example.up (up-regulated gene identifiers)
##' signed.input.example.down (down-regulated gene identifiers)
##' @return Character string with html code
##' @author Thomas Sandmann

signed_input_example <- function(){
  signed.input.example.popover <- getOption("signed.input.example.popover", default="Example data with human Entrez identifiers")
  
  up.ids <- getOption("signed.input.example.up", default="28762, 81168, 391239, 100422641, 10533, 100421821, 100507058, 645722, 544430, 3117, 10755, 9290, 693209, 84138, 100132272, 260429, 100034248, 100653343, 83697, 100133251, 100506354, 11060, 644154, 6443, 28827, 84266, 678, 83955, 100419037, 60529, 219428, 407001, 255082, 9771, 100287227, 646839, 100189223, 5550, 100126506, 94299, 100271523, 643376, 100151641, 386730, 339822, 100189006, 1174, 199834, 100128521, 728115")  
  down.ids <- getOption("signed.input.example.down", default="200025, 100415942, 133789, 26301, 3572, 5122, 55739, 286297, 645811, 92340, 100422935, 54048, 151649, 57513, 643965, 28398, 200575, 100131795, 4281, 100419788, 59274, 100288831, 64641, 57579, 196513, 100421248, 100418596, 84570, 100133100, 100526840, 129666, 767561, 89853, 56922, 100130778, 10562, 730747, 6244, 26476, 7271, 25950, 2971, 642290, 100419816, 100133259, 100189306, 149041, 93974, 100271001, 439951")
  
  if( length( c(up.ids, down.ids)) > 0){
    cat( sprintf("<a class=\"btn btn-mini pull-right\" id=\"show_example\" rel=\"popover\" data-content=\"%s\" data-placement=\"right\" data-trigger=\"hover\" onclick=\"$('[name=query_data_up]').val('%s');\n", signed.input.example.popover, up.ids))
    cat( sprintf("$('[name=query_data_down]').val('%s')\">\n", down.ids))
    cat( "<i class=\"icon-info-sign\"></i> Example query</a>" )
  }
}
##' Html code for generating and example query button on the unsigned gene set submission page
##'
##' Content can be provided / modified through the global parameters
##' unsigned.input.example.popover ( text for popover help )
##' unsigned.input.example (gene identifiers)
##' @return Character string with html code
##' @author Thomas Sandmann
unsigned_input_example <- function(){
  unsigned.input.example.popover <- getOption("unsigned.input.example.popover", default="Example data with human Entrez identifiers")
  gene.ids <- getOption("unsigned.input.example", default="28762, 81168, 391239, 100422641, 10533, 100421821, 100507058, 645722, 544430, 3117, 10755, 9290, 693209, 84138, 100132272, 260429, 100034248, 100653343, 83697, 100133251, 100506354, 11060, 644154, 6443, 28827, 84266, 678, 83955, 100419037, 60529, 219428, 407001, 255082, 9771, 100287227, 646839, 100189223, 5550, 100126506, 94299, 100271523, 643376, 100151641, 386730, 339822, 100189006, 1174, 199834, 100128521, 728115, 200025, 100415942, 133789, 26301, 3572, 5122, 55739, 286297, 645811, 92340, 100422935, 54048, 151649, 57513, 643965, 28398, 200575, 100131795, 4281, 100419788, 59274, 100288831, 64641, 57579, 196513, 100421248, 100418596, 84570, 100133100, 100526840, 129666, 767561, 89853, 56922, 100130778, 10562, 730747, 6244, 26476, 7271, 25950, 2971, 642290, 100419816, 100133259, 100189306, 149041, 93974, 100271001, 439951")
  if( length( gene.ids) > 0){
    
    cat( sprintf("<a class=\"btn btn-mini pull-right\" id=\"show_example\" rel=\"popover\" data-content=\"%s\" data-placement=\"right\" data-trigger=\"hover\" onclick=\"$('[name=query_data]').val('%s')\">\n", unsigned.input.example.popover, gene.ids))
    cat( "<i class=\"icon-info-sign\"></i> Example query</a>" )
  }
}

##' Html code for generating an example query button on the profile submission page
##'
##' Content can be provided / modified through the global parameters
##' profile.input.example.popover ( text for popover help )
##' profile.input.example (gene identifiers and scores)
##' @return Character string with html code
##' @author Thomas Sandmann
profile_input_example <- function(){
  ## function to read the example data from file
  read_profile_example_file <- function(){
    genes.and.scores <- read.delim( system.file(file.path( "extdata", 
                                                           "profile_entrez_hs.txt"), 
                                                package="gCMAPWeb"),
                                    header=FALSE)
    genes.and.scores <- paste( genes.and.scores[,1], genes.and.scores[,2], sep=",")
    genes.and.scores <- paste( genes.and.scores, collapse="\\n")
    return( genes.and.scores)
  }
  
  profile.input.example.popover <- getOption("profile.input.example.popover", 
                                             default="Example data with human Entrez identifiers and z-scores")
  
  ## check if profile.input.example option is set, otherwise read example from file
  text <- getOption("profile.input.example", 
                    default=read_profile_example_file())
  
  if( length( text) > 0){
    cat( sprintf("<a class=\"btn btn-mini pull-right\" id=\"show_example\" rel=\"popover\" data-content=\"%s\" data-placement=\"right\" data-trigger=\"hover\" onclick=\"$('[name=query_data]').val('%s')\">\n", profile.input.example.popover, text))
    cat( "<i class=\"icon-info-sign\"></i> Example query</a>" )
  }
}

##' This function generates the figure legend, with a separate dom identifier for each output panel
##' @param reference.name character, name of the reference database used
##' @return character string with the html code element required to insert the figure legend into the html report
##' @author Thomas Sandmann
create_figure_legend <- function( reference.name ){
      cat(sprintf( "<span class='label label-info pull-left' data-toggle='collapse' data-target='#%s'>", paste(reference.name, "figure_legend", sep="_")),
            "Figure legend</span><br><br>",
            "<div class='span12'><br></div>",
            sprintf("<div id='%s' class='collapse span12'>", paste(reference.name, "figure_legend", sep="_")),
            sep="\n")
}

##' Content for the legend of the overview density plot displayed on every main results page
##'
##' @title Legend for overview density plot
##' @param text Character, text for the legend of the density plot the main report page. Can be set as the global variabel "gene.set.legend".
##' @return Character
##' @author Thomas Sandmann
create_overview_legend <- function( text=getOption( "gene.set.legend", default=sprintf("<strong>Gene-set scores</strong><br><strong>Left: </strong>Distribution of similarity scores between your query and all instances in the selected reference database. For reference, a standard normal distribution is indicated as a dotted line. The (up to) top %s scores for significantly correlated (green) or anti-correlated (blue) instances are indicated in the rug.<br><strong>Right: </strong>A heatmap of the rank-ordered similarity scores.", getOption( "max.results", default=50))) ){
  sprintf("<p align='justify'><small>%s</small><br></p>", text)
}

##' Content for the legend of the overview heatmap plot displayed on the main results page for directional and non-directional queries
##'
##' @title Legend for overview heatmap
##' @param text Character, text for the legend of the density plot the main report page. Can be set as the global variabel "gene.set.legend".
##' @return Character
##' @author Thomas Sandmann
create_heatmap_legend <- function( text=getOption( "heatmap.legend", default="<strong>Gene scores</strong><br><strong>Center: </strong>Heatmap of query gene scores (columns) in the reported reference datasets (rows), colored according to differential expression score. <strong>Left: </strong>Dendrogram from hierarchically clustering the reference datsets (rows) according to the query gene scores (only shown if 3 or more sets were reported). <strong>Top: </strong> query gene direction (black for up, grey for down-regulated query genes), only available for directional queries. <strong>Right: </strong>Direction of similarity (correlated, green or anti-correlated, blue). Click to enlarge.")){
  sprintf("<p align='justify'><small>%s</small><br></p>", text)
}

##' Html code for the legend of pie the pie chart of the gene-level report
##'
##' @param text Character, text for the legend of the density plot on gene-level reports for non-directional and directional queries. Can be set as the global variabel "gene.density.legend".
##' @return Character string with html code
##' @author Thomas Sandmann
gene_density_chart_legend <- function(text=getOption( "gene.density.legend", default="<strong>Top: </strong>The grey density shows the differential expression scores for all genes in this reference dataset, up-regulated genes have high scores, down-regulated genes have low scores and unchanged genes have scores around zero. If you submitted a signed query, the distribution of scores for your up-regulated genes are shown in green, and for those you labeled down-regulated in blue. Otherwise, a single distribution for all query genes is shown in black.<br><strong>Bottom: </strong>Each dash at the bottom of the plot corresponds to a single query gene, following the same color scheme as in the density plot above.")){
  sprintf("<p align='justify'><small>%s</small><br><br></p>", text) 
}

##' Html code for the legend of the pie chart of the gene-level report
##'
##' @param text Character, text for the legend of the pie chart on gene-level reports for non-directional queries. Can be set as the global variabel "gene.pie.legend".
##' @return Character string with html code
##' @author Thomas Sandmann
gene_pie_chart_legend <- function(  text=getOption( "gene.pie.legend", default="This pie chart shows how many of your query genes are also annotated in this reference dataset. The reported FDR and effect sizes were calculated using Fisher's exact test and reflect the likelihood of observing the overlap (grey) between your query and the reference gene sets, taking into account the size of the reference set and the total number of genes in the database.")){
  sprintf("<p align='justify'><small>%s</small><br><br></p>", text)   
}

##' Html code for the legend of the density charts of the gene-level reports from Profile queries
##'
##' @param text Character, text for the legend of the density plot on gene-level reports for profile queries. Can be set as the global variabel "gene.profile.legend".
##' @return Character string with html code
##' @author Thomas Sandmann
gene_density_profile_legend <- function( text=getOption( "gene.profile.legend", default="<strong>Top: </strong>The grey density displays the distribution of all <strong>differential expression scores in the query</strong> you submitted. The colored line(s) highlight where the genes annotated as differentially expressed in the reference experiment fall in your query distribution.<br>Scores for genes annotated as up-regulated in the reference experiment are shown in green, those annotated as down-regulated in the reference dataset are shown in blue.<br><strong>Bottom: </strong>Each dash at the bottom of the plot corresponds to a gene in the reference dataset, following the same color scheme as in the density plot above.")){
  sprintf("<p align='justify'><small>%s</small><br><br></p>", text)   
}

##' Shared html code, constituting the first part of the body section
##'
##' @title Definition of html navigation bar elements
##' @param url.base character, path to htdocs directory (optional)
##' @return Html code as a character string
##' @author Thomas Sandmann
body_first <- function(url.base=NULL){
  cat("<body>",
      "<div class=\"navbar navbar-inverse navbar-fixed-top\">",
      "<div class=\"navbar-inner\">",
      "<div class=\"container\">",
      "<a class=\"btn btn-navbar\" data-toggle=\"collapse\" data-target=\".nav-collapse\">",
      "<span class=\"icon-bar\"></span>",
      "<span class=\"icon-bar\"></span>",
      "<span class=\"icon-bar\"></span>",
      "</a>",
      "<!-- customized site title -->",
      sprintf( "<a class='brand' href='%s'>", getOption( "home.url", default="")),
      getOption( "site.title", default="gCMAP"),
      "<i class=\"icon-home icon-white\"></i></a>",
      "<div class=\"nav-collapse collapse pull-right\">",
      "<ul class=\"nav\">",
      sep="\n"
  )
  
  feedback.url <- getOption( "feedback.url", default=NULL)
  if( !is.null (feedback.url)){
    cat( "<li>" )
    cat( sprintf( "<a href='%s'>", feedback.url ) )
    cat( "<i class='icon-forward icon-white'></i>", "Feedback", "</a>","</li>", sep="\n")
  }
  
  if( identical( "help.html", getOption( "doc.url", default="help.html"))){
    if( is.null( url.base )){
      doc.url <-"help.html"
    } else {
      doc.url <- file.path( url.base, "help.html")
    }
  } else {
    doc.url <-getOption( "doc.url")
  }
  
  if( !is.null (doc.url)){
    cat( "<li>" )
    cat( sprintf( "<a href='%s'>", doc.url ) )
    cat( "<i class='icon-forward icon-white'></i>", "Help", "</a>","</li>", sep="\n")
  }
  contact.email <- getOption( "contact.email", default=NULL)
  if( !is.null (contact.email)){
    cat( "<li>" )
    cat( sprintf("<a href='%s'>", contact.email ))
    cat( "<i class='icon-envelope icon-white'></i>", "Contact", "</a>","</li>", sep="\n")
  }
  name.out <- getOption( "name.out", default=NULL)
  link.out <- getOption( "link.out", default=NULL)
  if( !is.null( name.out ) & ! is.null( link.out )){
    cat( "<li>" )
    cat( sprintf( "<a href='%s'>", link.out ) )
    cat( "<i class='icon-arrow-right icon-white'></i>", name.out, "</a>","</li>", sep="\n")
  }
  cat("</ul>",
      "</div><!--/.nav-collapse -->",
      "</div>",
      "</div>",
      "</div>",
      "<noscript>",
      "<style type=\"text/css\">",
      ".container{display:none;}",
      "</style>",
      "<div class=\"hero-unit\">",
      "<p class=\"text-warning\">",
      "Please enable javascript in your browser to use gCMAP",
      "</p>",
      "</div>",
      "</noscript>",
      "<div class=\"container\">",
      sep="\n"
  )
}


##' Html code for title of the hero unit of the index.rhtml page
##'
##' @param text Character, main title of the hero unit on the index.rhtml page. Can be set as the global variabel "index.main".
##' @return Character string with html code
##' @author Thomas Sandmann
index_title <- function(text=getOption( "index.main", default="gConnectivity Map")){
  cat(sprintf("<h1>%s</h1>", text))
}

##' Html code for subtitle of the hero unit of the index.rhtml page
##'
##' @param text Character, subtitle of the hero unit on the index.rhtml page. Can be set as the global variabel "index.sub".
##' @return Character string with html code
##' @author Thomas Sandmann
index_subtitle <- function(text=getOption( "index.sub", default="a search engine for differential gene expression profiles.")){
  cat(sprintf("<h3>%s</h3>", text))
}

##' Html code for text of the hero unit of the index.rhtml page
##'
##' @param text Character, text of the hero unit on the index.rhtml page. Can be set as the global variabel "index.text".
##' @return Character string with html code
##' @author Thomas Sandmann
index_text <- function(text=getOption( "index.text", default="Compare your favorite genes to a reference database of differential expression experiments")){
  if(! is.null( text)){
    cat(sprintf("<p>%s</p>", text))
  }
}

##' Html code for text of the additional message on the index page (warning box)
##'
##' @param text Character, text for the additional message on the index page (warning box)
##' @return text Character string with html code
##' @author Thomas Sandmann
index_message <- function(text=getOption( "index.message", default=NULL)){
  if(! is.null( text)){
    cat( sprintf("<div class='alert alert-info span8'>%s</div>", text))
  }
}

##' Html code for quote on the index page
##'
##' @return Character string with html code
##' @param text, Character, the content of the blockquote field on the index.rhtml page. Can be set as the global variabel "index.quote".
##' @author Thomas Sandmann
index_quote <- function(text=getOption( "index.quote", default="<p>What if at least some parts of [ the laborious screening of genetic or chemical libraries ] could be systematized and centralized? <br>And hits found and hypotheses generated with something resembling an internet search engine? <br>These are the questions the Connectivity Map project set out to answer.</p><small>Justin Lamb in <cite title='Source Title'>Nature Reviews Cancer 7, 54-60 (January 2007)</cite></small></blockquote>")){
  
  if( !is.na( text )){
    cat( sprintf( "<div class='span12'><blockquote>%s</blockquote></div>", text))
  }
}

##' This function reads an eSet object and returns the html code to display a legend for all columns that have more descriptive annotations in the varMetadata slot.
##'
##' @param col.names character, a vector with column names that are always included in the legend
##' @param eset eSet object
##' @return character string with the html code element required to insert the legend into the html report
##' @author Thomas Sandmann
create_gene_table_legend <- function( col.names, eset ){
  legend.text <- c( 
    exprs = "Mean expression across all samples. For microarray data, the mean normalized fluorescence intensity is reported on a log2 scale. For RNAseq experiment, the mean number of counts, normalized to a robust estimate of the library size, is reported.",
    p = "Raw p-value",
    z = "z-score, calculated based on a standard normal distribution. Down-regulated genes have a negative, up-regulated genes a positive sign.",
    log_fc = "Log2 fold change, obtained by dividing the mean intensities (for microarrays) or counts (for RNAseq data) of treatment samples by those of the controls.",
    mod_fc = "Moderated log2 fold change, obtained by applying a variance-stabilizing transformation before calculating the fold change between treatment and control samples. This reduces the variability associated with very small counts."
    )
  
  ## remove rows without additional annotation
  informative <- col.names[ col.names %in% names( legend.text )]
  if( length( informative ) > 0){
    
    ## generate html code
    html.code <- paste("<dl class='dl-horizontal'>",
                       paste(
                         sapply( 1:length( informative ), function( n ){
                           sprintf("<dt>%s</dt><dd>%s</dd>", 
                                   informative[n], 
                                   legend.text[informative[n]] )
                         }), collapse=" " ),
                       "</dl>", collapse=" ")
    
    sprintf("<span class='label label-info pull-left' data-toggle='collapse' data-target='#%s'>Table legend</span><div id='%s' class='collapse span8'>%s</div><br>", "legend", "legend", html.code )
    
  } else {
    NULL
  }
}
