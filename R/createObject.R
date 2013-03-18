##' This function extracts the content from a POST request and validates its content
##'
##' @title Html request validation
##' @param req Request object as defined by the Rook package
##' @param conf_data list, the configuration data as returned by the read_config_file function
##' @param cmaps.concatenated.by character, specifying the separator (e.g. semicolon, comma) used to concatenate multiple requested reference databases
##' @return Logical, TRUE if all required fields could be validated, FALSE otherwise
##' @author Thomas Sandmann
validate_request <- function( req, conf_data, cmaps.concatenated.by=getOption( "cmaps.concatenated.by", default=",") ){
  ## extract payload
  request <- req$params()
  
  ## verify basic propertis of the request
  stopifnot( req$form_data() == TRUE)
  stopifnot( length( request) > 0)
  
  ## verify that content has been pasted or a file uploaded
  if( ! any(grepl( "^query_", names( request )))){
    stop( "No query data was pasted or uploaded." )
  }
  
  ## verify that at least one reference dataset has been picked
  if( ! "selected_cmaps" %in% names( request )){
    stop( "No reference dataset has been selected." )
  }
  
  ## verify that other required fields are present
  required.fields <- c( "inputType", "species", "idType")
  required.files.found <- required.fields %in% names( request )
  if( ! all( required.files.found )){
    stop( sprintf("Required field(s) %s not found in html request", 
                  paste(required.fields[!required.files.found], collapse=",")))
  }
  
  ## verify datatype is supported by config file
  if( ! request$inputType %in% getOption( "supported.inputType", 
                                          default=c("single", "non-directional", 
                                                    "directional", "profile"))  
      ){
    stop(sprintf("Query type %s is not supported.", request$inputType))
  }
  ## verify species is supported by config file
  if( ! request$species %in% names( conf_data$species )){
    stop(sprintf("Species %s is not supported.", request$species))
  }
  ## verify gene id type is supported by config file
  if( ! request$idType %in% getOption( "supported.idType", default=c("symbol", "entrez", "probe"))){
    stop(sprintf("Gene identifier type %s is not supported.", request$geneid))
  }
  ## verify supported dastaset has been requested
  supported_datasets <- unlist(sapply( conf_data$species, function( x ) { names(x[["cmaps"]])}) )
  request$selected_cmaps <- strsplit(URLdecode(request$selected_cmaps), cmaps.concatenated.by)[[1]]
  
  if(! all( request$selected_cmaps %in% supported_datasets )){
    not.found <- request$selected_cmaps[ ! request$selected_cmaps %in% supported_datasets]
    stop(sprintf("Requested reference dataset(s): %s not available", paste(not.found, collapse=", ")))
  }
  
  ## verify array platform is supported by config file
  if( request$idType == "probe"){
    if( ! "platform" %in% names( request )){
      stop("Probe identifiers submitted without platform specification.")
    }
    requested_platform <- request$platform
    supported_platforms <- unlist(sapply( conf_data$species, function( x ) { x[["platforms"]] }) )
    if( ! requested_platform %in% supported_platforms ){
      stop("Requested array platform is not supported.")
    }
  }
  return(TRUE)
}
##' This function parses the a POST request and decodes the user-provided information 
##'
##' @title POST request parser
##' @param request Request object as defined by the Rook package
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @param cmaps.concatenated.by character, specifying the separator (e.g. semicolon, comma) used to concatenate multiple requested reference databases
##' @return list, all elements from POST request, decoded if necessary
##' @author Thomas Sandmann
parse_request <- function( request, conf_data, cmaps.concatenated.by=getOption( "cmaps.concatenated.by", default=",")){
  post <- request$params()
  ## reference dataset names were concatenated for form submission,
  ## and are now split into separate strings again
  post$selected_cmaps <- strsplit(URLdecode(post$selected_cmaps),
                                  cmaps.concatenated.by, fixed=TRUE)[[1]]
  
  ## Have files been uploaded (identified by prefix "query_file") ?
  file.input <- grep("^query_file", names(post), value=TRUE )
  if( length( file.input) > 0){
    
    ## parse and deposit content into 'query_data' elements of 'post'
    for( query.file in file.input){
      post[[ sub("file", "data", query.file) ]] <- parse_file_input( post[[ query.file ]] )
    }
  } else{
    ## decode textarea fields (identified by prefix "query_data")
    textarea.input <- grep("^query_data", names(post), value=TRUE )
    for( query.data in textarea.input){
      post[[ query.data ]] <- parse_textarea_input( post[[ query.data ]] )
    }
  }
  if( getOption( "save.intermediates", default = FALSE) == TRUE){
    save(post, file=file.path(tempdir(),"parsed_post.rdata"))
  }

  ## process input according to inputType
  parsed.data <- grep("^query_data", names(post), value=TRUE )
  for( dat in parsed.data){
    
    ## simple id input
    if( post$inputType %in% c("single", "non-directional", "directional")){
      post[[ dat ]] <- as.vector( unlist( post[[dat]] )) ## character vector
    } else {
      ## input with scores
      post[[ dat ]] <- process_score_input( post[[dat]] ) ## data.frame
    }
  }
  return( post )
}

##' Function to trim leading / tailing characters
##'
##' @title Function to trim leading / tailing whitespace characters
##' @param x character
##' @return character
##' @author Thomas Sandmann
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

##' This functions parses the user-uploaded files
##'
##' @title Uploaded file parser
##' @param temp.file character, path to temporary file (usually part of the POST request)
##' @param regexp character, a regular expression matching all separators used to separate gene identifiers
##' @param n.score.col integer, for profile uploads n.score.col specifies the number of data columns (usually 1)
##' @return list of character vectors, one element for each row of temp.file
##' @author Thomas Sandmann
parse_file_input <- function( temp.file, regexp="[+,;\t ]+", n.score.col=1 ){
  ## make sure the file exists
  stopifnot( file.exists( temp.file[["tempfile"]] ))
  
  ## read input file line by line
  file.content <- try( readLines( temp.file[["tempfile"]]))
  if( inherits( content, "try-error")){
    stop(sprintf("Uploaded file %s could not be read.", temp.file[["filename"]]))
  } 
  file.content <- sapply( file.content, strsplit, regexp)
  file.content <- file.content[ ! sapply( file.content, function(x){ identical( x, character(0))})]
  return( file.content ) ## list of character vectors, one per row
}

##'  This function parses user-specified data pasted into the textarea fields of the input form
##'
##' @title Textarea parser
##' @param textarea.input character, the user-input extracted from the POST request 
##' @param regexp character, a regular expression matching all separators used to separate gene identifiers
##' @return list of character vectors, one element for each original row of textarea.input
##' @author Thomas Sandmann
parse_textarea_input <- function( textarea.input, regexp="[+,;\t ]+" ){
  post.decoded <- trim( URLdecode( textarea.input ) )
  post.vector <- strsplit(post.decoded, "\r\n", fixed=TRUE)
  input.rows <- lapply(post.vector, strsplit, regexp)[[1]]
  return(input.rows) ## list of character vectors, one per row
}

##' This function reformats score input uploaded or pasted by the user
##'
##' @title Score parser
##' @param query_data list of row-vectors, as generated by parse_file_input function
##' @param n.score.col integer, specifies the number of data columns (usually 1)
##' @return matrix of expression scores
##' @author Thomas Sandmann
process_score_input <- function( query_data, n.score.col=1 ){ ## ids & scores submitted
  ## do all rows have the righ number of columns ?
  if( !all ( sapply( query_data, length) == ( n.score.col + 1))){
    stop(sprintf( "Each input row must contain one gene identifier and %s score.", n.score.col), call. =FALSE)
  }
  ## separate ids from scores
  ids <- as.character(sapply(query_data, "[[", 1 ))
  scores <- sapply( query_data, function( x ){
    x[2:( n.score.col +1 )]
  })
  try( scores <- as.numeric( scores ))
  if( inherits( scores, "try-error")){
    stop("All scores must be numeric or NA.", call. = FALSE)
  }
  ## Create a score data.frame
  try( scores <- data.frame( Id=ids, Profile=scores ))
  if( inherits( scores, "try-error")){
    stop("Could not generate data.frame from submitted profile query.", call. =FALSE)
  }
  return( scores )
}

##' This function generates an appropriate R data object from the user query
##'
##' @title Query object creator
##' @param post list, POST component of the Rook request
##' @param conf_data list, the configuration data as returned by the read_config_file function
##' @return one of GeneSet, SignedGeneSet or ExpressionSet
##' @author Thomas Sandmann
create_query_objects <- function( post, conf_data ){
  if( post$inputType %in% c("single", "non-directional") ){
    obj <- create_GeneSet( post, conf_data)
 } else if( post$inputType == "directional"){
   obj <- create_SignedGeneSet( post, conf_data)
    
  } else if( post$inputType == "profile"){
    obj <- create_profile_ExpressionSet( post, conf_data )      
  }  
  query <- obj$gs
  conversion <- obj$conversion
  
  ## for single-gene queries pass the Entrez id on as character
  if( post$inputType == "single"){
    query <- geneIds( query)
    if( length( query ) > 1){
      stop("Sorry, it looks like you entered more than a single gene identifier.")
    }
  }
  
  return( list( query=query, conversion=conversion) )
}

##'This function extracts the species or platform information from the POST request and constructs GeneIdentifierType objects
##'
##' @title GeneIdentifierType creator
##' @param post  list, POST component of the Rook request
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @return GeneIdentifierType object
##' @author Thomas Sandmann
create_GeneIdentifierType <- function( post, conf_data ){
  ## extract species or platform annotation package
  ## from post request and construct GeneIdentifierType object
  
  species <- conf_data$species[[post$species]][["annotation"]]
  if( post$idType == "symbol"){
    query.IdType <- SymbolIdentifier(species)
  } else if(post$idType == "entrez" ){
    query.IdType <- EntrezIdentifier(species)
  } else if(post$idType == "probe"){
    query.IdType <- AnnotationIdentifier(post$platform)
  }
  return( query.IdType )
}

##' This function maps the gene identifiers of a GeneSet object to Entrez identifiers
##' and returns an error if none of them could be found.
##' 
##' gCMAPWeb uses this function to ensure that all submitted / retrieved Entrez Ids 
##' are valid.
##'
##' @title GeneSet Entrez mapper
##' @param gs GeneSet or SignedGeneSet
##' @return GeneSet or SignedGeneSet
##' @author Thomas Sandmann
entrez_GeneSets <- function( gs ){
  entrez.gs <- mapIdentifiers(gs, from=geneIdType( gs), to=EntrezIdentifier())
  n.valid.entrez <- length(geneIds(entrez.gs))
  if( n.valid.entrez == 0 ){
    stop("None of the submitted gene identifiers are / could be resolved to Entrez ids.", call. = FALSE)
  } else {
    message(sprintf("Query contains %s unique Entrez identifier(s).", n.valid.entrez))
  }
  return( entrez.gs )
}

##' This function strips a prefix from gene identifier strings
##'
##' @title Strip prefix from gene identifier names
##' @param ids character, vector of gene identifiers
##' @param prefix character, one or more strings to be removed
##' @return character, gene identifiers without prefix
##' @author Thomas Sandmann
stripPrefix <- function( ids, prefix=c("^GeneID:", "^GeneID")){
  id.names <- names( ids )
  for( pattern in prefix){
    ids <- sapply( ids, function( x ){
      sub( pattern, "", x)
    })
  }
  names( ids ) <- id.names
  return( ids )
}

##' This function creates a GeneSet object from user-specified identifiers
##'
##' @title GeneSet creator
##' @param post list, POST component of the Rook request
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @return GeneSet object
##' @author Thomas Sandmann
create_GeneSet <- function(post, conf_data) {
  
  ## generate GeneIdentifierType object
  query.IdType <- create_GeneIdentifierType( post, conf_data=conf_data )
  gene.ids <- unique( post$query_data )
  gene.ids <- stripPrefix( gene.ids )
  if( post$inputType == "non-directional" && length( gene.ids ) < 2 ){
    stop( "Please specify at least two valid gene identifiers for a non-directional query.", call. = FALSE)
  }
  
  ## convert gene symbols to Entrez Ids and return 
  ## translation table
  species <- conf_data$species[[post$species]][["annotation"]]
  conv <- convert_gene_identifiers( query.IdType=query.IdType,
                                    gene.ids=gene.ids,
                                    species=species)
  query.IdType <- conv[["query.IdType"]]
  gene.ids <- conv[["gene.ids"]]
  conversion <- conv[["conversion"]]
  
  ## create GeneSet object
  user.data <- GeneSet(gene.ids,
                       geneIdType=query.IdType,
                       setName="your.genes")
  
  ## check / convert to Entrez identifiers
  gs.entrez <- entrez_GeneSets( user.data )
  return( list( gs=gs.entrez, conversion=conversion ))
}

##' This function creates a SignedGeneSet object from user-specified identifiers.
##'
##' @title SignedGeneSet creator
##' @param post list, POST component of the Rook request
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @return SignedGeneSet object
##' @author Thomas Sandmann
create_SignedGeneSet <- function(post, conf_data){
  ## generate GeneIdentifierType object
  query.IdType <- create_GeneIdentifierType( post, conf_data=conf_data )
  
  ## were up-regulated gene ids submitted ?
  if( "query_data_up" %in% names( post )){
    genes.up <- unique( post$query_data_up )
  } else {
    genes.up <- NULL
  }
  
  ## were down-regulated gene ids submitted ?
  if( "query_data_down" %in% names( post )){ 
    genes.down <- unique( post$query_data_down )
  } else {
    genes.down <- NULL
  }
  
  ## remove genes present in both categories
  if( length( genes.up ) > 0 & length( genes.down) >0 ){
    up.and.down <- intersect(genes.up,genes.down)
    if( length( up.and.down) > 0){
      genes.up <- genes.up[-match( up.and.down, genes.up)]
      genes.down <- genes.down[-match( up.and.down, genes.down)]
    }
  }
  
  ## create 'sign' vector
  gene.ids <- c(genes.up, genes.down)
  gene.ids <- stripPrefix( gene.ids )
  signs <- c(rep("up", length(genes.up)),
             rep("down", length(genes.down)))
  names(signs) <- gene.ids

  ## convert gene symbols to Entrez Ids and return 
  ## translation table
  species <- conf_data$species[[post$species]][["annotation"]]
  conv <- convert_gene_identifiers( query.IdType=query.IdType,
                                    gene.ids=gene.ids,
                                    species=species)
  query.IdType <- conv[["query.IdType"]]
  gene.ids <- conv[["gene.ids"]]
  conversion <- conv[["conversion"]]
  signs <- signs[ conv[["original.ids"]] ]
  
  ## create SignedGeneSet object
  user.data <- SignedGeneSet(gene.ids,
                             setName="your.genes",
                             geneSign=signs,
                             geneIdType=query.IdType)
  
  ## check / convert to Entrez identifiers
  gs.entrez <- entrez_GeneSets( user.data )
  return( list( gs=gs.entrez, conversion=conversion))
}

##' This function maps the submitted gene identifiers to Entrez identifiers
##' and returns a translation table with the original and mapped ids
##'
##' @title Entrez mapper
##' @param gene.ids Character vector with gene ids
##' @param query.IdType A GeneIdentifierType object
##' @param species String identifying the species, will be inserted into a new EntrezIdentifier object and returned with the translated results
##' @return A list with the following elements
##'   \itemize{
##'     \item gene.ids, a character vector of unique EntrezIds
##'     \item query.IdType, an EntrezIdentifier object with the species
##'     \item conversion, a list containing the full translation table and a summary message
##'     \item original.ids, a character vector of the submitted ids corresponding to the returned EntrezIds
##'   }
##' @author Thomas Sandmann
convert_gene_identifiers <- function(gene.ids, query.IdType, species){
  ## convert gene symbols to Entrez Ids and return 
  ## translation table
  if( query.IdType@type == "Symbol"){
    conversion <- .alias2entrez( gene.ids, query.IdType@annotation )
    translation.table <- conversion$translation.table
    translation.table <- translation.table[ which( !duplicated(translation.table[, "EntrezId"])),]
    gene.ids <- unique( na.omit( as.character( translation.table[, "EntrezId"] ) ) )
    query.IdType <- EntrezIdentifier( species )
    original.ids <- as.character( translation.table[which(!is.na(translation.table$EntrezId )), "Symbol"] )
      
  } else if( query.IdType@type == "Annotation"){
    conversion <- .probe2entrez( gene.ids, query.IdType@annotation )
    translation.table <- conversion$translation.table
    translation.table <- translation.table[ which( !duplicated(translation.table[, "EntrezId"])),]
    gene.ids <- unique( na.omit( as.character( translation.table[, "EntrezId"] ) ) )
    query.IdType <- EntrezIdentifier( species )
    original.ids <- as.character( translation.table[which(!is.na(translation.table$EntrezId )), "Probe"])
  
  } else {
    conversion <- .entrez2symbol( gene.ids, query.IdType@annotation )
    original.ids <- gene.ids
  }
  return( list( gene.ids=gene.ids, 
                query.IdType=query.IdType, 
                conversion=conversion,
                original.ids=original.ids)
          )
}

## 'This function creates an ExpressionSet object from user-specified identifiers and scores.
##'
##' @title ExpressionSet creator
##' @param post  list, POST component of the Rook request
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @return ExpressionSet object
##' @author Thomas Sandmann
create_profile_ExpressionSet <- function( post, conf_data ){
  
  post$query_data[,"Id"] <- stripPrefix( post$query_data[,"Id"] )
  
  if( post$idType =="entrez"){
    colnames( post$query_data ) <- c("EntrezId", "Profile")
    conversion <- list( translation.table=data.frame(EntrezId=post$query_data$EntrezId),
                        feedback=sprintf( "Your query contained %s unique Entrez identiers.", length(unique( post$query_data$EntrezId ))))
  } else if( post$idType == "probe") {
    conversion <- .probe2entrez( as.character(post$query_data[,"Id"]), post$platform)
    entrez.scores <- merge( conversion$translation.table, post$query_data, by.x="Probe", by.y="Id" )
    entrez.scores <- entrez.scores[!is.na( entrez.scores$EntrezId),]
    post$query_data <- data.frame( EntrezId=entrez.scores$EntrezId, Profile=entrez.scores$Profile)
    
  } else if( post$idType == "symbol"){
    species <- conf_data$species[[post$species]][["annotation"]]
    conversion <- .alias2entrez( as.character( post$query_data[,"Id"]), species)
    entrez.scores <- merge( conversion$translation.table, post$query_data, by.x="Symbol", by.y="Id" )
    entrez.scores <- entrez.scores[!is.na( entrez.scores$EntrezId),]
    post$query_data <- data.frame( EntrezId=entrez.scores$EntrezId, Profile=entrez.scores$Profile)
  }
  
  ## create score matrix, average scores for genes submitted with multiple scores
  user.data <-tapply( post$query_data$Profile, 
                      post$query_data$EntrezId, 
                      mean, na.rm = TRUE)
  user.data <- matrix(user.data, 
                      ncol=1, 
                      dimnames=list(names( user.data), "Profile"))
  
  ## create ExpressionSet
  user.data <- ExpressionSet(user.data)
  
  ## add annotation to ExpressionSet  
  Biobase::annotation(user.data) <- "EntrezId"
  return( list( gs=user.data, conversion=conversion ))
}

##' This function leverages Bioconductor annotation packages to translate gene alias identifiers into Entrez identifiers
##'
##' @title Alias to Entrez identifier converter
##' @param gene.ids character, gene identifiers to convert
##' @param universe character, the name of an annotation package (without .db)
##' @return a list with two elements: a data.frame with the original and translated identifiers and a character string summarizing the conversion process.
##' @author Thomas Sandmann
.alias2entrez <- function( gene.ids, universe) {
  ## load annotation package
  DB <- paste(universe,"db",sep=".")
  suppressPackageStartupMessages(require(DB,character.only=TRUE))
  
  ## construct environment name
  s2e <- get( paste( universe, "ALIAS2EG", sep=""))
  
  ## query environment for Entrez Ids
  entrez.ids <- AnnotationDbi::mget(gene.ids, s2e, ifnotfound=NA)
  entrez.ids <- sapply( entrez.ids, "[[", 1) ## for multi-matches, use first Entrez Id
  
  ## Give feedback
  if( !is.null( entrez.ids ) && length( entrez.ids ) > 0 ) {
    feedback <- sprintf("Successfully mapped %s query symbols to %s unique Entrez identifiers.", 
                length( entrez.ids[!is.na( entrez.ids)] ),
                length( unique( entrez.ids[!is.na( entrez.ids)] )))
  } else {
    stop("None of the query symbols could not be mapped to Entrez identifiers.", call. = FALSE)
  }
  
  return( list( 
    translation.table=data.frame( Symbol=gene.ids, EntrezId=entrez.ids ),
    feedback=feedback))
}

##' This function leverages Bioconductor annotation packages to translate microarray probe identifiers into Entrez identifiers
##'
##' @title Probe to Entrez identifier converter
##' @param gene.ids character, gene identifiers to convert
##' @param universe character, the name of an annotation package (without .db)
##' @return a list with two elements: a data.frame with the original and translated identifiers and a character string summarizing the conversion process.
##' @author Thomas Sandmann
.probe2entrez <- function( gene.ids, universe) {
  ## load annotation package
  DB <- paste(universe,"db",sep=".")
  suppressPackageStartupMessages(require(DB,character.only=TRUE))
  
  ## construct environment name
  s2e <- get( paste( universe, "ENTREZID", sep=""))
  
  ## query environment for Entrez Ids
  entrez.ids <- AnnotationDbi::mget(gene.ids, s2e, ifnotfound=NA)
  entrez.ids <- sapply( entrez.ids, "[[", 1) ## for multi-matches, use first Entrez Id
  
  ## Give feedback
  if( !is.null( entrez.ids ) && length( entrez.ids ) > 0 ) {
    feedback <- sprintf("Successfully mapped %s query probes to %s unique Entrez identifiers.", 
                length( entrez.ids[!is.na( entrez.ids)] ),
                length( unique( entrez.ids[!is.na( entrez.ids)] )))
  } else {
    feedback <- "None of the query EntreIds is associated with gene symbols."
  }
  
  return( list( 
    translation.table=data.frame( Probe=gene.ids, EntrezId=entrez.ids ),
    feedback=feedback))
}
##' This function leverages Bioconductor annotation packages to translate microarray entrez identifiers into gene symbols
##'
##' @title Entrez identifier to symbol converter
##' @param gene.ids character, gene identifiers to convert
##' @param universe character, the name of an annotation package (without .db)
##' @return a list with two elements: a data.frame with the original and translated identifiers and a character string summarizing the conversion process.
##' @author Thomas Sandmann
.entrez2symbol <- function( gene.ids, universe) {
  ## load annotation package
  DB <- paste(sub(".db$", "", universe), "db",sep=".")
  suppressPackageStartupMessages(require(DB,character.only=TRUE))
  
  ## construct environment name
  s2e <- get( paste( universe, "SYMBOL", sep=""))
  
  ## query environment for Entrez Ids
  entrez.ids <- AnnotationDbi::mget(gene.ids, s2e, ifnotfound=NA)
  entrez.ids <- sapply( entrez.ids, "[[", 1) ## for multi-matches, use first Entrez Id
  
  ## Give feedback
  if( !is.null( entrez.ids ) && length( entrez.ids ) > 0 ) {
    feedback <- 
        sprintf("The submitted query Entrez identifiers are associated with %s unique gene symbols.", 
                length( unique( entrez.ids[!is.na( entrez.ids)] )))
  } else {
    feedback("None of the query Entrez identifiers are associated with gene symbols.")
  }
  
  return( list( 
    translation.table=data.frame( EntrezId=gene.ids, Symbol=entrez.ids ),
    feedback=feedback))
}
