##' This function controls the gCMAPWeb analysis workflow
##' 
##' @title Function controlling the gCMAP analysis workflow
##' @param req a Rook Request object
##' @param conf_data list, the configuration data as returned by the read_config_file function
##' @param reference.cmaps list, containing the reference database eSet objects
##' @param element character, identifying the elementName of the channel extracted from NChannelSet objects
##' @param save.intermediates logical, if TRUE rdata files of intermediate results will be stored in the temporary directory for debugging
##' @return a list with three elements: conversion, reports and tmp_filename
##' @importFrom parallel mclapply
##' @author Thomas Sandmann
cmapAnalysis <- function(
  req,
  conf_data,
  reference.cmaps,
  element=getOption( "element",
    default="z"),
  save.intermediates=getOption( "save.intermediates",
    default=FALSE)){
  
  ## validate & parse post request
  if( getOption( "save.intermediates", default = FALSE) == TRUE){
        save(req, file=file.path(tempdir(),"req.rdata"))
  }
  
  if( validate_request( req, conf_data) == FALSE ){
    stop("Received an invalid request from the web browser: 
         please make sure post request is compatible with the specified configuration file.")
  }
  
  ## parse & decode user-submitted data
  post <- parse_request( req, conf_data)
  if( getOption( "save.intermediates", default = FALSE) == TRUE){
    save(post, file=file.path(tempdir(),"post.rdata"))
  }
  
  ## construct query objects from post data
  obj <- create_query_objects( post, conf_data )
  query <- obj$query
  conversion <- obj$conversion
  
  if( getOption( "save.intermediates", default = FALSE) == TRUE){
    save(obj, file=file.path(tempdir(),"query.rdata"))
  }

  ##------- separately query all selected reference datsets
  selected.cmaps <- reference.cmaps[ post$selected_cmaps ]
  results <- mclapply( selected.cmaps, function( ref.data){
    try( cmapRun( query, ref.data ))
  })
 
  ##--------- reporting
  
  ## create output directory for this query
  tmp_filename <- basename( tempfile ())  
  dir.create(
    file.path(
      tempdir(),
      "results",
      tmp_filename
      ),
    showWarnings=FALSE,
    recursive=TRUE)
  ## copy js, css, etc
  file.copy(
    system.file(
      file.path("htdocs","css"),
      package="gCMAPWeb"),
    file.path(
      tempdir(),
      "results",
      tmp_filename
      ),
    recursive=TRUE)
  file.copy(
    system.file(
      file.path("htdocs","js"),
      package="gCMAPWeb"),
    file.path(
      tempdir(),
      "results",
      tmp_filename),
    recursive=TRUE)
  file.copy(
    system.file(
      file.path("htdocs","img"),
      package="gCMAPWeb"),
    file.path(
      tempdir(),
      "results",
      tmp_filename),
    recursive=TRUE)
  file.copy(
    system.file(
      file.path("htdocs","favicon.ico"),
      package="gCMAPWeb"),
    file.path(
      tempdir(),
      "results",
      tmp_filename))
  file.copy(
    system.file(
      file.path("htdocs","help.html"),
      package="gCMAPWeb"),
    file.path(
      tempdir(),
      "results",
      tmp_filename),
    recursive=TRUE)
  
  ## save results as rdata object
  session.info <- sessionInfo()
  save(
    results,
    session.info,
    file=file.path(
      tempdir(),
      "results",
      tmp_filename,
      "results.rdata"
      )
    )
  
  ## for symbol or probe queries, return identifier conversion table
  if( !is.null( conversion)){
    conversion$html<- conversion_html(
      df=conversion$translation.table, 
      message=conversion$feedback,
      file.name="identifier_conversion",
      tmp_filename=tmp_filename,
      result.dir = file.path( tempdir(), "results", tmp_filename)
      )
  } else {
    conversion <- NULL
  }
  
  reports <- lapply( seq_along( results ), function( n ){
    
    ## conserve error messages for failed analyses
    if( inherits( results[[n]], "try-error")){
      return( list(
        header=NULL,
        error=sprintf("<div class='alert alert-error'>%s</div>", 
                results[[n]][[1]]),
        overview=NULL,
        table=NULL,
        download=NULL
        ))
        
    } else {
      ## create reports for successful analyses
      annotation.db=paste(
        conf_data[["species"]][[post$species]][["annotation"]],
        "db",
        sep=".")
      report <- generate_report(
        results[[n]], query, tmp_filename=tmp_filename,
        reference.name=names(results)[n],
        reference=reference.cmaps[[names(results)[n]]],
        element=element,
        annotation.db=annotation.db)
    }
  })
  names( reports) <- names( results )
  
  ## save reports for debugging purposes
  if( getOption( "save.intermediates", default = FALSE) == TRUE){ 
    save(reports,
         conversion,
         file=file.path(
           tempdir(),
           "reports.rdata"
           )
         )
  }
  
  return(
    list(
      conversion=conversion,
      reports=reports,
      tmp_filename=tmp_filename
      )
    )
}
