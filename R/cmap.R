.e <- new.env()

##' This function starts a gCMAPWeb instance on the local machine using R's internal web server.
##'
##' @title Start local gCMAPWeb instance
##' @param config.file.path character, path to a gCMAPWeb configuration file in yaml format
##' @param url.root character, path to the htdocs folder
##' @return an Rhttpd class object
##' @export
##' @author Thomas Sandmann
##' @examples
##' if (interactive()) {
##'   ## start a gCMAPWeb instance with the example data and
##'   ## configuration provided in the package
##'   gCMAPWeb()
##'   ## same as above, explicitely specifying the location
##'   ## of the configuration file
##'   gCMAPWeb( 
##'     config.file.path = system.file("config", "config.yml", 
##'                                     package = "gCMAPWeb" )
##'   )
##'}
gCMAPWeb <- function(
  config.file.path=system.file("config", "config.yml", package="gCMAPWeb"), 
  url.root=system.file("htdocs", package="gCMAPWeb")){
  
  ##----- read & validate config file, load specified reference datasets
  conf_data <- validate_config_file( config.file.path )
  reference.cmaps <- load_cmaps( conf_data )
  
  ## create per-session temp dir, if necessary
  dir.create(
    file.path(tempdir(),'results'),
    showWarnings=FALSE
    )
  
  ## create gCMAP app
  gcmap <- Builder$new(
    Static$new(
      urls = c('/css','/img','/js'),
      root = url.root
    ),
    Static$new(urls='/results',
               root=tempdir()
               ),
    
    Brewery$new(
      url='/',
      root=url.root,
      resultpath=tempdir(),
      conf_data=conf_data,
      reference.cmaps=reference.cmaps
    ),
    Redirect$new('/index.rhtml')
  )
  
  if (is.null(.e$s)){
    .e$s <- Rhttpd$new()
  } else { 
    .e$s$remove(all=TRUE)
  }

  ## start server
  .e$s$launch(name="gcmap", app=gcmap)
}  
