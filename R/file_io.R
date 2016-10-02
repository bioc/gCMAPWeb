##' This function parses the gCMAPWeb configuration file (in yaml format) and returns the information as a list
##'
##' @title Parser for gCMAPWeb configuration file
##' @param config.file.path character, full path to gCMAPWeb configuration file
##' @return a nested list
##' @author Thomas Sandmann
##' @importFrom yaml yaml.load_file
read_config_file <- function(
  config.file.path=system.file(
    "config",
    "config.yml",
    package="gCMAPWeb"
    )
  ) {
  ## verify the config file exists
  stopifnot( file.exists(config.file.path ))
  
  ## load & parse the file
  tryCatch({
    conf_data <- yaml::yaml.load_file( config.file.path )
  }, error = function(e) {
      message( paste("Your configuration file could not be parsed.",
                     e[1],
                     "Consider validating the canonical yaml format here: http://yaml-online-parser.appspot.com/", "",sep="\n"))
    })
  return( conf_data )
}

##' This function validates the content of the gCMAPWeb configuration file.
##'
##' Verifies that
##' 1. all required fields are present
##' 2. at least one species has been defined
##' 3. all supported annotation packages are available
##' 4. all cmaps have unique labels
##' 5. each supported species has at least one associated reference dataset 
##' @title Validation of gCMAPWeb configuration file
##' @param config.file.path full path to gCMAPWeb configuration file (in yaml format)
##' @return Information from a valid configuration file is returned as a nested list. Invalid entries in the config file with cause an error.
##' @author Thomas Sandmann
##' @export
##' @examples
##' ## read the example configuration yaml file without validation
##' library(yaml)
##' conf1 <- yaml.load_file( system.file("config", "config.yml", 
##'              package="gCMAPWeb") )
##' ## read the example configuration file and validate that
##' ## all required information is provided and valid
##' conf2 <- validate_config_file( system.file("config", "config.yml", 
##'              package="gCMAPWeb") )
##' identical( conf1, conf2)
validate_config_file <- function( config.file.path ){
  ## read yaml file
  conf_data <- read_config_file( config.file.path)

  ## are all required fields present ?
  required.fields <- c("species")
  missing.fields <- setdiff(
    required.fields,
    names( conf_data )
    )
  if( length( missing.fields ) > 0){
    stop(
      sprintf("The '%s' field(s) is/are missing from the configuration file",
              paste(
                missing.fields,
                collapse=","
                )
              )
      )
  }
  
  ## has at least input type been defined ? ?
  if(
    length(
      intersect(
        getOption(
          "supported.inputType",
          default=c(
            "single",
            "non-directional",
            "directional",
            "profile")
          ), 
        c(
          "single",
          "non-directional",
          "directional",
          "profile"
          )
        )
      ) == 0){
    stop("Please specify at least one of 'single', 'non-directional', 'directional' or 'profile' as 'inputType' via the global 'supported.inputType' parameter.")
  }
  
  ## has at least one species been defined ?
  n.species <- length( conf_data[["species"]])
  if( length( n.species ) == 0){
    stop("At least one supported species must be specified.")
  }
  
  ## has a supported identifier type been specified ?
  if(
    length(
      intersect(
        getOption(
          "supported.idType",
          default=c(
            "symbol",
            "entrez",
            "probe"
            )
          ),
        c(
          "symbol",
          "entrez",
          "probe"
          )
        )
      ) == 0){
    stop("Please specify at least one of 'symbol', 'entrez' or 'probe' as 'idType' via the global 'supported.idType' parameter.")
  }
  
  ## are all supported annotation packages available ?
  required.packages <- unlist(
    sapply(
      conf_data[["species"]],
      function( supported.species ){
    paste( 
      c(
        supported.species[["annotation"]],
        supported.species[["platforms"]]),
      "db",
      sep=".")
  }))
  package.available <- sapply(
    required.packages,
    function( x ){
      is.element(x, installed.packages()[,1])
    })
  if(
    !all(
      package.available
      )
    ){
    missing.packages <- required.packages[! package.available]
    stop(
      sprintf(
        "Package(s) %s are not installed on this system!",
        paste(
          missing.packages,
          collapse=", ")
        )
      )
  }
  
  ## do all cmaps have unique labels ?
  if(
    any(
      duplicated(
        unlist(
          sapply(
            conf_data$species,
            function(x) names(x[["cmaps"]])
            )
          )
        )
      )
    ){
    stop("All reference datasets (cmaps) must be associated with a unique label.")
  }
    
  ## can the supported species be queried with at least one cmap ?
  for( supported.species in conf_data[["species"]]){
    reference.datasets <- unlist( supported.species[["cmaps"]] )
    if( length(reference.datasets) == 0){
      stop("At least one reference dataset (cmap) must be specified for each supported species.")
    }
  }
  return( conf_data )
}
##' This function connects to / loads all reference datasets and returns them in a list
##'
##' @title Loading reference datasets
##' @param conf_data  list, the configuration data as returned by the read_config_file function
##' @param min.set.size integer, gene sets with less than min.set.size members will be dropped from CMAPCollections
##' @return list of eSet objects
##' @author Thomas Sandmann
##' @export
##' @importMethodsFrom gCMAP minSetSize
##' @examples
##' library(yaml)
##' ## read the example configuration file
##' conf_data <- yaml.load_file( system.file("config", "config.yml", 
##'              package="gCMAPWeb") )
##' ## load the example reference NChannelSet objects specified in 
##' ## the configuration file
##' ref.cmaps <- load_cmaps( conf_data )
##' class( ref.cmaps )
##' names( ref.cmaps )
##' ref.cmaps[[1]]
load_cmaps <- function(
  conf_data,
  min.set.size=getOption(
    "min.set.size",
    default=5)
  ){
  ## loop over all supported species
  cmaps <- sapply(
    conf_data[["species"]],
    function( supported.species ){
    reference.datasets <- unlist(
      supported.species[["cmaps"]]
      )
    
    ## loop over all supported reference datasets for this species
    cmaps.this.species <- sapply(
      reference.datasets,
      function( dataset.name ){
        tryCatch({    
          if( file.exists( dataset.name )) { ## interpret data as path to robject
            get( load( dataset.name ))
            
          } else { ## interpret data as build-in package
            get( load( system.file( "data", paste0(dataset.name, ".rdata"),
                                   package="gCMAPWeb" )
                      )
                )
          }
          
        }, error = function(e) {
          print( paste("Error: the following dataset could not be loaded:", dataset.name))
        })
      })
    names( cmaps.this.species ) <- names( reference.datasets )
    return( cmaps.this.species )
  })
  ## flatten the list and assign the user-defined label to each cmap
  cmaps.all <- unlist( cmaps )
  names( cmaps.all) <- unlist(
    sapply(
      conf_data$species,
      function(x) names(x[["cmaps"]])))
  
  ## drop excluded annotation columns from phenoData slot (based on varMetadata column "include")
  cmaps.all <-lapply( cmaps.all, function( eset ){
    if("include" %in% colnames( varMetadata( eset))){
      include.info <- varMetadata( eset)$include
      phenoData( eset ) <- phenoData( eset )[,which( varMetadata(eset)$include != FALSE )]
    }
    return( eset )
  })
  
  for( n in 1:length( cmaps.all ) ){
    ## BigMatrix assayDataElements are deprecated and this code will be removed
    # if( !suppressWarnings(require("bigmemoryExtras", quietly=TRUE, character.only=TRUE))){
    #   attachAssayDataElements <- NA # to suppress R CMD check warnings
    # } else {
    #   attachAssayDataElements( assayData( cmaps.all[[n]] ))
    # }
    if( inherits( cmaps.all[[n]], "CMAPCollection")){
      cmaps.all[[n]] <-minSetSize( cmaps.all[[n]], min.members=min.set.size)
    }
  }
  return( cmaps.all )
}
