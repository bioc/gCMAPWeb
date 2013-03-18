.setUp <- function(){
  conf_data <- list( species=list( 
    human=list( annotation = "org.Hs.eg", 
                platforms=c("hgug4100a","hgug4110b","hgu133a","hgu133plus2"), 
                cmaps=list( Example1="cmap1", Example2="cmap2")), 
    mouse=list( annotation="org.Mm.eg", 
                platforms=c("mgug4104a", "mgug4104a"), 
                cmaps=list(Example4="cmap4"))), 
                     inputType=c("GeneSet", "SignedGeneSet", "Profile", "Experiment"), 
                     idType=c("symbol", "entrez", "probe"), 
                     colNames=c("name", "set", "trend", "pval", "effect"))
  return( conf_data )
}

test_load_cmaps <- function(){
  conf_data <- yaml::yaml.load_file( system.file("config", "config.yml", 
                                           package="gCMAPWeb") ) 
  ref.cmaps <- load_cmaps( conf_data )
  checkTrue( identical( class( ref.cmaps), "list"), "did not return a list." )
  checkTrue( identical( class( ref.cmaps[[1]])[1], "NChannelSet"), "did not return an NChannelSet as a first list element" )
}

test_validate_config_file <- function(){
  conf1 <- yaml::yaml.load_file( system.file("config", "config.yml", 
                                       package="gCMAPWeb") )
  conf2 <- validate_config_file( system.file("config", "config.yml", 
                                             package="gCMAPWeb") )
  checkTrue( identical( conf1, conf2), "validate_config_file and yaml.load_file did not return the same results" )
}

test_exampleCMAP <- function(){
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    checkTrue( identical( class( exampleCMAP() )[1], "NChannelSet"), "did not generate an NChannelSet")
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_examplePOST <- function(){
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    cmap <- exampleCMAP()
  checkTrue( identical( class( examplePost( cmap) ), "list"), "did not generate a list")
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_parse_textarea_input <- function(){
  checkTrue( 
    identical(
    gCMAPWeb:::parse_textarea_input(URLencode( "A, text with 5 ;elements"))[[1]],
    c("A", "text", "with", "5", "elements")
    )
  )
}

test_parse_file_input <- function(){
  temp.file <- list( tempfile=system.file( 
    file.path( "extdata", 
               "profile_entrez_hs.txt"), 
    package="gCMAPWeb"))
  parsed.file <- gCMAPWeb:::parse_file_input( temp.file )
  checkTrue( class( parsed.file ) == "list", "did not return a list")
  checkTrue( length( parsed.file ) == 1000, "did not return a 1000 rows")
}

test_process_score_input <- function(){
  temp.file <- list( tempfile=system.file( 
    file.path( "extdata", 
               "profile_entrez_hs.txt"), 
    package="gCMAPWeb"))
  parsed.file <- gCMAPWeb:::parse_file_input( temp.file )
  scores <- gCMAPWeb:::process_score_input( parsed.file )
  checkTrue( class( scores ) == "data.frame", "did not return a data.frame")
  checkTrue( all( dim( scores ) == c(1000,2)), "did not return 1000 rows and two columns")
}

test_create_GeneSet<- function(){
  conf_data <- .setUp()
  ## post with 'symbol' idType
  post <- list( inputType="GeneSet", species="human", idType ="symbol", cmap_1="Example1", cmap_2="Example2", query_data=c("TP53", "ACTN2", "dFSDFA"))
  res <- gCMAPWeb:::create_GeneSet( post, conf_data)[[1]]
  checkTrue( class( res ) == "GeneSet", "created GeneSet")
  checkTrue( class( geneIdType( res)) == "EntrezIdentifier", "created GeneSet with EntrezIdentifiers")
  checkTrue( length( geneIds( res)) == 2, "did not match two out of three symbols to Entrez ids")
}

test_create_SignedGeneSet<- function(){
  conf_data <- .setUp()
  ## post with 'symbol' idType
  post <- list( inputType="GeneSet", species="human", idType ="symbol", cmap_1="Example1", cmap_2="Example2", query_data_up=c("TP53", "ACTN2", "dFSDFA"), query_data_down=c("TP53", "MYC"))
  res <- gCMAPWeb:::create_SignedGeneSet( post, conf_data)[[1]]
  checkTrue( class( res ) == "SignedGeneSet", "created GeneSet")
  checkTrue( class( geneIdType( res)) == "EntrezIdentifier", "created GeneSet with EntrezIdentifiers")
  checkTrue( length( geneIds( res)) == 2, "did not return 2 Entrez ids")
}

test_create_query_objects <- function(){
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    conf_data <- .setUp()
    ## create NChannelSet "sample.cmap"
    sample.cmap <- exampleCMAP()
    query.types <- c(GeneSet="non-directional", 
                     SignedGeneSet="directional",
                     ExpressionSet="profile")
    res <- sapply( 1:length( query.types ), function(n){
      ## create simulated user input
      post <- examplePost(sample.cmap, inputType=query.types[n])
      query <- suppressMessages( gCMAPWeb:::create_query_objects( post, conf_data)[[1]] )
      class( query ) == names( query.types)[n] 
    })
  checkTrue( all( res ), sprintf("only generated %s/%s query objects", 
                                 sum( res )/length(res)))
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_create_GeneIdentifierType <- function(){
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    conf <- list(species=list(human=list(annotation="org.Hs.eg")))
    cmap <- exampleCMAP()
    id.type <- c(SymbolIdentifier="symbol", 
                 EntrezIdentifier="entrez", 
                 AnnotationIdentifier="probe")
    res <- sapply( 1:length(id.type), function(n){
      post <- examplePost(cmap, idType=id.type[n], array.platform="test")
      identical(
        class(gCMAPWeb:::create_GeneIdentifierType(post, conf))[1],
        names(id.type[n])
      )
    })
    checkTrue( all( res ), sprintf("only generated %s/%s GeneIdentifierTypes", 
                              sum( res )/length(res))) 
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_entrez_GeneSets <- function(){
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    gs <- GeneSet( c("1000", "999"), geneIdType=EntrezIdentifier("org.Hs.eg"))
    checkTrue( class( suppressMessages(gCMAPWeb:::entrez_GeneSets( gs ))) == "GeneSet",
               "did not validate a GeneSet with Entrez identifiers 1000 and 999.")
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_stripPrefix <- function(){
  ids <- c("GeneId:1", "2", "GeneID:3")
  checkTrue( identical( gCMAPWeb:::stripPrefix( ids ), c("GeneId:1","2","3")),
             "did not strip GeneID from identifiers correctly.")
}

test_convert_gene_identifiers <- function(){
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    gene.ids <- c("TP53", "CDC2", "THISISNOTAGENE")
    query.IdType <- GSEABase::SymbolIdentifier("org.Hs.eg")
    species <- "org.Hs.eg"
    converted.ids <- gCMAPWeb:::convert_gene_identifiers( gene.ids, query.IdType, species)
    checkTrue( class(converted.ids ) == "list",
               "did not return a list.")
    checkTrue( all( converted.ids$original.ids == c("TP53", "CDC2")), "did not identify TP53 and CDC2 entrez ids.")
    checkTrue( class( converted.ids$conversion$translation.table) == "data.frame",
               "did not return a data frame as translation.table")
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

##---------- tests with simulated user input (posts)

test_cmapRun_entrez <- function() {
  conf_data <- .setUp()
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    
    ## create NChannelSet "sample.cmap"
    sample.cmap <- exampleCMAP()
    
    query.types <- c("non-directional", "directional", "profile")
    for( qtype in query.types ){
      
      ## create simulated user input
      post <- examplePost(sample.cmap, inputType=qtype) ## create CMAPParams object
      
      ## create query object
      query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
      results <- gCMAPWeb:::cmapRun( query, sample.cmap)
      ## check that the first CMAP instance is the top hit in each case
      checkTrue( as.character( cmapTable( results )[1,"set"]) == "Exp1",
                 msg=paste("cmapRun did not identify the test gene set using a", qtype, "query.")
      )
    }
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_cmapRun_entrez_mouse <- function() {
  conf_data <- .setUp()
  if( is.element("org.Mm.eg.db", installed.packages()[,1])){
    require( org.Mm.eg.db )
    
    ## create NChannelSet "sample.cmap"
    sample.cmap <- exampleCMAP(universe="org.Mm.eg.db")
    
    query.types <- c("non-directional", "directional", "profile")
    for( qtype in query.types ){
      
      ## create simulated user input
      post <- examplePost(sample.cmap, inputType=qtype, species="mouse") ## create CMAPParams object
      
      ## create query object
      query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
      results <- gCMAPWeb:::cmapRun( query, sample.cmap) ## run analysis
      
      ## check that the first CMAP instance is the top hit in each case
      checkTrue( as.character( cmapTable( results)[1,"set"]) == "Exp1",
                 msg=paste("cmapRun did not identify the test gene set using a", qtype, "query.")
      )
    }
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Mm.eg.db is not available and skipped this test.")
  }
}

test_cmapRun_affy <- function() {
  conf_data <- .setUp()
  if( is.element("hgu133plus2.db", installed.packages()[,1])){
    require( hgu133plus2.db )
    
    ## create NChannelSet "sample.cmap"
    affy.cmap <- exampleCMAP(universe="hgu133plus2.db",idType=NULL)
    sample.cmap <- mapNmerge( affy.cmap )
    
    query.types <- c("non-directional", "directional", "profile")
    for( qtype in query.types ){
      
      ## create simulated user input
      post <- examplePost(affy.cmap, inputType=qtype, array.platform="hgu133plus2", idType="probe")
      post$selected_cmaps <- "sample.cmap"  ## query Entrez reference set        
      
      ## create query object
      query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
      results <- gCMAPWeb:::cmapRun( query, sample.cmap) ## run analysis
      
      ## check that the first CMAP instance is the top hit in each case
      checkTrue( as.character( cmapTable( results)[1,"set"]) == "Exp1",
                 msg=paste("cmapRun did not identify the test gene set using a", qtype, "query.")
      )
    }
  } else {
    checkTrue( TRUE, "noticed that annotation package hgu133plus2.db is not available and skipped this test.")
  }
}

test_cmapRun_affy_mouse <- function() {
  conf_data <- .setUp()
  if( is.element("mgug4104a.db", installed.packages()[,1])){
    require( mgug4104a.db )
    
    ## create NChannelSet "sample.cmap"
    affy.cmap <- exampleCMAP(universe="mgug4104a.db",idType=NULL)
    sample.cmap <- mapNmerge( affy.cmap )
    
    query.types <- c("non-directional", "directional", "profile")
    for( qtype in query.types ){
      
      ## create simulated user input
      post <- examplePost(affy.cmap, inputType=qtype, array.platform="mgug4104a", idType="probe", species="mouse")
      post$selected_cmaps <- "sample.cmap"  ## query Entrez reference set
      
      ## create query object
      query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
      results <- gCMAPWeb:::cmapRun( query, sample.cmap) ## run analysis
      
      ## check that the first CMAP instance is the top hit in each case
      checkTrue( as.character( cmapTable( results)[1,"set"]) == "Exp1",
                 msg=paste("cmapRun did not identify the test gene set using a", qtype, "query.")
      )
    }
  } else {
    checkTrue( TRUE, "noticed that annotation package mgug4104a.db is not available and skipped this test.")
  }
}

test_cmapRun_symbol <- function() {
  conf_data <- .setUp()
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    
    ## create NChannelSet "sample.cmap"
    symbol.cmap <- exampleCMAP(idType="SYMBOL")
    sample.cmap <- symbol.cmap
     
    entrez.ids <- gCMAPWeb:::.alias2entrez( featureNames( symbol.cmap ),
                              "org.Hs.eg")$translation.table[,"EntrezId"]
    duplicate.entrez <- duplicated( entrez.ids )
    sample.cmap <- sample.cmap[! duplicate.entrez, ]
    featureNames( sample.cmap) <- entrez.ids[ ! duplicate.entrez]
    query.types <- c("non-directional", "directional", "profile")
    for( qtype in query.types ){
      
      ## create simulated user input
      post <- examplePost(symbol.cmap, inputType=qtype, idType="symbol")
      post$selected_cmaps <- "sample.cmap"  ## query Entrez reference set
      
      ## create query object
      query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
      results <- gCMAPWeb:::cmapRun( query, sample.cmap) ## run analysis
      
      ## check that the first CMAP instance is the top hit in each case
      checkTrue( as.character( cmapTable( results)[1,"set"]) == "Exp1",
                 msg=paste("cmapRun did not identify the test gene set using a", qtype, "query.")
      )
    }
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}

test_cmapRun_symbol_mouse <- function() {
  conf_data <- .setUp()
  if( is.element("org.Mm.eg.db", installed.packages()[,1])){
    require( org.Mm.eg.db )
    
    ## create NChannelSet "sample.cmap"
    symbol.cmap <- exampleCMAP(idType="SYMBOL", universe="org.Mm.eg.db")
    sample.cmap <- symbol.cmap
    entrez.ids <- gCMAPWeb:::.alias2entrez( featureNames( symbol.cmap ),
                                            "org.Mm.eg")$translation.table[,"EntrezId"]
    duplicate.entrez <- duplicated( entrez.ids )
    sample.cmap <- sample.cmap[! duplicate.entrez, ]
    featureNames( sample.cmap) <- entrez.ids[ ! duplicate.entrez]
    
    query.types <- c("non-directional", "directional", "profile")
    for( qtype in query.types ){
      
      ## create simulated user input
      post <- examplePost(symbol.cmap, inputType=qtype, idType="symbol", species="mouse")
      post$selected_cmaps <- "sample.cmap"  ## query Entrez reference set
      
      ## create query object
      query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
      results <- gCMAPWeb:::cmapRun( query, sample.cmap) ## run analysis
      
      ## check that the first CMAP instance is the top hit in each case
      checkTrue( as.character( cmapTable( results)[1,"set"]) == "Exp1",
                 msg=paste("cmapRun did not identify the test gene set using a", qtype, "query.")
      )
    }
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Mm.eg.db is not available and skipped this test.")
  }
}

test_generate_report <- function(){
  conf_data <- .setUp()
  if( is.element("org.Hs.eg.db", installed.packages()[,1])){
    require( org.Hs.eg.db )
    
    ## create NChannelSet "sample.cmap"
    sample.cmap <- exampleCMAP()
    qtype <- "directional"
    ## create simulated user input
    post <- examplePost(sample.cmap, inputType=qtype)
    
    ## create query object
    query <- gCMAPWeb:::create_query_objects( post, conf_data)[[1]]
    results <- gCMAPWeb:::cmapRun( query, sample.cmap)
    report <- gCMAPWeb:::generate_report( 
      results, 
      reference=sample.cmap, 
      reference.name="sample.cmap",
      annotation.db="org.Hs.eg.db",
      element="z", 
      query=query, 
      tmp_filename=tempdir()
    )
    checkTrue( class( report ) == "list", "did not return a list.")
    checkTrue( length( report ) == 11, "did not return a list with 11 elements.")
  } else {
    checkTrue( TRUE, "noticed that annotation package org.Hs.eg.db is not available and skipped this test.")
  }
}
