##----- String vs. CMAP: simple lookup for single genes
setMethod(
  "cmapRun", 
  signature( user.input= "character", cmap="NChannelSet" ),
  function(user.input, cmap,
           z.column=getOption( "z.column", default="z"),
           p.column=getOption( "p.column", default="p"),
           p.from.z=FALSE
  ){
        
    ## check if data is available for the gene of interest
    if( ! user.input %in% featureNames( cmap )){
      stop( sprintf("Sorry, this reference dataset does not contain Entrez identfier %s.", user.input ),
            call. = FALSE)
      
    } else {
      ## retrieve all scores for the gene of interest
      scores <- sapply( assayDataElementNames( cmap ), 
                        function(x){ 
                          assayDataElement(cmap, x)[user.input,,drop=FALSE]
                        })
      scores <- data.frame( scores )
      
      if( p.from.z == TRUE){
        stopifnot( z.column %in% assayDataElementNames( cmap ))
        scores <- scores[, setdiff( colnames( scores ), 
                                    c("pval", "p", "padj", "FDR")), drop=FALSE]
        scores$pval <- pnorm(abs(scores[, z.column]), lower.tail=FALSE)*2
        scores$padj <- p.adjust( scores[,"pval"], method="BH")  
      } else {
        stopifnot( p.column %in% assayDataElementNames( cmap ))
        scores <- scores[, setdiff( colnames( scores ), 
                                    c("padj", "FDR")), drop=FALSE]
        scores$padj <- p.adjust( scores[, p.column], method="BH")
      }
      
      colnames( scores )[ which( colnames( scores) == z.column)] <- "effect"
      
      res <- gCMAP:::CMAPResults(
        data=data.frame(
          set=sampleNames( cmap),
          scores, 
          nSet=1,
          nFound=1,
          pData( cmap )
        ),
        docs =sprintf( "\n Differential expression scores for gene %s", user.input) 
      )
      if( z.column == "z") {
        varMetadata(res)["effect",
                         "labelDescription"] <- "z-score"
      } else {
        varMetadata(res)["effect",
                         "labelDescription"] <- z.column
      }
      if( "exprs" %in% varLabels( res )) {
        varMetadata(res)["exprs",
                         "labelDescription"] <- "Mean gene expression across all conditions"
      }
    }
    return( res )
  }
)

##----- Set vs. Set comparison with fisher_score

#' @rdname cmapRun-methods
#' @aliases cmapRun,CMAPCollection,CMAPCollection-method
setMethod(
  "cmapRun", 
  signature( user.input= "CMAPCollection", cmap="CMAPCollection" ),
  function(user.input, cmap) {
    universe <- featureNames(cmap)
    ## run scoring method
    res <- fisher_score(user.input, cmap, universe=universe, keep.scores=TRUE)
    varMetadata( res )["nFound","labelDescription"] <- sprintf( "Number of query genes with significant differential expression scores ( < %s or > %s ) in this reference experiment.",
                                                                getOption( "lower.threshold", default="-3"),
                                                                getOption( "higher.threshold", default="3")
    )
    res <- res[ res@data$nSet <= getOption( "max.set.size", default=Inf), ]
    return(res)
  }
)

#' @rdname cmapRun-methods
#' @aliases cmapRun,GeneSet,CMAPCollection-method
setMethod(
  "cmapRun",
  signature( user.input= "GeneSet", cmap="CMAPCollection" ),
  function(user.input, cmap, ...) {
    cmapRun(as(user.input, "CMAPCollection"), cmap, ...)
  })

#' @rdname cmapRun-methods
#' @aliases cmapRun,GeneSet,eSet-method
setMethod(
  "cmapRun",
  signature( user.input= "GeneSet", cmap="eSet" ),
  function(user.input, cmap, ...) {
    cmapRun(as(user.input, "CMAPCollection"), cmap, ...)
  })

#' @rdname cmapRun-methods
#' @aliases cmapRun,CMAPCollection,eSet-method
setMethod(
  "cmapRun",
  signature(user.input= "CMAPCollection", cmap="eSet" ),
  function(user.input, 
           cmap, 
           lower=getOption( "lower.threshold", default=-3), 
           higher=getOption( "higher.threshold", default=3), 
           element=getOption( "element", default="z"), 
           min.set.size=getOption( "min.set.size", default=5),
           keep.scores=TRUE) {
    
    universe <- featureNames(cmap)
    ## run scoring method
    res <- fisher_score(user.input, cmap, universe=universe, keep.scores=keep.scores,
                        lower=lower, higher=higher, element="z",min.set.size=min.set.size)
    varMetadata( res )["nFound","labelDescription"] <- sprintf( "Number of query genes with significant differential %s scores ( < %s or > %s ) in this reference experiment.", element, lower, higher)
    res <- res[ res@data$nSet <= getOption( "max.set.size", default=Inf), ]
    return( res )
  })

##--------- SignedGeneSet vs. eSet comparison with gsealm_jg_score

#' @rdname cmapRun-methods
#' @aliases cmapRun,SignedGeneSet,eSet-method
setMethod(
  "cmapRun", ## gsealm_score for SignedGeneSets
  signature( user.input="SignedGeneSet", cmap= "eSet" ),
  function(user.input, cmap, element=getOption( "element", default="z")) {
    ## run scoring method
    out <- gsealm_jg_score(user.input, cmap, element=element, removeShift=FALSE, keep.scores=TRUE)
    varMetadata( out )["nFound","labelDescription"] <- "Total number of query genes for which expression scores are available in this reference experiment (up-, down- or unchanged)."
    return( out )
  }
)

##--------- eSet Profile vs. eSet comparison with gsealm_jg_score

#' @rdname cmapRun-methods
#' @aliases cmapRun,eSet,CMAPCollection-method
setMethod(
  "cmapRun",
  signature( user.input="eSet", cmap= "CMAPCollection" ),
  function(user.input, cmap, element=getOption( "element", default="z")) {
    
    stopifnot( ncol(user.input) == 1)## two-class comparisons are not supported, yet
    out <- gsealm_jg_score(user.input, cmap, element="exprs", keep.scores=TRUE)
    varMetadata( out )["nFound","labelDescription"] <- "Total number of query genes for which expression scores are available in this reference experiment (up-, down- or unchanged)."
    out <- out[ out@data$nSet <= getOption( "max.set.size", default=Inf), ]
    return( out )
  }
)

#' @rdname cmapRun-methods
#' @aliases cmapRun,eSet,eSet-method
setMethod(
  "cmapRun",
  signature( user.input="eSet", cmap= "eSet" ),
  function( user.input, cmap, lower=getOption( "lower.threshold", default=-3), higher=getOption( "higher.threshold", default=3), element=getOption( "element", default="z")) {
    
    ## two-class comparisons are not supported, yet
    stopifnot( ncol(user.input) == 1)
    
    ## induce signed sets from the reference datasets
    cmap.collection <- induceCMAPCollection( cmap, lower=lower, higher=higher, element=element )
    cmap.collection <- cmap.collection[, setSizes(cmap.collection)$n.total != 0 ] ## remove empty sets
    if( ncol(cmap.collection) == 0){
      stop("None of the genes in the reference dataset passed the score cutoff to induce gene sets.")
    }
    cmapRun(user.input, cmap.collection)
  }
)
