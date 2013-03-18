library(gCMAP)
library(ArrayExpress)
library(hthgu133a.db)
library(hgu133plus2.db)

##-------- processing E-GEOD-5258 (Broad CMAP version 1)

## retrieve the raw data from ArrayExpress
GEOD5258.batch <- ArrayExpress("E-GEOD-5258")

## Since its deposition, the first array platform has
## changed its name, so we update the annotation string
annotation(GEOD5258.batch[[1]]) <- "hthgu133a"

## As this experiment was performed on two different array platforms
## ArrayExpress returns a list with two affyBatch objects, one for 
## each array platform. We normalize each object separately.
GEOD5258.rma <- lapply( GEOD5258.batch, rma )
save(GEOD5258.rma, file="~/Desktop/GEOD5258.rma.rdata" )
rm( GEOD5258.batch )

## We select one probeset for each Entrez gene
GEOD5258.eSets <- lapply( GEOD5258.rma, mapNmerge)
save(GEOD5258.eSets, file="GEOD5258.eSets.normalized.merged.rdata" )
rm( GEOD5258.rma )

## now that we have mapped the expression values to Entrez Ids, we can combine
## the two ExpressionSets into one
GEOD5258.eSet <- mergeCMAPs( GEOD5258.eSets[[1]], GEOD5258.eSets[[2]] )

## We identify individual perturbation experiments and store them as individual 
## ExpressionSets in a list
## We are interested in studying the effect of the different compounds.
## Controls received treatment 'none' and need to be matched to 
## perturbations based on CellLine and Vehicle
GEOD5258.list <- splitPerturbations( GEOD5258.eSet, 
                                     factor.of.interest="Compound",
                                     control="none",
                                     controlled.factors=c("CellLine", "Vehicle")
)

## We compile information about each perturbation into a data.frame,
## with one row for each perturbation
sample.anno <- 
  t(sapply( GEOD5258.list, function( x ){
    perturb <- pData( x )
    perturb <- perturb[ perturb$cmap == "perturbation",][1,1:5]
    unlist( perturb )
  }))
sample.anno <- data.frame( apply( sample.anno, 2, as.character) )

## We perform differential expression analysis (using limma) on
## each perturbation instance separately
GEOD5258.cmap <- generate_gCMAP_NChannelSet( GEOD5258.list, 
                                             uids=1:length( GEOD5258.list ),
                                             sample.annotation=sample.anno)

## By thresholding the z-scores we create a CMAPCollection 
## with up- and down-regulated gene sets
GEOD5258.sets <- induceCMAPCollection( GEOD5258.cmap, element="z", higher=3, lower=-3 )
head( setSizes( GEOD5258.sets ) )
