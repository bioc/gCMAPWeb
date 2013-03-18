library(ArrayExpress)
library(affy)
library(hgfocus.db)
library(gCMAP)
library(Matrix)
##-------- processing of E-GEOD-1541

## retrieve raw data from ArrayExpress, normalize expression & summarize probesets
GEO1541.batch <- ArrayExpress("E-GEOD-1541")
GEO1541.eSet <- rma( GEO1541.batch )

## convert to Entrez ids and calculate mean expression 
## for genes with multiple probes
GEO1541.eSet <- mapNmerge( GEO1541.eSet )

## list experimental factors
grep( "^Factor", varLabels( GEO1541.eSet), value=TRUE)

## split the dataset into individual perturbation experiments
GEO1541.list <- splitPerturbations( GEO1541.eSet, 
                    control="none",
                    factor.of.interest="Compound",
                    controlled.factors="all")

## perform differential expression analysis (using limma)
GEO1541.cmap <- generate_gCMAP_NChannelSet( GEO1541.list , 
                                            uids=names( GEO1541.list ))
save( GEO1541.cmap, file="GEO1541_cmap.rdata")

## create CMAPCollection by thresholding z-scores
GEO1541.sets <- induceCMAPCollection( GEO1541.cmap, element="z", higher=3, lower=-3 )
setSizes( GEO1541.sets )

