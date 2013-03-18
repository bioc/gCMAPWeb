library(ArrayExpress)
library(affy)
library(gCMAP)
library(hgfocus.db)
##-------- processing of E-GEOD-6907

## retrieve raw data from ArrayExpress, normalize expression & summarize probesets
GEO6907.batch <- ArrayExpress("E-GEOD-6907")
GEO6907.eSet <- rma( GEO6907.batch )

## convert affymetrix probe to Entrez identifiers and 
## calculate mean expression for genes with multiple probes
GEO6907.eSet <- mapNmerge( GEO6907.eSet )

## split the dataset into individual perturbation experiments
GEO6907.list <- splitPerturbations( GEO6907.eSet, 
                    control="none",
                    factor.of.interest="COMPOUND",
                    controlled.factors="none")

## perform differential expression analysis (using limma)
GEO6907.cmap <- generate_gCMAP_NChannelSet( GEO6907.list , 
                                            uids=names( GEO6907.list ))

save( GEO6907.cmap, file="GEO6907_cmap.rdata")
