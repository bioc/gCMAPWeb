library(affy)
library(gCMAP)
library(hgu133plus2.db)
library(hgu133plus2cdf)

##-------- processing of E-GEOD-7216

## retrieve raw data from ArrayExpress (~ 70 Mb)
url <- "http://www.ebi.ac.uk/arrayexpress/files/E-GEOD-7216/E-GEOD-7216.eSet.r"
download.file(url, destfile=file.path(tempdir(), "GEOD7216.rdata"))
GEO7216.batch <- get(load(file.path( tempdir(),"GEOD7216.rdata" )))

## normalize expression & summarize probesets
GEO7216.eSet <- rma( GEO7216.batch )

## convert to Entrez ids and calculate mean expression 
## for genes with multiple probes
GEO7216.eSet <- mapNmerge( GEO7216.eSet )

## check experimental factors
conditions <- grep("^Factor", varLabels( GEO7216.eSet), value=TRUE)
pData( GEO7216.eSet) <- pData( GEO7216.eSet)[, conditions, drop=FALSE]

## split the dataset into individual perturbation experiments
GEO7216.list <- splitPerturbations( GEO7216.eSet, 
                    control="none",
                    factor.of.interest="compound",
                    controlled.factors="none")
sample.anno <- annotate_eset_list( GEO7216.list )
head( sample.anno )

## perform differential expression analysis (using limma)
GEO7216.cmap <- generate_gCMAP_NChannelSet( GEO7216.list , 
                                            uids=names( GEO7216.list ))

## create CMAPCollection by thresholding z-scores
GEO7216.sets <- induceCMAPCollection( GEO7216.cmap, element="z", higher=4, lower=-4 )
setSizes( GEO7216.sets )

save( GEO7216.cmap, file="GEO7216_cmap.rdata")
