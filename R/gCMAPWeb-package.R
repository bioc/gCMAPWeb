##' A web-interface for gene-set enrichment analysis
##' 
##' This package provides a stand-alone web interface for gene-set enrichment analysis. Run either through R's internal Rhttpd server of deployed through an Apache2 webserver, gCMAPWeb allows users to search reference datasets stored in \code{\linkS4class{NChannelSet}} or \code{\linkS4class{CMAPCollection}} objects via three different methods:
##' \itemize{
##'   \item Directional queries performed by calculating the parametric JG score from the reference datasets
##'   \item Non-directional queries performed via Fisher's exact test
##'   \item Profile queires performed by calculating the parametric JG score from the query data
##' }
##' A web interace is populated based on a configuration file in YAML format and additional information from the reference dataset objects (if available).
##' For each query, a report is generated in html format, including graphs and tabular output. Extensive customization of the graphical user interface can easily be performed through global options.
##' @import Biobase
##' @import BiocGenerics
##' @import Rook
##' @import methods
##' @import grDevices
##' @name gCMAPWeb-package
##' @docType package
##' @title A web-interface for gene-set enrichment analysis
##' @author Thomas Sandmann
##' @keywords package
##' @references Jiang Z, Gentleman R., Extensions to gene set enrichment.
##'   Bioinformatics. 2007 Feb 1;23(3):306-13
##'   \url{http://www.ncbi.nlm.nih.gov/pubmed/17127676}
##' @examples
##' example( gCMAPWeb )
NULL

##' Example reference datasets
##'
##' Five example reference datasets containing simulated data to demonstrate the
##' functionality of the gCMAPWeb package. These objects were generated with the
##' \code{\link{exampleCMAP}} function using different seeds.
##'
##' cmap1, cmap2, cmap3 and cmap5 contain data for 1000 human Entrez identifiers.
##' cmap1, cmap2 and cmap3 are \code{\linkS4class{NChannelSet}} objects with one channel, the 'z' AssayDataElement.
##' cmap4 is an \code{\linkS4class{NChannelSet}} with data for 1000 mouse Entrez identifiers in the 'z' and 'log_fc' channels.
##' cmap5 is a code{\linkS4class{CMAPCollection}}.
##'
##' @aliases cmap1 cmap2 cmap3 cmap4 cmap5
##' @docType data
##' @keywords datasets
##' @format cmap1 to 4 are \code{\linkS4class{NChannelSet}} objects each with 1000 simulated z-scores for 10 samples. cmap5 is a \code{\linkS4class{CMAPCollection}} with membership information about 1000 genes and 10 gene sets.
##' @name cmap-data
NULL
