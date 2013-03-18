\docType{methods}
\name{cmapRun}
\alias{cmapRun}
\alias{cmapRun,CMAPCollection,CMAPCollection-method}
\alias{cmapRun,CMAPCollection,eSet-method}
\alias{cmapRun,eSet,CMAPCollection-method}
\alias{cmapRun,eSet,eSet-method}
\alias{cmapRun,GeneSet,CMAPCollection-method}
\alias{cmapRun,GeneSet,eSet-method}
\alias{cmapRun,SignedGeneSet,eSet-method}
\title{cmapRun}
\arguments{
  \item{user.input}{An object of class GeneSet,
  SignedGeneSet, eSet or CMAPCollection with the
  user-provided query.}

  \item{cmap}{The reference database, a CMAPCollection or
  eSet object.}

  \item{lower}{Numeric, when reference databases are
  thresholded, genes with scores less than 'lower' are
  considered down-regulated. Default: getOption(
  "lower.threshold", default="-3")}

  \item{higher}{Numeric, when reference databases are
  thresholded, genes with scores larger than 'higher' are
  considered up-regulated. Default: getOption(
  "higher.threshold", default="3")}

  \item{element}{Character,specifying which channel /
  assayDataElement of the reference database to query.
  Default: getOption( "element", default="z")}

  \item{min.set.size}{integer, gene sets with less than
  min.set.size members will be dropped from
  CMAPCollections. Default: getOption( "min.set.size",
  default=5)}

  \item{...}{Arguments specific to individual methods}
}
\value{
  A CMAPResults object
}
\description{
  This S4 method dispatches a gCMAP analysis and selects a
  suitable gene-set-enrichment analysis method based on the
  class of its arguments.
}
\details{
  The cmapRun methods choose one of three approaches for
  gene-set-enrichment analyses, depending on the nature of
  the user-provided query and the data available in the
  reference databases.

  1. for non-directional gene set queries (e.g. a list of
  gene identifiers) a Fisher exact test is performed (with
  a call to the fisher_score method from the gCMAP package)
  to evaluate the overlap with significantly up- or
  down-regulated gene sets from the reference database. If
  the reference object is an NChannelSet, it is thresholded
  on the fly to obtain a set of up- or down-regulated
  genes. If the reference database is a CMAPCollection, the
  gene sets are used directly. Please note that the Fisher
  exact test does not take into account information about
  the directionality of gene regulation (potentially)
  available in the reference database.

  2. for directional gene set queries (e.g. two separate
  lists of up- and down-regulated genes of interest) the
  gene expression scores available in the reference
  database are summarized as the JG score by running the
  gsealm_jg_score method from the gCMAP package.

  3. for queries with complete differential gene expression
  profiles, directional gene sets are induced from the
  reference database. As above, the JG score is used to
  summarize the expression changes for each gene set, but
  this time the sets are derived from the database, while
  the scores are provided by the user.

  This method is called by the cmapAnalysis function of the
  gCMAPWeb package.

  Please note: Most of the parameters of the cmapRun
  methods can be set as global options.
}
\seealso{
  \code{\link{gsealm_jg_score}} and
  \code{\link{fisher_score}}
}

