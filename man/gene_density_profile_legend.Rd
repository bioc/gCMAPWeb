\name{gene_density_profile_legend}
\alias{gene_density_profile_legend}
\title{Html code for the legend of the density charts of the gene-level reports from Profile queries}
\usage{
  gene_density_profile_legend(text = getOption("gene.profile.legend", default = "<strong>Top: </strong>The grey density displays the distribution of all <strong>differential expression scores in the query</strong> you submitted. The colored line(s) highlight where the genes annotated as differentially expressed in the reference experiment fall in your query distribution.<br>Scores for genes annotated as up-regulated in the reference experiment are shown in green, those annotated as down-regulated in the reference dataset are shown in blue.<br><strong>Bottom: </strong>Each dash at the bottom of the plot corresponds to a gene in the reference dataset, following the same color scheme as in the density plot above."))
}
\arguments{
  \item{text}{Character, text for the legend of the density
  plot on gene-level reports for profile queries. Can be
  set as the global variabel "gene.profile.legend".}
}
\value{
  Character string with html code
}
\description{
  Html code for the legend of the density charts of the
  gene-level reports from Profile queries
}
\author{
  Thomas Sandmann
}

