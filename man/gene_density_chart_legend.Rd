\name{gene_density_chart_legend}
\alias{gene_density_chart_legend}
\title{Html code for the legend of pie the pie chart of the gene-level report}
\usage{
  gene_density_chart_legend(text = getOption("gene.density.legend", default = "<strong>Top: </strong>The grey density shows the differential expression scores for all genes in this reference dataset, up-regulated genes have high scores, down-regulated genes have low scores and unchanged genes have scores around zero. If you submitted a signed query, the distribution of scores for your up-regulated genes are shown in green, and for those you labeled down-regulated in blue. Otherwise, a single distribution for all query genes is shown in black.<br><strong>Bottom: </strong>Each dash at the bottom of the plot corresponds to a single query gene, following the same color scheme as in the density plot above."))
}
\arguments{
  \item{text}{Character, text for the legend of the density
  plot on gene-level reports for non-directional and
  directional queries. Can be set as the global variabel
  "gene.density.legend".}
}
\value{
  Character string with html code
}
\description{
  Html code for the legend of pie the pie chart of the
  gene-level report
}
\author{
  Thomas Sandmann
}

