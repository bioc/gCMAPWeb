\name{gene_pie_chart_legend}
\alias{gene_pie_chart_legend}
\title{Html code for the legend of the pie chart of the gene-level report}
\usage{
  gene_pie_chart_legend(text = getOption("gene.pie.legend", default = "This pie chart shows how many of your query genes are also annotated in this reference dataset. The reported FDR and effect sizes were calculated using Fisher's exact test and reflect the likelihood of observing the overlap (grey) between your query and the reference gene sets, taking into account the size of the reference set and the total number of genes in the database."))
}
\arguments{
  \item{text}{Character, text for the legend of the pie
  chart on gene-level reports for non-directional queries.
  Can be set as the global variabel "gene.pie.legend".}
}
\value{
  Character string with html code
}
\description{
  Html code for the legend of the pie chart of the
  gene-level report
}
\author{
  Thomas Sandmann
}

