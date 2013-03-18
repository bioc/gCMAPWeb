\name{create_heatmap_legend}
\alias{create_heatmap_legend}
\title{Legend for overview heatmap}
\usage{
  create_heatmap_legend(text = getOption("heatmap.legend", default = "<strong>Gene scores</strong><br><strong>Center: </strong>Heatmap of query gene scores (columns) in the reported reference datasets (rows), colored according to differential expression score. <strong>Left: </strong>Dendrogram from hierarchically clustering the reference datsets (rows) according to the query gene scores (only shown if 3 or more sets were reported). <strong>Top: </strong> query gene direction (black for up, grey for down-regulated query genes), only available for directional queries. <strong>Right: </strong>Direction of similarity (correlated, green or anti-correlated, blue). Click to enlarge."))
}
\arguments{
  \item{text}{Character, text for the legend of the density
  plot the main report page. Can be set as the global
  variabel "gene.set.legend".}
}
\value{
  Character
}
\description{
  Content for the legend of the overview heatmap plot
  displayed on the main results page for directional and
  non-directional queries
}
\author{
  Thomas Sandmann
}

