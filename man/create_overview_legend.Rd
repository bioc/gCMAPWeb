\name{create_overview_legend}
\alias{create_overview_legend}
\title{Legend for overview density plot}
\usage{
  create_overview_legend(text = getOption("gene.set.legend", default = sprintf("<strong>Gene-set scores</strong><br><strong>Left: </strong>Distribution of similarity scores between your query and all instances in the selected reference database. For reference, a standard normal distribution is indicated as a dotted line. The (up to) top \%s scores for significantly correlated (green) or anti-correlated (blue) instances are indicated in the rug.<br><strong>Right: </strong>A heatmap of the rank-ordered similarity scores.", getOption("max.results", 
        default = 50))))
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
  Content for the legend of the overview density plot
  displayed on every main results page
}
\author{
  Thomas Sandmann
}

