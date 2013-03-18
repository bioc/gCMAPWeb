\name{stripPrefix}
\alias{stripPrefix}
\title{Strip prefix from gene identifier names}
\usage{
  stripPrefix(ids, prefix = c("^GeneID:", "^GeneID"))
}
\arguments{
  \item{ids}{character, vector of gene identifiers}

  \item{prefix}{character, one or more strings to be
  removed}
}
\value{
  character, gene identifiers without prefix
}
\description{
  This function strips a prefix from gene identifier
  strings
}
\author{
  Thomas Sandmann
}

