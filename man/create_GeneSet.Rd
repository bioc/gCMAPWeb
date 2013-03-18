\name{create_GeneSet}
\alias{create_GeneSet}
\title{GeneSet creator}
\usage{
  create_GeneSet(post, conf_data)
}
\arguments{
  \item{post}{list, POST component of the Rook request}

  \item{conf_data}{list, the configuration data as returned
  by the read_config_file function}
}
\value{
  GeneSet object
}
\description{
  This function creates a GeneSet object from
  user-specified identifiers
}
\author{
  Thomas Sandmann
}

