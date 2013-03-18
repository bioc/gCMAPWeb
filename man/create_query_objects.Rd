\name{create_query_objects}
\alias{create_query_objects}
\title{Query object creator}
\usage{
  create_query_objects(post, conf_data)
}
\arguments{
  \item{post}{list, POST component of the Rook request}

  \item{conf_data}{list, the configuration data as returned
  by the read_config_file function}
}
\value{
  one of GeneSet, SignedGeneSet or ExpressionSet
}
\description{
  This function generates an appropriate R data object from
  the user query
}
\author{
  Thomas Sandmann
}

