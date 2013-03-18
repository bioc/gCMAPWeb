\name{addLinks}
\alias{addLinks}
\title{Adding links to columns of a data.frame}
\usage{
  addLinks(df, pattern = ".url$",
    swap.colnames = getOption("swap.colnames", default = list(padj = "FDR", nFound = "Genes")))
}
\arguments{
  \item{df}{data.frame, must contain both target and url
  columns}

  \item{pattern}{character, the suffix linking target and
  url columns. Default=c('.url')}

  \item{swap.colnames}{list, containing alternative names
  for CMAPResults columns.}
}
\value{
  Data frame with href html tags in the target column(s)
}
\description{
  This function identifies column pairs from a data.frame
  that differ only in a string suffix (default: .url). The
  column with the suffix is used to add http href tags to
  the other column and is then removed.
}
\details{
  To annotate columns with links to other urls, the urls
  must be included in the data.frame in a separate column
  matching the target column name but carrying the
  additional suffix in the column name. For example, to add
  links to the 'id' column, an additional id.url column can
  be used. Multiple columns can be processed
  simultaneously, if each of them has a matching url
  column.
}
\author{
  Thomas Sandmann
}

