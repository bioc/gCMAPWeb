\name{create_tab}
\alias{create_tab}
\title{Create tab-delimited output}
\usage{
  create_tab(df, result.dir, url.base = NULL, file.name)
}
\arguments{
  \item{df}{data.frame}

  \item{result.dir}{character, path to the output
  directory}

  \item{url.base}{character, the name of the reference
  database}

  \item{file.name}{character, name of the report file}
}
\value{
  character string with the html code pointing to the
  download URL
}
\description{
  This function exports results in tab-delimited format
}
\author{
  Thomas Sandmann
}

