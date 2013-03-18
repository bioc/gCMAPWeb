\name{index_quote}
\alias{index_quote}
\title{Html code for quote on the index page}
\usage{
  index_quote(text = getOption("index.quote", default = "<p>What if at least some parts of [ the laborious screening of genetic or chemical libraries ] could be systematized and centralized? <br>And hits found and hypotheses generated with something resembling an internet search engine? <br>These are the questions the Connectivity Map project set out to answer.</p><small>Justin Lamb in <cite title='Source Title'>Nature Reviews Cancer 7, 54-60 (January 2007)</cite></small></blockquote>"))
}
\arguments{
  \item{text,}{Character, the content of the blockquote
  field on the index.rhtml page. Can be set as the global
  variabel "index.quote".}
}
\value{
  Character string with html code
}
\description{
  Html code for quote on the index page
}
\author{
  Thomas Sandmann
}

