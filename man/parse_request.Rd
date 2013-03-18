\name{parse_request}
\alias{parse_request}
\title{POST request parser}
\usage{
  parse_request(request, conf_data,
    cmaps.concatenated.by = getOption("cmaps.concatenated.by", default = ","))
}
\arguments{
  \item{request}{Request object as defined by the Rook
  package}

  \item{conf_data}{list, the configuration data as returned
  by the read_config_file function}

  \item{cmaps.concatenated.by}{character, specifying the
  separator (e.g. semicolon, comma) used to concatenate
  multiple requested reference databases}
}
\value{
  list, all elements from POST request, decoded if
  necessary
}
\description{
  This function parses the a POST request and decodes the
  user-provided information
}
\author{
  Thomas Sandmann
}

