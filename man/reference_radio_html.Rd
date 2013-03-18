\name{reference_radio_html}
\alias{reference_radio_html}
\title{Html code for reference cmap radio button selectors}
\usage{
  reference_radio_html(conf_data, reference.cmaps)
}
\arguments{
  \item{conf_data}{list, the configuration data as returned
  by the read_config_file function}

  \item{reference.cmaps}{list containing all reference
  cmaps as eSet-like objects (e.g. NChannelSet,
  CMAPCollection)}
}
\value{
  Html code as character string.
}
\description{
  This function generates the html code required to display
  radio buttons for the reference datasets specified in the
  gCMAPWeb configuration file
}
\author{
  Thomas Sandmann
}

