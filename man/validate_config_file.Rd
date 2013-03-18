\name{validate_config_file}
\alias{validate_config_file}
\title{Validation of gCMAPWeb configuration file}
\usage{
  validate_config_file(config.file.path)
}
\arguments{
  \item{config.file.path}{full path to gCMAPWeb
  configuration file (in yaml format)}
}
\value{
  Information from a valid configuration file is returned
  as a nested list. Invalid entries in the config file with
  cause an error.
}
\description{
  This function validates the content of the gCMAPWeb
  configuration file.
}
\details{
  Verifies that 1. all required fields are present 2. at
  least one species has been defined 3. all supported
  annotation packages are available 4. all cmaps have
  unique labels 5. each supported species has at least one
  associated reference dataset
}
\examples{
## read the example configuration yaml file without validation
conf1 <- yaml.load_file( system.file("config", "config.yml",
             package="gCMAPWeb") )
## read the example configuration file and validate that
## all required information is provided and valid
conf2 <- validate_config_file( system.file("config", "config.yml",
             package="gCMAPWeb") )
identical( conf1, conf2)
}
\author{
  Thomas Sandmann
}

