% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractValues.R
\name{extractValues}
\alias{extractValues}
\title{extractValues}
\usage{
extractValues(
  path,
  occ,
  location = "species",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  original_format = ".asc"
)
}
\arguments{
\item{path}{character, the path to the folder containing the variables.}

\item{occ}{table containing columns with the species name, longitude, and
latitude.}

\item{location}{character, name of the column containing location IDs,
default is "species".}

\item{lon}{character, name of the longitude column, default is "default is
"decimalLongitude".}

\item{lat}{character, name of the latitude column, default is default is
"decimalLatitude".}

\item{original_format}{character, indicates the format of the layers.}
}
\value{
This function returns the values for variables in each point.
}
\description{
Extracts bioclimatic data from WorldClim layers for data points.
}
