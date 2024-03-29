% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{country_coords}
\alias{country_coords}
\title{Country Coordinates}
\format{
A simple feature collection with 172 features and 4 variables:
\describe{
\item{\code{TYPE}}{  character Entity type}
\item{\code{ADMIN}}{  character English country name}
\item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
\item{\code{geometry}}{  list List of simple features for mapping}
}
}
\usage{
country_coords
}
\description{
A saved shapefile of the world for use in onetable, and for mapping.
}
\details{
Projected using CRS: "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

Northern Cyprus, Antarctica, and Fiji are removed
}
\seealso{
\code{\link[=get_country_coords]{get_country_coords()}} for generation
}
\keyword{datasets}
