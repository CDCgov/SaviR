% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_covid_sources.R
\name{get_covid_df}
\alias{get_covid_df}
\title{get_covid_df}
\usage{
get_covid_df(sources = c("all", "WHO", "WHO+JHU", "WHO+Primary"))
}
\arguments{
\item{sources}{one of "all", "WHO", "WHO+JHU", "WHO+Primary" specifying the data sources to pull from. See details.}
}
\value{
Returns a data frame with n rows and 8 columns, including:
\itemize{
\item{\code{date}}{  date Date of observation}
\item{\code{iso2code}}{  character ISO 3166-1 alpha-2 country code}
\item{\code{country}}{  character WHO english country name}
\item{\code{new_cases}}{  integer Number of new cases reported on date}
\item{\code{cumulative_cases}}{  integer Number of cumulative cases to date}
\item{\code{new_deaths}}{  integer Number of new deaths reported on date}
\item{\code{cumulative_deaths}}{  integer Number of cumulative deaths to date}
\item{\code{source}}{  character Data Source}
}
}
\description{
Get and prepare COVID data.

Pull in current case and death counts from WHO source.
For disaggregated China, Taiwan, Hong Kong, and Macau data we pull from JHU or primary sources.
}
\details{
In legacy versions, the default was to pull "all" sources, which included the WHO case/death time-series and JHU data for China Mainland, HK, Macau, and Taiwan.
Due to sun-setting and changes in reporting, we now capture HK and Taiwan data from primary sources ("WHO+Primary"). Note that this also includes JHU data on Macau
which will be reported thru Mar 10, 2023 when JHU closes their dashboard.
}
