% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vaccinations.R
\name{get_vax}
\alias{get_vax}
\title{get_vax}
\usage{
get_vax()
}
\value{
Returns a data frame with n rows and 17 columns, including:

\describe{
\item{\code{owid_country}}{  character English country name from OWID}
\item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
\item{\code{date}}{  date Date of vaccination observation}
\item{\code{total_vaccinations}}{  double total number of doses administered. For vaccines that require multiple doses, each individual dose is counted. If a person receives one dose of the vaccine, this metric goes up by 1. If they receive a second dose, it goes up by 1 again. If they receive a third/booster dose, it goes up by 1 again.}
\item{\code{people_vaccinated}}{  double total number of people who received at least one vaccine dose. If a person receives the first dose of a 2-dose vaccine, this metric goes up by 1. If they receive the second dose, the metric stays the same.}
\item{\code{people_fully_vaccinated}}{  double total number of people who received all doses prescribed by the vaccination protocol. If a person receives the first dose of a 2-dose vaccine, this metric stays the same. If they receive the second dose, the metric goes up by 1.}
\item{\code{total_boosters}}{  integer total number of COVID-19 vaccination booster doses administered (doses administered beyond the number prescribed by the vaccination protocol)}
\item{\code{daily_vaccinations_raw}}{  integer daily change in the total number of doses administered. It is only calculated for consecutive days. This is a raw measure provided for data checks and transparency, but we strongly recommend that any analysis on daily vaccination rates be conducted using daily_vaccinations instead.}
\item{\code{daily_vaccinations}}{  integer new doses administered per day (7-day smoothed). For countries that don't report data on a daily basis, we assume that doses changed equally on a daily basis over any periods in which no data was reported. This produces a complete series of daily figures, which is then averaged over a rolling 7-day window.}
\item{\code{total_vaccinations_per_hundred}}{  double total_vaccinations per 100 people in the total population of the country.}
\item{\code{people_vaccinated_per_hundred}}{  double people_vaccinated per 100 people in the total population of the country.}
\item{\code{people_fully_vaccinated_per_hundred}}{  double people_fully_vaccinated per 100 people in the total population of the country.}
\item{\code{total_boosters_per_hundred}}{  double Total number of COVID-19 vaccination booster doses administered per 100 people in the total population.}
\item{\code{daily_vaccinations_per_million}}{  integer daily_vaccinations per 1,000,000 people in the total population of the country.}
\item{\code{daily_people_vaccinated}}{  integer daily change in number of people who received at least one vaccine dose.}
\item{\code{daily_people_vaccinated_per_hundred}}{  double daily_people_vaccinated per 100 people in the total population of the country.}
\item{\code{daily_vaccinations_per_hundred}}{  double daily_vaccinations per 100 people in the total population of the country.}
}
}
\description{
Get vaccination data from Our World in Data. (OWID)
Note that all cumulative totals are all carried forward if NA on a given day.
}
