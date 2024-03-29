% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{calc_window_incidence}
\alias{calc_window_incidence}
\title{Compute average incidence over a set day interval}
\usage{
calc_window_incidence(data, type = c("cases", "deaths"), window = 14)
}
\arguments{
\item{data}{a data.frame with required columns to compute the metric}

\item{type}{(character) one of cases or deaths, specifying the appropriate basis for the metric}

\item{window}{(numeric, default: 14) a numeric representing days to calculate the metric over}
}
\value{
a data.frame of summarized incidence values (ave_incidence) by date, possibly including grouping vars if data were grouped.
}
\description{
Compute average incidence over a set day interval
}
\details{
For \code{type} == "cases", data should contain at least date, new_cases, and population columns.
For \code{type} == "deaths", data should contain new_deaths instead.

Note that incidence here is per 100K population.
The function assumes that data passed has observations for each day for each country, since we use
and index-based approach to compute average incidence, not calendar-time.
}
\examples{
\dontrun{
data <- get_covid_df("WHO")
# 14d average incidence world-wide
calc_window_incidence(window = 14)
# For grouped operations, group data beforehand and pipe:
# 14d average incidence by country
data |>
  group_by(iso2code, country) |>
  calc_window_incidence(window = 14)
}

}
