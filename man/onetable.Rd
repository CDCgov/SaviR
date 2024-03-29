% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{onetable}
\alias{onetable}
\title{Onetable}
\format{
A data frame with 237 rows and 10 columns, including:
\itemize{
\item{\code{id}}{character ISO 3166-1 alpha-3 country code}
\item{\code{iso2code}}{character ISO 3166-1 alpha-2 country code}
\item{\code{state_region}}{character US Department of State Region}
\item{\code{who_region}}{character WHO Region acronym}
\item{\code{who_region_desc}}{character WHO Region english name}
\item{\code{who_country}}{character WHO English country text name}
\item{\code{incomelevel_value}}{character World Bank Income level}
\item{\code{population}}{double UN Total population estimates for 2020}
\item{\code{eighteenplus}}{double UN 18+ population estimates for 2020}
\item{\code{geometry}}{list List of simple features for mapping}
}
}
\usage{
onetable
}
\description{
A centralized metadata table containing all country ids, categories, and population counts
}
\section{Notes}{

Population figures for Pitcairn Islands augmented from CIA World Factbook.
}

\section{Sources}{

\itemize{
\item https://population.un.org/wpp/Download/Files/1_Indicators\%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx
\item https://population.un.org/wpp/Download/Files/1_Indicators\%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.csv
\item https://population.un.org/wpp/Download/Files/1_Indicators\%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx
\item https://www.cia.gov/the-world-factbook/field/population/country-comparison
\item https://worldbank.org
}
}

\keyword{datasets}
