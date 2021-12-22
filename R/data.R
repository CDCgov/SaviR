#' @title Onetable
#' @description A centralized metadata table containing all country ids, categories, and population counts
#' @format A data frame with 238 rows and 9 variables:
#' \itemize{
#'   \item{\code{iso3code}}{character ISO 3166-1 alpha-3 country code}
#'   \item{\code{iso2code}}{character ISO 3166-1 alpha-2 country code}
#'   \item{\code{state_region}}{character US Department of State Region}
#'   \item{\code{who_region}}{integer WHO Region}
#'   \item{\code{who_country}}{character WHO English country text name}
#'   \item{\code{incomelevel_value}}{character World Bank Income level}
#'   \item{\code{population}}{double UN Total population estimates for 2020}
#'   \item{\code{eighteenplus}}{double UN 18+ population estimates for 2020}
#'   \item{\code{geometry}}{list List of simple features for mapping}
#' }
#' @section Notes:
#' Population figures for Guernsey, Jersey, Pitcairn Islands, and Kosovo augmented from CIA World Factbook.
#'
#' @section Sources:
#' \itemize{
#'   \item{https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX}{}
#'   \item{https://population.un.org/wpp/Download/Files/1_Indicators\%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx}{}
#'   \item{https://population.un.org/wpp/Download/Files/1_Indicators\%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx}{}
#'   \item{https://worldbank.org}{}
#' }
#'
"onetable"