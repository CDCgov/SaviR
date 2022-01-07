#' @title Onetable
#' @description A centralized metadata table containing all country ids, categories, and population counts
#' @format A data frame with 237 rows and 10 variables:
#' \itemize{
#'   \item{\code{id}}{character ISO 3166-1 alpha-3 country code}
#'   \item{\code{iso2code}}{character ISO 3166-1 alpha-2 country code}
#'   \item{\code{state_region}}{character US Department of State Region}
#'   \item{\code{who_region}}{character WHO Region acronym}
#'   \item{\code{who_region_desc}}{character WHO Region english name}
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

#' @title OWID Testing Metadata
#' @describeIn get_owid_testing_meta Saved dataset
#' @description A metadata table used within [get_testing()] to determine how country testing data is computed.
#' @format A data frame with 141 rows and 5 variables:
#' \itemize{
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{test_definition}}{  character Description of OWID testing definition}
#'   \item{\code{case_definition}}{  character Description of OWID case definition}
#'   \item{\code{posrate_definition}}{  character Description of OWID test positivity calculation}
#'   \item{\code{posrate_direct}}{  logical Indicator for whether OWID pulls test positivity directly, or computes it}
#' }
"owid_testing_meta"

#' Additional entries not acknowledged by WHO
#' but required to create onetable
onetable_addn_countries <- data.frame(
  iso2code = c("HK", "MO", "TW"),
  country = c("Hong Kong", "Macau", "Taiwan"),
  who_region = c("WPRO", "WPRO", "WPRO")
)

#' Countries with no current UNWPP data
#' that are manually updated here from CIA world factbook
#' https://www.cia.gov/the-world-factbook/field/population/country-comparison
#' Currently 2021 projections are being used
cia_wfb_addn_countries <- data.frame(
  country = c("Guernsey", "Jersey", "Pitcairn Islands", "Kosovo"),
  id = c("GGY", "JEY", "PCN", "XKX"),
  total = c(67334, 101476, 50, 1935259)
)
