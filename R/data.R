#' @title Onetable
#' @description A centralized metadata table containing all country ids, categories, and population counts
#' @format A data frame with 237 rows and 10 columns, including:
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
#' Population figures for Pitcairn Islands augmented from CIA World Factbook.
#'
#' @section Sources:
#'   * https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx
#'   * https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.csv
#'   * https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx
#'   * https://www.cia.gov/the-world-factbook/field/population/country-comparison
#'   * https://worldbank.org
#'
#' @md
"onetable"

#' @title Our World in Data (OWID) Testing Metadata
#' @describeIn get_owid_testing_meta Saved dataset
#' @description A metadata table used within [get_testing()] to determine how country testing data is computed.
#' @format A data frame with 141 rows and 5 columns, including:
#' \itemize{
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{test_definition}}{  character Description of OWID testing definition}
#'   \item{\code{case_definition}}{  character Description of OWID case definition}
#'   \item{\code{posrate_definition}}{  character Description of OWID test positivity calculation}
#'   \item{\code{posrate_direct}}{  logical Indicator for whether OWID pulls test positivity directly, or computes it}
#' }
"owid_testing_meta"

#' @title Country Coordinates
#' @description A saved shapefile of the world for use in onetable, and for mapping.
#'
#' @format A simple feature collection with 172 features and 4 variables:
#' \describe{
#'   \item{\code{TYPE}}{  character Entity type}
#'   \item{\code{ADMIN}}{  character English country name}
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{geometry}}{  list List of simple features for mapping}
#'}
#' @details
#' Projected using CRS: "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#'
#' Northern Cyprus, Antarctica, and Fiji are removed
#' @seealso [get_country_coords()] for generation
"country_coords"

#' Additional entries not acknowledged by WHO
#' but required to create onetable
onetable_addn_countries <- data.frame(
  iso2code = c("HK", "MO", "TW"),
  country = c("Hong Kong", "Macau", "Taiwan"),
  who_region = c("WPRO", "WPRO", "WPRO")
)

#' Countries with no current UNWPP data
#' that are manually updated here from CIA World Factbook
#' https://www.cia.gov/the-world-factbook/field/population/country-comparison
#' Currently 2022 estimates are being used
cia_wfb_addn_countries <- data.frame(
  country = c("Pitcairn Islands"),
  id = c("PCN"),
  total = c(50)
)
