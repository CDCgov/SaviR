#' @title Aesthetics for WHO regions
#' @description Used internally in plotting functions to style plots with WHO-regions.
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#'   \item{\code{cat_values}}{character plot legend values (which match directly to WHO region names)}
#'   \item{\code{cat_names}}{character plot legend names (which are displayed on the plot legend)}
#'   \item{\code{cat_colors}}{character Color mappings for each WHO region}
#'   \item{\code{cat_lines}}{character Line-styles for each WHO region}
#' }
who_aes <- data.frame(
  cat_values = c("AMRO", "EURO", "SEARO", "EMRO", "AFRO", "WPRO"),
  cat_names = c("Americas", "Europe", "Southeast Asia", "Eastern Mediterranean", "Africa", "Western Pacific"),
  cat_colors = c("#aa001e", "#e7b351", "#00818a", "#d26230", "#005e70", "#9c4f9f"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid", "solid")
)

#' @title Aesthetics for US Department of State regions
#' @description Used internally in plotting functions to style plots with DoS-regions.
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{\code{cat_values}}{character plot legend values (which match directly to DoS region names)}
#'   \item{\code{cat_names}}{character plot legend names (which are displayed on the plot legend)}
#'   \item{\code{cat_colors}}{character Color mappings for each state region}
#'   \item{\code{cat_lines}}{character Line-styles for each state region}
#' }
state_aes <- data.frame(
  cat_values = c(
    "East Asia and the Pacific",
    "Europe and Eurasia",
    "Near East (Middle East and Northern Africa)",
    "South and Central Asia",
    "Sub-Saharan Africa",
    "Western Hemisphere",
    "US",
    "None-state"
  ),
  cat_names = c(
    "East Asia and the Pacific",
    "Europe and Eurasia",
    "Near East (Middle East and North Africa)",
    "South and Central Asia",
    "Sub-Saharan Africa",
    "Western Hemisphere (not incl US)",
    "US",
    "None-state"
  ),
  cat_colors = c("#d00000", "#ffba08", "#3f88c5", "#032b43", "#136f63", "#a5c651", "#d64550", "#808080"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid", "solid", "dashed", "solid")
)

#' @title Aesthetics for World Bank Country income group
#' @description Used internally in plotting functions to style plots with World Bank income group.
#' @format A data frame with 5 rows and 4 variables:
#' \describe{
#'   \item{\code{cat_values}}{character plot legend values (which match directly to World Bank income levels)}
#'   \item{\code{cat_names}}{character plot legend names (which are displayed on the plot legend)}
#'   \item{\code{cat_colors}}{character Color mappings for income group level}
#'   \item{\code{cat_lines}}{character Line-styles for each income group level}
#' }
income_aes <- data.frame(
  cat_values = c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"),
  cat_names = c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"),
  cat_colors = c("#045a8d", "#74a9cf", "#fdbb84", "#d7301f", "#808080"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid")
)

#' OWID location renamings
owid_lk <- c(
  "United Kingdom"            = "The United Kingdom",
  "Syria"                     = "Syrian Arab Republic",
  "South Korea"               = "Republic of Korea",
  "Sint Maarten (Dutch part)" = "Sint Maarten",
  "Russia"                    = "Russian Federation",
  "Pitcairn"                  = "Pitcairn Islands",
  "Moldova"                   = "Republic of Moldova",
  "Macao"                     = "Macau",
  "Laos"                      = "Lao People's Democratic Republic",
  "Iran"                      = "Iran (Islamic Republic of)",
  "Curacao"                   = "Curaçao",
  "Cape Verde"                = "Cabo Verde"
)

#' WHO Country renamings / rebinnings
who_lk <- c(
  "Kosovo[1]" = "Kosovo",
  "Bonaire" = "Bonaire, Sint Eustatius, and Saba",
  "Sint Eustatius" = "Bonaire, Sint Eustatius, and Saba",
  "Saba" = "Bonaire, Sint Eustatius, and Saba",
  "Côte d\u2019Ivoire" = "Cote d'Ivoire",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo DR",
  "Falkland Islands (Malvinas)" = "Falkland Islands",
  "Iran (Islamic Republic of)" = "Iran",
  "Democratic People's Republic of Korea" = "Korea (North)",
  "Lao People's Democratic Republic" = "Laos",
  "Micronesia (Federated States of)" = "Micronesia",
  "Northern Mariana Islands (Commonwealth of the)" = "Northern Mariana Islands",
  "occupied Palestinian territory, including east Jerusalem" = "Palestinian Territory",
  "Myanmar" = "Burma",
  "Republic of Korea" = "Korea (South)",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Syrian Arab Republic" = "Syria",
  "United Republic of Tanzania" = "Tanzania",
  "United States Virgin Islands" = "Virgin Islands, US",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "The United Kingdom" = "United Kingdom"
)

#' A list of all data sources used in the package
#' to be updated as needed.
datasource_lk <- list(
  # OWID cases and deaths
  owid_all = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
  # OWID Testing dataset
  owid_testing = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv",
  # Testing data and metadata from FIND
  find_testing = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv",
  find_metadata = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/unit_info.csv",
  # OWID vaccination data
  owid_vax = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv",
  # OWID vaccination metadata that lists current vaccines in use by country
  owid_vax_manufacturers = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv",
  # WHO Case and Death data
  who_all = "https://covid19.who.int/WHO-COVID-19-global-data.csv",
  # Cases and Deaths from JHU (for HK, Macau, Taiwan)
  jhu_case = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  jhu_death = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
)