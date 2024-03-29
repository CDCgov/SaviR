#' @title Aesthetics for WHO regions
#' @description Used internally in plotting functions to style plots with WHO-regions.
#' @returns
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

#' @title WHO "pretty" english region names
#' @description A helper lookup table to map WHO region acronyms to a "pretty" english name.
#' @format A character vector of length 6:
#' \describe{
#'   \item{\code{AMRO}}{character Americas}
#'   \item{\code{EURO}}{character Europe}
#'   \item{\code{SEARO}}{character Southeast Asia}
#'   \item{\code{EMRO}}{character Eastern Mediterranean}
#'   \item{\code{AFRO}}{character Africa}
#'   \item{\code{WPRO}}{character Western Pacific}
#' }
#' @details For internal use in generating [onetable] via [get_onetable()]
who_region_lk <- c(
  AMRO = "Americas",
  EURO = "Europe",
  SEARO = "Southeast Asia",
  EMRO = "Eastern Mediterranean",
  AFRO = "Africa",
  WPRO = "Western Pacific"
)

#' @title Aesthetics for US Department of State regions
#' @description Used internally in plotting functions to style plots with DoS-regions.
#' @returns
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
#' @returns
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

#' Manual ISO 3166-1 alpha-3 country code entries that don't get parsed in get_onetable()
manual_iso3_lk <- list(
  Micronesia = "FSM",
  `Saint Martin` = "MAF",
  `Eswatini` = "SWZ"
)

#' A list of all data sources used in the package
#' to be updated as needed.
datasource_lk <- list(
  # OWID cases and deaths
  # SB Note: Beginning Mar 8, 2023 OWID has ceased pulling from JHU
  # and JHU will cease operations itself on Mar 10, 2023. This will contain legacy data
  owid_all = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data-old.csv",
  # HK Cases and Deaths
  hk_case_deaths = "http://www.chp.gov.hk/files/misc/latest_situation_of_reported_cases_covid_19_eng.csv",
  # Taiwan Cases and Deaths
  taiwan_cases = "https://data.cdc.gov.tw/en/download?resourceid=a65c7cb5-8a3c-4859-a27a-9019f65dd66e&dataurl=https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv",
  taiwan_deaths = "https://data.cdc.gov.tw/en/download?resourceid=a12dfeba-0dea-4b3f-b1b0-1bf3524b3ca9&dataurl=https://od.cdc.gov.tw/eic/open_data_death_date_statistics_19CoV_5.csv",
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
  jhu_death = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
  # World Bank country income classification
  wb_income = "http://api.worldbank.org/v2/country?format=json&per_page=300",
  # UN World Population Projections (UNWPP)
  # Location metadata
  un_location_meta = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  # Total country population projections
  un_overall_projections = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.csv",
  # Country population projections by age group
  un_age_projections = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F03_1_POPULATION_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx",
  owid_hosp = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/hospitalizations/covid-hospitalizations.csv",
  ecdc_hosp = "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv",
  owid_vax_by_type = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv"
)

#' @title Function to get bounding boxes for maps by WHO/DoS region
#' @description Used internally in viz_maps.R functions for maps
#' @returns a vector of class bbox (from the sf package)
#'  with the min and max xy coordinates of the bounding box
#' @keywords internal
bbox_fun <- function(region, df) {
  check_region(region)
  
  switch(region,
        `Europe and Eurasia` = sf::st_bbox(c(xmin = -1400000, ymin = 200000, xmax = 6500000, ymax = 8200000)),
         EURO = sf::st_bbox(c(xmin = -1400000, ymin = 200000, xmax = 6500000, ymax = 8200000)),
         `Western Hemisphere` =,
         AMRO = sf::st_bbox(c(xmin = -14300000, ymin = -5500074, xmax = -3872374, ymax = 5000000)),
         SEARO = sf::st_bbox(c(xmin = 6484395, ymin = -2008021, xmax = 12915540, ymax = 4596098)),
        `Near East (Middle East and Northern Africa)` = sf::st_bbox(c(xmin = -1300000, ymin = 100000, xmax = 4900000, ymax = 6000000)),
         EMRO = sf::st_bbox(c(xmin = -1600000, ymin = -1800026.8, xmax = 6418436.7, ymax = 6245846.3)),
        `Sub-Saharan Africa` =,
         AFRO = sf::st_bbox(c(xmin = -2400000, ymin = -4200074, xmax = 6000000, ymax = 4218372)),
         `East Asia and the Pacific` =,
         WPRO = sf::st_bbox(c(xmin = 5884395, ymin = -5308021, xmax = 16500000, ymax = 5396098)),
         US =,
        `South and Central Asia` =,
         sf::st_bbox(sf::st_as_sf(df))
  )
}

# A dynamic lookup of WHO + DoS Regions
known_regions <- c(
  names(who_region_lk),
  state_aes$cat_values
)

# A quick helper to check if the region passed is known
check_region <- function(x) {
  if (!x %in% known_regions) {
    warning(
      sprintf(
        r"{Region "%s" supplied not recognized, defaulting to standard bbox}",
        x
      ),
      immediate. = TRUE
    )
  }
}