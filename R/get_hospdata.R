
#' Get hospitalization data from both OWID and ECDC
#'
#' Data are pulled in from both [OWID](https://github.com/owid/covid-19-data/tree/master/public/data/hospitalizations) and ECDC.
#' and [ECDC](https://www.ecdc.europa.eu/en/publications-data/download-data-hospital-and-icu-admission-rates-and-current-occupancy-covid-19)
#' for countries in the EU/EAA.
#' See [here](https://github.com/owid/covid-19-data/blob/master/public/data/hospitalizations/locations.csv) for
#' more information on where OWID hospitalization data comes from by country. See [this
#' document](https://www.ecdc.europa.eu/sites/default/files/documents/2021-01-13_Variable_Dictionary_and_Disclaimer_hosp_icu_all_data.pdf)
#' for more details on the ECDC data.
#'
#' @return A data frame with n rows and 5 columns:
#'
#' \describe{
#'       \item{id}{character, ISO 3166-1 alpha-3 country code}
#'       \item{indicator}{character, one of: "Daily hospital occupancy",
#'       "Daily ICU occupancy", "Weekly new hospital admissions per 100k",
#'       "Weekly new ICU admissions per 100k", "Daily ICU occupancy",
#'       "Daily ICU occupancy per million", "Daily hospital occupancy",
#'       "Daily hospital occupancy per million", "Weekly new hospital admissions",
#'       "Weekly new hospital admissions per million", "Weekly new ICU admissions",
#'       "Weekly new ICU admissions per million". See \code{\link{get_hospdata_wide}} for more details
#'       on each metric.}
#'       \item{source}{character, ECDC or OWID, see details for more information.}
#'       \item{date}{date of observation}
#'       \item{value}{value of observation for each indicator}
#'       \item{carry_fwd_value}{when data is missing for a given date,
#'             if there is a non-missing value for that indicator-source-country
#'             combination within the previous 14 days, this column is that value
#'             carried forward.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'   hospdata_long <- get_hospdata_long()
#' }
get_hospdata_long <- function() {

  # read in data
  owid_data <-
    fread(datasource_lk$owid_hosp, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
    select(-entity) %>%
    mutate(source = "OWID")

  ecdc_data <-
    fread(datasource_lk$ecdc_hosp, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
    mutate(iso_code = passport::parse_country(country, to = "iso3c")) %>%
    select(-source, -url, -year_week, -country) %>%
    mutate(source = "ECDC")


  # combine sources
  hospdata <- bind_rows(ecdc_data, owid_data)

  # complete the data
  # date seq
  all_dates <- seq.Date(
    from = as.Date(min(owid_data$date, ecdc_data$date)),
    to = as.Date(max(ecdc_data$date, owid_data$date)),
    by = "day"
  )

  # complete by date grouped within country/indicator/source
  hospdata_long <-
    hospdata %>%
    mutate(date = lubridate::ymd(date)) %>%
    group_by(iso_code, indicator, source) %>% # for each indicator & source
    tidyr::complete(date = all_dates) %>% # complete by country and date
    mutate(carry_fwd_value = zoo::na.locf(value, na.rm = F, maxgap = 14)) %>%
    arrange(iso_code, date) %>%
    ungroup() %>%
    rename(id = iso_code)


  return(hospdata_long)
}

#' Get hospital data in a wide format where indicators are columns
#'
#' @param hospdata_long the hospital data in long format as generated
#'  by \code{\link{get_hospdata_long}}.
#' @param preferred_source character or character vector, either "OWID",
#'  "ECDC", or c("OWID", "ECDC") for both). Defaults to keeping both sources.
#'
#' @return
#' A data frame with n rows and up to 13 columns:
#'
#' \describe{
#'       \item{id}{character, ISO 3166-1 alpha-3 country code}
#'       \item{source}{character, ECDC or OWID, see details for more information.}
#'       \item{date}{date of observation}
#'       \item{daily_icu_occupancy}{OWID & ECDC: Number of COVID-19 patients in ICU on a given day}
#'       \item{daily_icu_occupancy_per_million}{OWID: daily_icu_occupancy per million people}
#'       \item{daily_hospital_occupancy}{OWID & ECDC: Number of COVID-19 patients in hospital on a given day}
#'       \item{daily_hospital_occupancy_per_million}{OWID: daily_hospital_occupancy per million people}
#'       \item{weekly_new_hospital_admissions}{OWID & ECDC: Number of COVID-19 patients newly admitted
#'             to hospitals in a given week}
#'       \item{weekly_new_hospital_admissions_per_100k}{ECDC: weekly_new_hospital_admissions from ECDC per 100,000 persons}
#'       \item{weekly_new_hospital_admissions_per_million}{OWID: weekly_new_hospital_admissions from OWID per million people}
#'       \item{weekly_new_icu_admissions}{OWID & ECDC: Number of COVID-19 patients newly admitted to ICU in a given week}
#'       \item{weekly_new_icu_admissions_per_million}{OWID: Weekly_new_icu_admissions from OWID per million people}
#'       \item{weekly_new_icu_admissions_per_100k}{ECDC: weekly_new_icu_admissions from ECDC per 100,000 persons}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  hospdata_long <- get_hospdata_long()
#'  hospdata_wide <- get_hospdata_wide(hospdata_long, preferred_source = "OWID")
#' }
#'
get_hospdata_wide <- function(hospdata_long,
                              preferred_source = c("OWID", "ECDC")) {
  preferred_source <- match.arg(preferred_source, several.ok = TRUE)

  hospdata_wide <-
    hospdata_long %>%
    filter(source %in% preferred_source) %>%
    select(-carry_fwd_value) %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    janitor::clean_names()

  return(hospdata_wide)
}

#' Get latest hospitalization data by country, indicator, and data source
#'
#' @inheritParams get_hospdata_wide
#'
#' @return The same output as \code{\link{get_hospdata_long}}, except with one
#'  row per country, indicator, and data source for the data from the most recent date
#'  (so dates may vary by country, source, and indicator), and no column for `carry_fwd_value`.
#' @export
#'
#' @examples
#' \dontrun{
#'   hospdata_long <- get_hospdata_long()
#'   hospdata_latest <- get_hospdata_latest(hospdata_long, preferred_source = "OWID")
#'   head(hospdata_latest)
#' }
get_hospdata_latest <- function(hospdata_long,
                                preferred_source = c("OWID", "ECDC")) {

  preferred_source <- match.arg(preferred_source, several.ok = TRUE)

  hospdata_latest <-
    hospdata_long %>%
    group_by(id, indicator, source) %>%
    select(-carry_fwd_value) %>%
    filter(!is.na(value),
           source %in% preferred_source) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()

  return(hospdata_latest)
}
