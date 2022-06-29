
#' Get hospitalization data from both OWID and ECDC
#'
#' Data are pulled in from both [Our World in Data (OWID)](https://github.com/owid/covid-19-data/tree/master/public/data/hospitalizations)
#' and [European Centre for Disease Prevention and Control(ECDC)](https://www.ecdc.europa.eu/en/publications-data/download-data-hospital-and-icu-admission-rates-and-current-occupancy-covid-19)
#' for countries in the European Union/European Economic Area(EU/EEA).
#' See [here](https://github.com/owid/covid-19-data/blob/master/public/data/hospitalizations/locations.csv) for
#' more information on where OWID hospitalization data comes from by country. See [this
#' document](https://www.ecdc.europa.eu/sites/default/files/documents/2021-01-13_Variable_Dictionary_and_Disclaimer_hosp_icu_all_data.pdf)
#' for more details on the ECDC data.
#'
#' @return Returns a data frame with n rows and 6 columns, including:
#'
#' \describe{
#'       \item{id}{character, ISO 3166-1 alpha-3 country code}
#'       \item{indicator}{character, one of:
#'         \itemize{
#'             \item "Daily ICU occupancy" (OWID & ECDC)
#'             \item "Daily ICU occupancy per million" (OWID)
#'             \item "Daily hospital occupancy" (OWID & ECDC)
#'             \item "Daily hospital occupancy per million" (OWID)
#'             \item "Weekly new hospital admissions" (OWID)
#'             \item "Weekly new hospital admissions per 100k" (ECDC)
#'             \item "Weekly new hospital admissions per million" (OWID)
#'             \item "Weekly new ICU admissions" (OWID)
#'             \item "Weekly new ICU admissions per million" (OWID)
#'             \item "Weekly new ICU admissions per 100k" (ECDC)
#'            }
#'       }
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
#'
#'   # get the full hospital data
#'   hospdata <- get_hospdata()
#'
#'   # get the most recent non-missing data for each indicator by country
#'   # for a specific source
#'   hospdata %>%
#'     group_by(id, indicator, source) %>%
#'     select(-carry_fwd_value) %>%
#'     filter(!is.na(value),
#'            source %in% "OWID") %>%
#'     arrange(date) %>%
#'     slice_max(order_by = date, n = 1) %>%
#'     ungroup()
#'
#'   # to get in a wide format with indicators as columns
#'   hospdata %>%
#'     filter(source %in% "ECDC") %>%
#'     select(-carry_fwd_value) %>%
#'     tidyr::pivot_wider(names_from = indicator,
#'                        values_from = value)
#' }
#'
get_hospdata <- function() {

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
  hospdata <-
    hospdata %>%
    mutate(date = lubridate::ymd(date)) %>%
    group_by(iso_code, indicator, source) %>% # for each indicator & source
    tidyr::complete(date = all_dates) %>% # complete by country and date
    mutate(carry_fwd_value = zoo::na.locf(value, na.rm = F, maxgap = 14)) %>%
    arrange(iso_code, date) %>%
    ungroup() %>%
    rename(id = iso_code)


  return(hospdata)
}
