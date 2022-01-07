# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax
#' @description Get vaccination data from OWID
#' Note that all cumulative totals are all carried forward if NA on a given day.
#'
#' @return A data frame with n rows and 17 variables:
#'
#' \describe{
#'   \item{\code{owid_country}}{  character English country name from OWID}
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{date}}{  date Date of vaccination observation}
#'   \item{\code{total_vaccinations}}{  double total number of doses administered. For vaccines that require multiple doses, each individual dose is counted. If a person receives one dose of the vaccine, this metric goes up by 1. If they receive a second dose, it goes up by 1 again. If they receive a third/booster dose, it goes up by 1 again.}
#'   \item{\code{people_vaccinated}}{  double total number of people who received at least one vaccine dose. If a person receives the first dose of a 2-dose vaccine, this metric goes up by 1. If they receive the second dose, the metric stays the same.}
#'   \item{\code{people_fully_vaccinated}}{  double total number of people who received all doses prescribed by the vaccination protocol. If a person receives the first dose of a 2-dose vaccine, this metric stays the same. If they receive the second dose, the metric goes up by 1.}
#'   \item{\code{total_boosters}}{  integer total number of COVID-19 vaccination booster doses administered (doses administered beyond the number prescribed by the vaccination protocol)}
#'   \item{\code{daily_vaccinations_raw}}{  integer daily change in the total number of doses administered. It is only calculated for consecutive days. This is a raw measure provided for data checks and transparency, but we strongly recommend that any analysis on daily vaccination rates be conducted using daily_vaccinations instead.}
#'   \item{\code{daily_vaccinations}}{  integer new doses administered per day (7-day smoothed). For countries that don't report data on a daily basis, we assume that doses changed equally on a daily basis over any periods in which no data was reported. This produces a complete series of daily figures, which is then averaged over a rolling 7-day window.}
#'   \item{\code{total_vaccinations_per_hundred}}{  double total_vaccinations per 100 people in the total population of the country.}
#'   \item{\code{people_vaccinated_per_hundred}}{  double people_vaccinated per 100 people in the total population of the country.}
#'   \item{\code{people_fully_vaccinated_per_hundred}}{  double people_fully_vaccinated per 100 people in the total population of the country.}
#'   \item{\code{total_boosters_per_hundred}}{  double Total number of COVID-19 vaccination booster doses administered per 100 people in the total population.}
#'   \item{\code{daily_vaccinations_per_million}}{  integer daily_vaccinations per 1,000,000 people in the total population of the country.}
#'   \item{\code{daily_people_vaccinated}}{  integer daily change in number of people who received at least one vaccine dose.}
#'   \item{\code{daily_people_vaccinated_per_hundred}}{  double daily_people_vaccinated per 100 people in the total population of the country.}
#'   \item{\code{daily_vaccinations_per_hundred}}{  double daily_vaccinations per 100 people in the total population of the country.}
#' }
#'
#' @importFrom bit64 is.integer64
#' @export
#'

get_vax <- function() {
  df <- fread(datasource_lk$owid_vax, stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename(id = iso_code, owid_country = location) %>%
    mutate(date = as.Date(date)) %>%
    mutate(id = recode(id, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", id)) %>%
    mutate(owid_country = recode(owid_country, !!!owid_lk)) %>%
    calc_vax_carryforward() %>%
    mutate(daily_vaccinations_per_hundred = daily_vaccinations_per_million / 10000) %>%
    mutate(across(where(bit64::is.integer64), as.double))

  return(df)
}

#' @title Get last non-NA date for key vaccination metrics by country
#' @description Computes the latest date of vaccination data provided by each country from OWID source.
#'
#' @return a data frame with n rows and 5 columns
#' \describe{
#'   \item{\code{owid_country}}{  character English country name from OWID (may not match WHO country name)}
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{total_doses_date}}{double Date of last update for total vaccine doses}
#'   \item{\code{partial_date}}{double Date of last update for persons vaccinated}
#'   \item{\code{fully_date}}{double Date of last update for persons fully vaccinated}
#' }
#' @seealso [get_vax()] for full vaccination data from the same source
#' @export
get_vax_dates <- function() {
  df <- fread(datasource_lk$owid_vax, stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename(id = iso_code, owid_country = location) %>%
    mutate(date = as.Date(date)) %>%
    mutate(id = recode(id, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", id)) %>%
    mutate(owid_country = recode(owid_country, !!!owid_lk))

  # Note: I could have written a subfunction here, but I was lazy,
  # and it's plenty fast anyways
  total_dates <- df %>%
    filter(!is.na(total_vaccinations_per_hundred)) %>%
    group_by(id) %>%
    summarize(total_doses_date = max(date)) %>%
    ungroup()

  partial_dates <- df %>%
    filter(!is.na(people_vaccinated_per_hundred)) %>%
    group_by(id) %>%
    summarize(partial_date = max(date)) %>%
    ungroup()

  fully_dates <- df %>%
    filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
    group_by(id) %>%
    summarize(fully_date = max(date)) %>%
    ungroup()

  out <- df %>%
    distinct(owid_country, id) %>%
    full_join(total_dates, by = "id") %>%
    full_join(partial_dates, by = "id") %>%
    full_join(fully_dates, by = "id")

  return(out)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax_manufacturers
#' @description Get vaccination metadata from OWID
#'
#' @return A data frame with 218(-ish) rows and 6 variables:
#'
#' \itemize{
#'   \item{\code{owid_country}}{  character English country name from OWID}
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{vaccines}}{  character list of vaccines administered in the country up to the current date.}
#'   \item{\code{last_observation_date}}{  date date of the last observation in OWID data.}
#'   \item{\code{source_name}}{  character name of OWID source for data collection.}
#'   \item{\code{source_website}}{  character web location of OWID source. It can be a standard URL if numbers are consistently reported on a given page; otherwise it will be the source for the last data point.}
#' }

#'
#' @export
#'

get_vax_manufacturers <- function() {
  df <- fread(datasource_lk$owid_vax_manufacturers, stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename(id = iso_code, owid_country = location) %>%
    mutate(last_observation_date = as.Date(last_observation_date)) %>%
    mutate(id = recode(id, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", id)) %>%
    mutate(owid_country = recode(owid_country, !!!owid_lk))

  return(df)
}
