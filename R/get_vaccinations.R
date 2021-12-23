# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax
#' @description Get vaccinations from OWID
#' @importFrom bit64 is.integer64
#' @export
#'

get_vax <- function() {
  df <- fread(datasource_lk$owid_vax, stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename(iso3code = iso_code, owid_country = location) %>%
    mutate(date = as.Date(date)) %>%
    mutate(iso3code = recode(iso3code, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", iso3code)) %>%
    mutate(owid_country = recode(owid_country, !!!owid_lk)) %>%
    mutate(daily_vaccinations_per_hundred = daily_vaccinations_per_million / 10000) %>%
    mutate(across(where(bit64::is.integer64), as.double))

  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax_manufacturers
#' @description Get vaccinations from OWID
#'

#'
#' @export
#'

get_vax_manufacturers <- function() {
  df <- fread(datasource_lk$owid_vax_manufacturers, stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename(iso3code = iso_code, owid_country = location) %>%
    mutate(last_observation_date = as.Date(last_observation_date)) %>%
    mutate(iso3code = recode(iso3code, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", iso3code)) %>%
    mutate(owid_country = recode(owid_country, !!!owid_lk))

  return(df)
}