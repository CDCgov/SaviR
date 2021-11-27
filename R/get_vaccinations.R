# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax
#' @description Get vaccinations from OWID
#'
#' @export
#'

get_vax <- function() {
  df <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>%
    mutate(date = as.Date(date)) %>%
    mutate(iso_code = recode(iso_code, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", iso_code)) %>%
    mutate(location = recode(location, !!!owid_lk)) %>%
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
  df <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) %>%
    mutate(last_observation_date = as.Date(last_observation_date)) %>%
    mutate(iso_code = recode(iso_code, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", iso_code)) %>%
    mutate(location = recode(location, !!!owid_lk))
  
  return(df)
}