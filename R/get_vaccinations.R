#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax
#' @description Get vaccinations from OWID
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

get_vax <- function() {

  df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv",
                 as.is = TRUE,
                 stringsAsFactors = FALSE,
                 check.names = FALSE) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(iso_code = dplyr::recode(iso_code, "OWID_KOS" = "XKX")) %>%
    dplyr::filter(!grepl("OWID", iso_code)) %>%
    dplyr::mutate(location = dplyr::recode(location,
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
                             "Cape Verde"                = "Cabo Verde"))%>%
    dplyr::mutate(daily_vaccinations_per_hundred = daily_vaccinations_per_million/10000)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_vax_manufacturers
#' @description Get vaccinations from OWID
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'

get_vax_manufacturers <- function() {

  df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv",
                 as.is = TRUE,
                 stringsAsFactors = FALSE,
                 check.names = FALSE) %>%
    dplyr::mutate(last_observation_date = as.Date(last_observation_date)) %>%
    dplyr::mutate(iso_code = dplyr::recode(iso_code, "OWID_KOS" = "XKX")) %>%
    dplyr::filter(!grepl("OWID", iso_code)) %>%
    dplyr::mutate(location = dplyr::recode(location,
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
                                           "Cape Verde"                = "Cabo Verde"))
}
