#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_df
#' @description Get and prepare COVID data.
#' @importFrom magrittr `%>%`
#'
#' @export

get_covid_df <- function(){

  df <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8") %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Kosovo[1]"                                                ~"Kosovo",
                                             country %in% c("Bonaire", "Sint Eustatius", "Saba")                   ~"Bonaire, Sint Eustatius, and Saba",
                                             country == "Bolivia (Plurinational State of)"                         ~"Bolivia",
                                             country == "Democratic Republic of the Congo"                         ~"Congo DR",
                                             country == "Falkland Islands (Malvinas)"                              ~"Falkland Islands",
                                             country == "Iran (Islamic Republic of)"                               ~"Iran",
                                             country == "Democratic People's Republic of Korea"                    ~"Korea (North)",
                                             country == "Lao People's Democratic Republic"                         ~"Laos",
                                             country == "Micronesia (Federated States of)"                         ~"Micronesia",
                                             country == "Northern Mariana Islands (Commonwealth of the)"           ~"Northern Mariana Islands",
                                             country == "occupied Palestinian territory, including east Jerusalem" ~"Palestinian Territory",
                                             country == "Myanmar"                                                  ~"Burma",
                                             country == "Republic of Korea"                                        ~"Korea (South)",
                                             country == "Republic of Moldova"                                      ~"Moldova",
                                             country == "Russian Federation"                                       ~"Russia"
                                             country == "Syrian Arab Republic"                                     ~"Syria",
                                             country == "United Republic of Tanzania"                              ~"Tanzania",
                                             country == "United States Virgin Islands"                             ~"Virgin Islands, US",
                                             country == "Venezuela (Bolivarian Republic of)"                       ~"Venezuela",
                                             country == "The United Kingdom"                                       ~"United Kingdom",
                                             TRUE ~country)) %>%
    dplyr::mutate(country_code = dplyr::case_when(country == "Namibia"                           ~"NA",
                                                  country == "Other"                             ~"OT",
                                                  country == "Bonaire, Sint Eustatius, and Saba" ~"BQ",
                                                  TRUE ~country_code)) %>%
    dplyr::rename("date" = names(.)[1]) %>%
    dplyr::group_by_if(is.character) %>%
    dplyr::summarize_all(list(~sum(., na.rm=T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.Date(date),
                  source = "WHO") %>%

    dplyr::bind_rows(
      read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
               as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE) %>%
        dplyr::rename_all(tolower) %>%
        dplyr::filter(`country/region` %in% c("Taiwan*", "China")) %>%
        dplyr::mutate(`country/region` = dplyr::case_when(`province/state` == "Hong Kong" ~ "Hong Kong",
                                                          `province/state` == "Macau"     ~ "Macau",
                                                          TRUE ~ `country/region`)) %>%
        dplyr::select(-lat,-long) %>%
        dplyr::group_by(`country/region`) %>%
        dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_cases") %>%
        dplyr::mutate(date             = lubridate::mdy(date)) %>%
        dplyr::mutate(`country/region` = dplyr::recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
        dplyr::group_by(`country/region`) %>%
        dplyr::mutate(new_cases        = cumulative_cases - dplyr::lag(cumulative_cases)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(
          read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                   as.is=TRUE, stringsAsFactors = FALSE, check.names=FALSE) %>%
            dplyr::rename_all(tolower) %>%
            dplyr::filter(`country/region` %in% c("Taiwan*", "China")) %>%
            dplyr::mutate(`country/region` = dplyr::case_when(`province/state` == "Hong Kong" ~ "Hong Kong",
                                                              `province/state` == "Macau"     ~ "Macau",
                                                              TRUE ~ `country/region`)) %>%
            dplyr::select(-lat,-long) %>%
            dplyr::group_by(`country/region`) %>%
            dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_deaths") %>%
            dplyr::mutate(date             = lubridate::mdy(date)) %>%
            dplyr::mutate(`country/region` = dplyr::recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
            dplyr::group_by(`country/region`) %>%
            dplyr::mutate(new_deaths       = cumulative_deaths - dplyr::lag(cumulative_deaths)) %>%
            dplyr::ungroup()
        ) %>%
        dplyr::rename(country = `country/region`) %>%
        dplyr::mutate(who_region = "WPRO",
                      country_code = dplyr::case_when(country == "China"     ~"CN",
                                                      country == "Taiwan"    ~"TW",
                                                      country == "Hong Kong" ~"HK",
                                                      country == "Macau"     ~"MO"),
                      source = "JHU")%>%
        dplyr::arrange(country, date)
    ) %>%
    dplyr::mutate(country    = dplyr::recode(country, "Côte d’Ivoire" = "Cote d'Ivoire")) %>%
    dplyr::mutate(who_region = factor(who_region, levels= c("AMRO","EURO","SEARO","EMRO","AFRO","WPRO"))) %>%
    dplyr::mutate(region     = dplyr::case_when(who_region == "AMRO"  ~"Americas",
                                                who_region == "EURO"  ~"Europe",
                                                who_region == "SEARO" ~"Southeast Asia",
                                                who_region == "EMRO"  ~"Eastern Mediterranean",
                                                who_region == "AFRO"  ~"Africa",
                                                who_region == "WPRO"  ~"Western Pacific"),
                  region     = factor(region, levels= c("Americas","Europe","Southeast Asia","Eastern Mediterranean","Africa","Western Pacific")))

}
