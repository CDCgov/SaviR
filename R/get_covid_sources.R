# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_df
#' @description Get and prepare COVID data.
#' @import dplyr
#' @importFrom data.table fread
#' @export

get_covid_df <- function() {
  who_data <- fread("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
    rename_all(tolower) %>%
    mutate(country = case_when(
      country == "Kosovo[1]" ~ "Kosovo",
      country %in% c("Bonaire", "Sint Eustatius", "Saba") ~ "Bonaire, Sint Eustatius, and Saba",
      country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      country == "Democratic Republic of the Congo" ~ "Congo DR",
      country == "Falkland Islands (Malvinas)" ~ "Falkland Islands",
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Democratic People's Republic of Korea" ~ "Korea (North)",
      country == "Lao People's Democratic Republic" ~ "Laos",
      country == "Micronesia (Federated States of)" ~ "Micronesia",
      country == "Northern Mariana Islands (Commonwealth of the)" ~ "Northern Mariana Islands",
      country == "occupied Palestinian territory, including east Jerusalem" ~ "Palestinian Territory",
      country == "Myanmar" ~ "Burma",
      country == "Republic of Korea" ~ "Korea (South)",
      country == "Republic of Moldova" ~ "Moldova",
      country == "Russian Federation" ~ "Russia",
      country == "Syrian Arab Republic" ~ "Syria",
      country == "United Republic of Tanzania" ~ "Tanzania",
      country == "United States Virgin Islands" ~ "Virgin Islands, US",
      country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      country == "The United Kingdom" ~ "United Kingdom",
      TRUE ~ country
    )) %>%
    mutate(country_code = case_when(
      country == "Namibia" ~ "NA",
      country == "Other" ~ "OT",
      country == "Bonaire, Sint Eustatius, and Saba" ~ "BQ",
      TRUE ~ country_code
    )) %>%
    rename("date" = names(.)[1]) %>%
    group_by_if(~ is.character(.) | lubridate::is.Date(.)) %>%
    summarize_all(list(~ sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(
      date = as.Date(date),
      source = "WHO"
    )

  jhu_cases <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename_all(tolower) %>%
    filter(`country/region` %in% c("Taiwan*", "China")) %>%
    mutate(`country/region` = case_when(
      `province/state` == "Hong Kong" ~ "Hong Kong",
      `province/state` == "Macau" ~ "Macau",
      TRUE ~ `country/region`
    )) %>%
    select(-lat, -long) %>%
    group_by(`country/region`) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_cases") %>%
    mutate(date = lubridate::mdy(date)) %>%
    mutate(`country/region` = recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
    group_by(`country/region`) %>%
    mutate(new_cases = case_when(
      is.na(lag(cumulative_cases)) ~ cumulative_cases,
      TRUE ~ cumulative_cases - lag(cumulative_cases)
    )) %>%
    ungroup()

  jhu_deaths <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", stringsAsFactors = FALSE, check.names = FALSE) %>%
    rename_all(tolower) %>%
    filter(`country/region` %in% c("Taiwan*", "China")) %>%
    mutate(`country/region` = case_when(
      `province/state` == "Hong Kong" ~ "Hong Kong",
      `province/state` == "Macau" ~ "Macau",
      TRUE ~ `country/region`
    )) %>%
    select(-lat, -long) %>%
    group_by(`country/region`) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_deaths") %>%
    mutate(date = lubridate::mdy(date)) %>%
    mutate(`country/region` = recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
    group_by(`country/region`) %>%
    mutate(new_deaths = case_when(
      is.na(lag(cumulative_deaths)) ~ cumulative_deaths,
      TRUE ~ cumulative_deaths - lag(cumulative_deaths)
    )) %>%
    ungroup()

  jhu_data <- left_join(jhu_cases, jhu_deaths, by = c("country/region", "date")) %>%
    rename(country = `country/region`) %>%
    mutate(
      who_region = "WPRO",
      country_code = case_when(
        country == "China" ~ "CN",
        country == "Taiwan" ~ "TW",
        country == "Hong Kong" ~ "HK",
        country == "Macau" ~ "MO"
      ),
      source = "JHU"
    ) %>%
    arrange(country, date)

  df <- bind_rows(who_data, jhu_data)

  df <- df %>%
    mutate(country = recode(country, "Côte d’Ivoire" = "Cote d'Ivoire")) %>%
    mutate(who_region = factor(who_region, levels = c("AMRO", "EURO", "SEARO", "EMRO", "AFRO", "WPRO"))) %>%
    mutate(
      region = case_when(
        who_region == "AMRO" ~ "Americas",
        who_region == "EURO" ~ "Europe",
        who_region == "SEARO" ~ "Southeast Asia",
        who_region == "EMRO" ~ "Eastern Mediterranean",
        who_region == "AFRO" ~ "Africa",
        who_region == "WPRO" ~ "Western Pacific"
      ),
      region = factor(region, levels = c("Americas", "Europe", "Southeast Asia", "Eastern Mediterranean", "Africa", "Western Pacific"))
    )

  return(df)
}