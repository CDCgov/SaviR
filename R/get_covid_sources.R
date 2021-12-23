# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_df
#' @description Get and prepare COVID data.
#' @import dplyr
#' @importFrom data.table fread
#' @export

get_covid_df <- function() {
  who_data <- fread(datasource_lk$who_all, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
    rename_all(tolower) %>%
    rename(iso2code = country_code) %>%
    mutate(country = recode(country, !!!who_lk)) %>%
    mutate(iso2code = case_when(
      country == "Namibia" ~ "NA",
      country == "Other" ~ "OT",
      country == "Bonaire, Sint Eustatius, and Saba" ~ "BQ",
      TRUE ~ iso2code
    )) %>%
    rename("date" = names(.)[1]) %>%
    group_by_if(~ is.character(.) | lubridate::is.Date(.)) %>%
    summarize_all(list(~ sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(
      date = as.Date(date),
      source = "WHO"
    ) %>%
    select(-who_region)

  jhu_cases <- fread(datasource_lk$jhu_case, stringsAsFactors = FALSE, check.names = FALSE) %>%
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

  jhu_deaths <- fread(datasource_lk$jhu_death, stringsAsFactors = FALSE, check.names = FALSE) %>%
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
      iso2code = case_when(
        country == "China" ~ "CN",
        country == "Taiwan" ~ "TW",
        country == "Hong Kong" ~ "HK",
        country == "Macau" ~ "MO"
      ),
      source = "JHU"
    ) %>%
    arrange(country, date)

  df <- bind_rows(who_data, jhu_data)

  return(df)
}