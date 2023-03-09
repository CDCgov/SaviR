# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_df
#' @description Get and prepare COVID data.
#'
#' Pull in current case and death counts from WHO source.
#' For disaggregated China, Taiwan, Hong Kong, and Macau data we pull from primary sources.
#'
#'
#' @return Returns a data frame with n rows and 8 columns, including:
#' \itemize{
#'   \item{\code{date}}{  date Date of observation}
#'   \item{\code{iso2code}}{  character ISO 3166-1 alpha-2 country code}
#'   \item{\code{country}}{  character WHO english country name}
#'   \item{\code{new_cases}}{  integer Number of new cases reported on date}
#'   \item{\code{cumulative_cases}}{  integer Number of cumulative cases to date}
#'   \item{\code{new_deaths}}{  integer Number of new deaths reported on date}
#'   \item{\code{cumulative_deaths}}{  integer Number of cumulative deaths to date}
#'   \item{\code{source}}{  character Data Source}
#' }
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

  hk_data <- get_hk_data()
  tw_data <- get_taiwan_data()
  df <- bind_rows(who_data, jhu_data, hk_data, tw_data)

  return(df)
}

get_hk_data <- function() {
  hk_data_raw <- fread(datasource_lk$hk_case_deaths, stringsAsFactors = FALSE, encoding = "UTF-8", data.table = FALSE) |>
    as_tibble()

  hk_data_raw[["pcr_and_rat"]] <- rowSums(
    hk_data_raw[, c("Number of cases tested positive for SARS-CoV-2 virus by nucleic acid tests", "Number of cases tested positive for SARS-CoV-2 virus by rapid antigen tests")],
    na.rm = TRUE
  )

  hk_data <- hk_data_raw |>
    mutate(
      date = as.Date(`As of date`, "%d/%m/%Y"),
      iso2code = "HK",
      country = "Hong Kong",
      source = "HK CHP",
      # Number of confirmed cases used to be used
      # prior to Omicron wave, but was replaced by
      # the two other vars that stratified by PCR or RAT pos
      cumulative_cases = case_when(
        !is.na(`Number of confirmed cases`) ~ as.numeric(`Number of confirmed cases`),
        pcr_and_rat != 0 ~ pcr_and_rat,
        TRUE ~ NA_real_
      )
    ) |>
    rename(cumulative_deaths = `Number of death cases`) |>
    # Cumultive case reporting stopped for some reason
    # so we need to fill downwards to continue it
    arrange(date) |>
    tidyr::fill(cumulative_cases, cumulative_deaths) |>
    mutate(
      # Started tracking new deaths via this variable in Jan 2023
      cumulative_deaths = if_else(
        !is.na(`Number of death cases related to COVID-19`),
        as.double(cumulative_deaths + cumsum(tidyr::replace_na(`Number of death cases related to COVID-19`, 0))),
        as.double(cumulative_deaths)
      ),
      # Started tracking new cases via this variable in Jan 2023
      cumulative_cases = if_else(
        !is.na(`Number of positive nucleic acid test laboratory detections`),
        as.double(cumulative_cases + cumsum(tidyr::replace_na(`Number of positive nucleic acid test laboratory detections`, 0))),
        as.double(cumulative_cases)
      ),
      new_cases = cumulative_cases - lag(cumulative_cases, default = 0),
      new_deaths = cumulative_deaths - lag(cumulative_deaths, default = 0)
    ) |>
    select(date, iso2code, country, new_cases, cumulative_cases, new_deaths, cumulative_deaths, source)

  return(hk_data)
}

get_taiwan_data <- function() {
  tw_case_raw <- data.table::fread(
    datasource_lk$taiwan_cases,
    encoding = "UTF-8",
    data.table = FALSE
  )

  tw_death_raw <- data.table::fread(
    datasource_lk$taiwan_deaths,
    encoding = "UTF-8",
    data.table = FALSE
  )

  tw_cases <- tw_case_raw |>
    rename(
      date = `個案研判日`,
      cases = `確定病例數`
    ) |>
    mutate(
      date = as.Date(date, "%Y/%m/%d")
    ) |>
    group_by(date) |>
    summarise(
      new_cases = sum(cases, na.rm = T)
    ) |>
    ungroup() |>
    arrange(date) |>
    mutate(cumulative_cases = cumsum(new_cases))

  tw_deaths <- tw_death_raw |>
    rename(
      date = `發病日`,
      deaths = `死亡病例數`
    ) |>
    mutate(date = as.Date(date, "%Y/%m/%d")) |>
    group_by(date) |>
    summarise(new_deaths = sum(deaths, na.rm = T)) |>
    arrange(date) |>
    mutate(cumulative_deaths = cumsum(new_deaths))

  tw_data <- full_join(
    tw_cases, tw_deaths,
    by = "date"
  ) |>
    mutate(
      iso2code = "TW",
      country = "Taiwan",
      source = "Taiwan CDC"
    )

  return(tw_data)
}
