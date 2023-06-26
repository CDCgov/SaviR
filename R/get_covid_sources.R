# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_covid_df
#' @description Get and prepare COVID data.
#'
#' Pull in current case and death counts from WHO source.
#' For disaggregated China, Taiwan, Hong Kong, and Macau data we pull from JHU or primary sources.
#'
#' @param sources one of "all", "WHO", "WHO+JHU", "WHO+Primary" specifying the data sources to pull from. See details.
#'
#' @details
#' In legacy versions, the default was to pull "all" sources, which included the WHO case/death time-series and JHU data for China Mainland, HK, Macau, and Taiwan.
#' Due to sun-setting and changes in reporting, we now capture HK and Taiwan data from primary sources ("WHO+Primary"). Note that this also includes JHU data on Macau
#' which will be reported thru Mar 10, 2023 when JHU closes their dashboard.
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

get_covid_df <- function(sources = c("all", "WHO", "WHO+JHU", "WHO+Primary")) {
  sources <- match.arg(sources)

  # Pull WHO Data (which will always be included)
  out <- .fetch_data(
    "who_all",
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  ) |>
    process_who_data()

  if (sources == "WHO") {
    return(out)
  }

  # Pull JHU data
  jhu_cases <- .fetch_data(
    "jhu_case",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )  |>
  process_jhu_case_data()

  jhu_deaths <- .fetch_data(
    "jhu_death",
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) |>
  process_jhu_death_data()

  jhu_data <- left_join(jhu_cases, jhu_deaths, by = c("country/region", "date")) %>%
    rename(country = `country/region`) %>%
    mutate(
      iso2code = case_match(
        country,
        "China" ~ "CN",
        "Taiwan" ~ "TW",
        "Hong Kong" ~ "HK",
        "Macau" ~ "MO"
      ),
      source = "JHU"
    ) %>%
    arrange(country, date)
  
  out <- bind_rows(out, jhu_data)

  if (sources == "WHO+JHU") {
    return(out)
  }

  # Fetch HK data from HK CHP
  hk_data <- .fetch_data("hk_case_deaths") |>
    process_hk_data()

  # Fetch Taiwan case and death data
  # from Taiwan CDC
  tw_cases <- .fetch_data("taiwan_cases") |>
    process_taiwan_case_data()

  tw_deaths <- .fetch_data(
    "taiwan_deaths",
    encoding = "UTF-8",
    data.table = FALSE,
    check.names = FALSE
  ) |>
    process_taiwan_death_data()
  
  tw_data <- full_join(
    tw_cases, tw_deaths,
    by = "date"
  ) |>
    mutate(
      iso2code = "TW",
      country = "Taiwan",
      source = "Taiwan CDC"
    )

  out <- bind_rows(out, hk_data, tw_data)

  # Keep only Macau data from JHU if we want primary sources + WHO
  # else, keep all of it
  if (sources %in% c("WHO+Primary")) {
    out <- out |>
      filter(!(source == "JHU" & country %in% c("Hong Kong", "China", "Taiwan")))
  }

  return(out)
}

process_who_data <- function(raw_data) {

  names(raw_data) <- c("date", names(raw_data)[-1])

  out <- raw_data |>
    rename_all(tolower) %>%
    rename(iso2code = country_code) %>%
    mutate(country = recode(country, !!!who_lk)) %>%
    mutate(
      iso2code = case_match(
        country,
        "Namibia" ~ "NA",
        "Other" ~ "OT",
        "Bonaire, Sint Eustatius, and Saba" ~ "BQ",
      .default =  iso2code
      )
    ) %>%
    group_by_if(~ is.character(.) | lubridate::is.Date(.)) %>%
    summarize_all(list(~ sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(
      date = as.Date(date),
      source = "WHO"
    ) %>%
    select(-who_region)

  return(out)
}

process_jhu_case_data <- function(raw_data) {
  out <- raw_data |>
    rename_all(tolower) %>%
    filter(`country/region` %in% c("Taiwan*", "China")) %>%
    mutate(`country/region` = case_when(
      `province/state` == "Hong Kong" ~ "Hong Kong",
      `province/state` == "Macau" ~ "Macau",
      TRUE ~ `country/region`
    )) %>%
    select(-lat, -long) %>%
    group_by(`country/region`, data_date) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_cases") %>%
    mutate(date = lubridate::mdy(date)) %>%
    mutate(`country/region` = recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
    group_by(`country/region`, data_date) %>%
    mutate(new_cases = case_when(
      is.na(lag(cumulative_cases)) ~ cumulative_cases,
      TRUE ~ cumulative_cases - lag(cumulative_cases)
    )) %>%
    ungroup()
  
  return(out)
}

process_jhu_death_data <- function(raw_data) {
  
  out <- raw_data %>%
    rename_all(tolower) %>%
    filter(`country/region` %in% c("Taiwan*", "China")) %>%
    mutate(`country/region` = case_when(
      `province/state` == "Hong Kong" ~ "Hong Kong",
      `province/state` == "Macau" ~ "Macau",
      TRUE ~ `country/region`
    )) %>%
    select(-lat, -long) %>%
    group_by(`country/region`, data_date) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = where(is.numeric), names_to = "date", values_to = "cumulative_deaths") %>%
    mutate(date = lubridate::mdy(date)) %>%
    mutate(`country/region` = recode(`country/region`, "Taiwan*" = "Taiwan")) %>%
    group_by(`country/region`, data_date) %>%
    mutate(new_deaths = case_when(
      is.na(lag(cumulative_deaths)) ~ cumulative_deaths,
      TRUE ~ cumulative_deaths - lag(cumulative_deaths)
    )) %>%
    ungroup()

  return(out)
}

#' @importFrom lubridate dmy
process_hk_data <- function(data_raw) {

  data_raw[["pcr_and_rat"]] <- rowSums(
    data_raw[, c("Number of cases tested positive for SARS-CoV-2 virus by nucleic acid tests", "Number of cases tested positive for SARS-CoV-2 virus by rapid antigen tests")],
    na.rm = TRUE
  )

  out <- data_raw |>
    mutate(
      date = lubridate::dmy(`As of date`),
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
    group_by(data_date) |>
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
    ungroup() |>
    select(date, data_date, iso2code, country, new_cases, cumulative_cases, new_deaths, cumulative_deaths, source)

  return(out)
}

process_taiwan_case_data <- function(data_raw) {
  case_cols <- c(
    "disease_name",
    "date",
    "county",
    "township",
    "gender",
    "imported",
    "age_group",
    "cases",
    "data_date"
  )

  out <- data_raw |>
    setNames(case_cols) |>
    select(date, data_date, cases) |>
    mutate(
      date = lubridate::ymd(date),
      cases = as.integer(cases)
    ) |>
    group_by(data_date, date) |>
    summarise(
      new_cases = sum(cases, na.rm = TRUE)
    ) |>
    arrange(date) |>
    mutate(cumulative_cases = cumsum(new_cases)) |>
    ungroup()
  
  return(out)
}

process_taiwan_death_data <- function(data_raw) {
  death_cols <- c(
    "disease_name",
    "date",
    "county",
    "township",
    "gender",
    "imported",
    "age_group",
    "deaths",
    "data_date"
  )
  
  # Note: "date" here is date of case onset
  # which is different from other place.
  out <- data_raw |>
    setNames(death_cols) |>
    select(date, data_date, deaths) |>
    mutate(
      date = lubridate::ymd(date),
      deaths = as.integer(deaths)
    ) |>
    group_by(data_date, date) |>
    summarise(new_deaths = sum(deaths, na.rm = TRUE)) |>
    arrange(date) |>
    mutate(cumulative_deaths = cumsum(new_deaths)) |>
    ungroup()

  return(out)
}

# A helper function to pull from web or data lake,
# depending on availability of data lake
.fetch_data <- function(lookup_name, ..., past_n = NULL) {

  if (getOption("savir.use_datalake", FALSE)) {
    rlang::check_installed("pins")
    rlang::check_installed("arrow")

    pin_board <- pins::board_azure(az_container, path = "DGHT/ITF-SAVI/COVID-19 Data Ingest")

    # If we requested multiple versions
    # pull the version numbers and download all
    if (!is.null(past_n)) {
      versions_to_pull <- pin_board |>
        pins::pin_versions(lookup_name) |>
        arrange(desc(created)) |>
        top_n(past_n)
      
      # Helper function to read in pinned data and append a timestamp
      pin_append_created_dt <- function(version, created, pin_board = pin_board, lookup_name = lookup_name) {
        raw_data <- pins::pin_read(board = pin_board, name = lookup_name, version = version) |>
          mutate(data_date = as.Date(created))
    
        return(raw_data)
      }

      # NOTE: This is currently sequential and not very performant.
      raw_data <- Map(pin_append_created_dt, versions_to_pull[["version"]], versions_to_pull[["created"]])

      # Combine all data prior to return
      raw_data <- data.table::rbindlist(raw_data) |>
        as_tibble()
    # Standard operation -> return only most recent version
    } else {
      raw_data <- pins::pin_read(
        board = pin_board,
        name = sprintf("%s_data", lookup_name)
      ) |>
      # HACK: to streamline the cleaning process
      # upstream, we just add an NA date here
      mutate(data_date = as.Date(NA)) |>
      as_tibble()
    }
  } else {
    raw_data <- data.table::fread(
      datasource_lk[[lookup_name]],
      ...
    ) |>
    # HACK: to streamline the cleaning process
    # upstream, we just add an NA date here
    mutate(data_date = as.Date(NA)) |>
    as_tibble()
  }

  return(raw_data)
}