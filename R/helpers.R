# Internal help functions that power other user-visible functions


#' @title Compute average incidence over a set day interval
#' @param data a data.frame with required columns to compute the metric
#' @param type (character) one of cases or deaths, specifying the appropriate basis for the metric
#' @param window (numeric, default: 14) a numeric representing days to calculate the metric over
#' 
#' @details 
#' For `type` == "cases", data should contain at least date, new_cases, and population columns.  
#' For `type` == "deaths", data should contain new_deaths instead.  
#' 
#' Note that incidence here is per 100K population.
#' The function assumes that data passed has observations for each day for each country, since we use
#' and index-based approach to compute average incidence, not calendar-time.
#' 
#' @return a data.frame of summarized incidence values (ave_incidence) by date, possibly including grouping vars if data were grouped.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_covid_df("WHO")
#' # 14d average incidence world-wide
#' calc_window_incidence(window = 14)
#' # For grouped operations, group data beforehand and pipe:
#' # 14d average incidence by country
#' data |>
#'   group_by(iso2code, country) |>
#'   calc_window_incidence(window = 14)
#' }
#'
calc_window_incidence <- function(data, type = c("cases", "deaths"), window = 14) {
  type <- match.arg(type)

  # Check that required columns are present
  # for cases: new_cases, date, population
  # for deaths: new_deaths, date, population
  required_cols <- c("date", "population", sprintf("new_%s", type))
  stopifnot(all(required_cols %in% colnames(data)))

  # dynamic col name based on cases/deaths
  calc_col <- as.name(sprintf("new_%s", type))

  out <- data |>
    group_by(date, .add = TRUE) |>
    summarize(
      !!calc_col := sum(!!calc_col, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    arrange(date) |>
    mutate(
      weekly_cases = RcppRoll::roll_sum(
        !!calc_col,
        n = window,
        align = "right",
        fill = NA,
        na.rm = TRUE
      ),
      ave_incidence = 1e5 * (weekly_cases / population) / window
    ) |>
    select(-weekly_cases, -!!calc_col, -population) |>
    ungroup()
  
  return(out)
}

#' @title Compute Percent-change in Cases Over a Specified Window
#' @description 
#' Given a time series of new cases over a certain date range, compute a windowed percent change
#' value of cases (i.e. one week versus previous 7 days, etc.).
#' 
#' The window is right-aligned by the date column (beginning the most recent date shifting backwards).
#' 
#' Note that because we're computing based on index rather than calendar time, results will be erroneous
#' if data are not complete for every date.
#' 
#' For `type` == "cases", data should contain at least "date" and "new_cases" columns.
#' For `type` == "deaths", data should contain at least "date" and "new_deaths" columns.  
#' 
#' @param return_totals (default: FALSE) return running sums used to compute `pct_change`?
#' 
#' @return a df summarized by date with new column `pct_change`, or pct_change, cases_current, cases_prev if `return_totals` is `TRUE`
#' @importFrom RcppRoll roll_sum
#' @inheritParams calc_window_incidence
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_covid_df()
#' calc_window_pct_change(window = 14)
#' # For grouped operations, group data beforehand and pipe:
#' data |>
#'   group_by(iso2code, country) |>
#'   calc_window_pct_change(window = 14)
#' 
#' }
#'
calc_window_pct_change <- function(data, type = c("cases", "deaths"), window = 14L, return_totals = FALSE) {
  
  type <- match.arg(type)

  # Check that required columns are present
  # for cases: new_cases, date, population
  # for deaths: new_deaths, date, population
  required_cols <- c("date", sprintf("new_%s", type))
  stopifnot(all(required_cols %in% colnames(data)))

  # dynamic col name based on cases/deaths
  calc_col <- as.name(sprintf("new_%s", type))

  out <- data |>
    group_by(date, .add = TRUE) |>
    summarize(!!calc_col := sum(!!calc_col, na.rm = TRUE), .groups = "drop_last") |>
    arrange(date) |>
    mutate(
      current = roll_sum(!!calc_col, n = window, align = "right", fill = NA),
      prev = dplyr::lag(current, n = window),
      pct_change = current / prev
    ) |>
    ungroup()

  if (!return_totals) {
    out <- select(out, -!!calc_col, -prev, -current)
  }

  return(out)
  
}