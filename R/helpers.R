# Internal help functions that power other user-visible functions


#' @title Compute average incidence over a set day interval
#' @param data a data.frame with at least [date, id, new_cases, and population] columns to compute incidence
#' @param window a numeric representing days to compute average incidence over
#' 
#' @details 
#' Note that incidence here is per 100K population.
#' The function assumes that data passed has observations for each day for each country, since we use
#' and index-based approach to compute average incidence, not calendar-time.
#' @return a data.frame of summarized incidence values (ave_incidence) by id, from the latest date provided
calc_window_incidence <- function(data, window = 7) {
  stopifnot(all(c("id", "date", "new_cases", "population") %in% colnames(data)))

  out <- data |>
    group_by(id) |>
    arrange(date) |>
    mutate(
      weekly_cases = RcppRoll::roll_sum(
        new_cases,
        n = window,
        align = "right",
        fill = NA
      ),
      ave_incidence = 1e5 * (weekly_cases / population) / window
    ) |>
    ungroup() |>
    filter(date == max(date)) |>
    select(id, date, ave_incidence)
  
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
#' @param df Dataframe with at least date, new_cases columns
#' @param window (default: 14) number of days the comparison windows should be
#' @param return_totals (default: FALSE) return running sums used to compute `pct_change`?
#' 
#' @return a df summarized by date with new column `pct_change`, or pct_change, cases_current_{window}, cases_prev_{window} if `return_totals` is `TRUE`
#' @importFrom RcppRoll roll_sum
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_covid_df()
#' calc_window_pct_change(data, window = 14)
#' # For grouped operations, group data beforehand and pipe:
#' data |>
#'   group_by(iso2code, country) |>
#'   calc_window_pct_change(data, window = 14)
#' }
#'
calc_window_pct_change <- function(df, window = 14L, return_totals = FALSE) {
  # Assert that we have the cols we need
  stopifnot(all(c("date", "new_cases") %in% colnames(df)))

  current_col <- as.name(sprintf("cases_current_%d", window))
  prev_col <- as.name(sprintf("cases_prev_%d", window))

  out <- df |>
    group_by(date, .add = TRUE) |>
    summarize(new_cases = sum(new_cases, na.rm = TRUE), .groups = "drop_last") |>
    arrange(date) |>
    mutate(
      !!current_col := roll_sum(new_cases, n = window, align = "right", fill = NA),
      !!prev_col := roll_sum(dplyr::lag(new_cases, n = window), n = window, align = "right", fill = NA),
      pct_change = !!current_col / !!prev_col
    ) |>
    ungroup()

  if (!return_totals) {
    out <- select(out, -!!prev_col, -!!current_col)
  }

  return(out)
  
}