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