#' @title Compute Percent-change in Cases Over a Specified Window
#' @description 
#' Given a time series of new cases over a certain date range, compute a windowed percent change
#' value of cases (i.e. one week versus previous 7 days, etc.).
#' 
#' The window is right-aligned by the date column (beginning the most recent date shifting backwards).
#' 
#' Note that because we're computing based on index rather than calendar time, results will be erroneous
#' if data are not complete for every date.
#' @param df Dataframe with at least [date, new_cases] columns
#' @param window (default: 14) number of days the comparison windows should be
#' @param grp optional grouping variable (such as isocode, country name, region, etc.)
#' 
#' @return a df summarized by date with new column `pct_change`, of dim [n, 2 (or 3, if `grp` is specified)]
#' @importFrom RcppRoll roll_sum
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_covid_df()
#' calc_window_pct_change(data, window = 14, grp = "country")
#' }
#'
calc_window_pct_change <- function(df, window = 14L, grp = NULL) {
  # Assert that we have the cols we need
  stopifnot(all(c("date", "new_cases") %in% colnames(df)))

  # Grouped operation
  # (probably a much cleaner way to do this to avoid a large branch, but w/e)
  if (!missing(grp)) {
    grp_quo <- rlang::enquo(grp)
    
    out <- df |>
      group_by({{ grp_quo }}, date) |>
      summarize(new_cases = sum(new_cases, na.rm = TRUE)) |>
      arrange(date) |>
      group_by({{ grp_quo }}) |>
      mutate(
        pct_change = (
          roll_sum(new_cases, n = window, align = "right", fill = NA) /
          roll_sum(dplyr::lag(new_cases, n = window), n = window, align = "right", fill = NA)
        )
      ) |>
      ungroup()
  } else {
    out <- df |>
      group_by(date) |>
      summarize(new_cases = sum(new_cases, na.rm = TRUE)) |>
      ungroup() |>
      arrange(date) |>
      mutate(
        pct_change = (
          roll_sum(new_cases, n = window, align = "right", fill = NA) /
          roll_sum(dplyr::lag(new_cases, n = window), n = window, align = "right", fill = NA)
        )
      )
  }

  return(out)
  
}