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