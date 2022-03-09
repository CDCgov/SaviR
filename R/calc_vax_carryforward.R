#' @title Carry-Forward vaccination metrics to latest date
#'
#' @param df (data.frame) Source data frame containing vaccine metrics from [get_vax()]
#' @param ... (optional) (un-)quoted column names to carry forward. Overwrites standard variable set
#'
#' @return df with either columns in \code{...} carried forward, or default set
#' @section Notes:
#' By default, function carries forward the following (unless names are passed in \code{...}):
#' \itemize{
#'   \item{total_vaccinations}{}
#'   \item{people_vaccinated}{}
#'   \item{people_fully_vaccinated}{}
#'   \item{total_boosters}{}
#'   \item{total_vaccinations_per_hundred}{}
#'   \item{people_vaccinated_per_hundred}{}
#'   \item{people_fully_vaccinated_per_hundred}{}
#'   \item{total_boosters_per_hundred}{}
#' }
#' @export
calc_vax_carryforward <- function(df, ...) {
  # Optionally specify vax cols by name if we want to be specific
  vax_cols <- rlang::enquos(...)

  # If not specified, just take standard set.
  if (!length(vax_cols)) {
    vaccine_col_str <- c(
      "total_vaccinations", "people_vaccinated", "people_fully_vaccinated",
      "total_boosters", "total_vaccinations_per_hundred", "people_vaccinated_per_hundred",
      "people_fully_vaccinated_per_hundred", "total_boosters_per_hundred"
    )

    vax_cols <- lapply(vaccine_col_str, as.name)
  }

  df %>%
    arrange(id, date) %>%
    group_by(id) %>%
    # Take any cols that
    mutate_at(vars(!!!vax_cols), zoo::na.locf, na.rm = FALSE) %>%
    ungroup()
}
