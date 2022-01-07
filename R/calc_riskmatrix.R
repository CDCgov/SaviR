# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title calc_add_risk
#' @description Calculate epi stats a generalized df input.
#' @param df Dataframe with id, date, new_cases, new_deaths, cumulative_cases, cumulative_deaths, AND population.
#'

#' @importFrom RcppRoll roll_mean
#' @import lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' df <- onetable %>%
#'   right_join(get_covid_df(), by = c("iso2code")) %>%
#'   filter(!(country == "China" & source == "WHO"))
#'
#' calc_add_risk(df)
#' }
#'
calc_add_risk <- function(df) {
  df <- df %>%
    mutate(
      new_cases_copy = replace(new_cases, new_cases < 0, 0),
      new_deaths_copy = replace(new_deaths, new_deaths < 0, 0),
      cumulative_cases_copy = replace(cumulative_cases, cumulative_cases < 0 | is.na(cumulative_cases), 0),
      cumulative_deaths_copy = replace(cumulative_deaths, cumulative_deaths < 0 | is.na(cumulative_deaths), 0)
    ) %>%
    group_by(source, id) %>%
    arrange(date) %>%
    mutate(
      weekdate = lubridate::floor_date(date, "week", week_start = 1),
      new_cases_7dav = roll_mean(new_cases, n = 7, fill = 0, align = "right"),
      new_deaths_7dav = roll_mean(new_deaths, n = 7, fill = 0, align = "right"),
      daily_case_incidence = if_else(population > 0, ((new_cases_copy / population)) * 100000, NA_real_),
      daily_death_incidence = if_else(population > 0, ((new_deaths_copy / population)) * 100000, NA_real_),
      week_case = cumulative_cases_copy - lag(cumulative_cases_copy, 7),
      prev_week_case = lag(cumulative_cases_copy, 7) - lag(cumulative_cases_copy, 14),
      prev_4week_case = lag(cumulative_cases_copy, 28) - lag(cumulative_cases_copy, 35),
      week_death = cumulative_deaths_copy - lag(cumulative_deaths_copy, 7),
      prev_week_death = lag(cumulative_deaths_copy, 7) - lag(cumulative_deaths_copy, 14)
    ) %>%
    mutate_at(vars(
      daily_case_incidence, daily_death_incidence,
      week_case, prev_week_case,
      week_death, prev_week_death
    ), ~ replace(., . < 0, NA_real_)) %>%
    ungroup() %>%
    mutate(
      diff_case = week_case - prev_week_case,
      diff_case4 = week_case - prev_4week_case,
      diff_death = week_death - prev_week_death
    ) %>%
    mutate(
      week_case_change = if_else(prev_week_case > 0, (diff_case) / prev_week_case, NA_real_),
      week_case_change4 = if_else(prev_week_case > 0, (diff_case4) / prev_4week_case, NA_real_),
      week_death_change = if_else(prev_week_death > 0, (diff_death) / prev_week_death, NA_real_)
    ) %>%
    mutate(
      week_case_incidence = if_else(population > 0, ((week_case / population) / 7) * 100000, NA_real_),
      week_death_incidence = if_else(population > 0, ((week_death / population) / 7) * 100000, NA_real_)
    ) %>%
    mutate(
      percent_change_case = if_else((!is.na(week_case_change)) & (!is.infinite(week_case_change)), week_case_change * 100, NA_real_),
      percent_change4_case = if_else((!is.na(week_case_change4)) & (!is.infinite(week_case_change4)), week_case_change4 * 100, NA_real_),
      percent_change_death = if_else((!is.na(week_death_change)) & (!is.infinite(week_death_change)), week_death_change * 100, NA_real_)
    )

  return(df)
}
