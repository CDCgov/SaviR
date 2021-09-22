#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title calc_add_risk
#' @description Calculate epi stats a generalized df input.
#' @param df Dataframe with id (iso3), date, new_cases, new_deaths, cumulative_cases, cumulative_deaths, AND population.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' riskmatrix_v3_df <- calc_riskmatrix_v3()}
#'

calc_add_risk <- function(df){

  df <- df %>%
    dplyr::mutate(new_cases_copy  = replace(new_cases, new_cases < 0, 0),
                  new_deaths_copy = replace(new_deaths, new_deaths < 0, 0),
                  cumulative_cases_copy = replace(cumulative_cases, cumulative_cases < 0 | is.na(cumulative_cases), 0),
                  cumulative_deaths_copy = replace(cumulative_deaths, cumulative_deaths < 0 | is.na(cumulative_deaths), 0)) %>%
    dplyr::group_by(source, id) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(weekdate              = lubridate::floor_date(date, "week", week_start = 1),
                  new_cases_7dav        = zoo::rollmean(new_cases, k = 7, fill = 0, align = "right"),
                  new_deaths_7dav       = zoo::rollmean(new_deaths, k = 7, fill = 0, align = "right"),
                  daily_case_incidence  = dplyr::if_else(population > 0, ((new_cases_copy/population)) * 100000,  NA_real_),
                  daily_death_incidence = dplyr::if_else(population > 0, ((new_deaths_copy/population)) * 100000, NA_real_),
                  week_case             = cumulative_cases_copy - dplyr::lag(cumulative_cases_copy, 7),
                  prev_week_case        = dplyr::lag(cumulative_cases_copy, 7) - dplyr::lag(cumulative_cases_copy, 14),
                  prev_4week_case       = dplyr::lag(cumulative_cases_copy, 28) - dplyr::lag(cumulative_cases_copy, 35),
                  week_death            = cumulative_deaths_copy - dplyr::lag(cumulative_deaths_copy, 7),
                  prev_week_death       = dplyr::lag(cumulative_deaths_copy, 7) - dplyr::lag(cumulative_deaths_copy, 14)) %>%
    dplyr::mutate_at(vars(daily_case_incidence, daily_death_incidence,
                          week_case, prev_week_case,
                          week_death, prev_week_death), ~replace(., .<0, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(diff_case             = week_case - prev_week_case,
                  diff_case4            = week_case - prev_4week_case,
                  diff_death            = week_death - prev_week_death) %>%
    dplyr::mutate(week_case_change      = dplyr::if_else(prev_week_case  > 0, (diff_case)/prev_week_case, NA_real_),
                  week_case_change4     = dplyr::if_else(prev_week_case  > 0, (diff_case4)/prev_4week_case, NA_real_),
                  week_death_change     = dplyr::if_else(prev_week_death > 0, (diff_death)/prev_week_death, NA_real_)) %>%
    dplyr::mutate(week_case_incidence   = dplyr::if_else(population > 0, ((week_case/population)/7)  * 100000,  NA_real_),
                  week_death_incidence  = dplyr::if_else(population > 0, ((week_death/population)/7) * 100000, NA_real_)) %>%
    dplyr::mutate(percent_change_case   = dplyr::if_else((!is.na(week_case_change))  & (!is.infinite(week_case_change)),  week_case_change  * 100, NA_real_),
                  percent_change4_case  = dplyr::if_else((!is.na(week_case_change4)) & (!is.infinite(week_case_change4)), week_case_change4 * 100, NA_real_),
                  percent_change_death  = dplyr::if_else((!is.na(week_death_change)) & (!is.infinite(week_death_change)), week_death_change * 100, NA_real_))

  return(df)
}
