# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_testing
#' @description Get testing data from OWID and FIND. And adds a new_tests column where OWID NA's are filled in with FIND observations.

#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing_df <- get_testing()
#' }
#'
get_testing <- function() {
  df_OWID <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv") %>%
    select(
      id = ISO.code,
      date = Date,
      new_tests_OWID = Daily.change.in.cumulative.total,
      cum_tests_OWID = Cumulative.total
    )

  df_FIND <- read.csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv") %>%
    filter(set == "country") %>%
    select(
      id = unit,
      date = time,
      new_tests_FIND = new_tests_orig,
      cum_tests_FIND = cum_tests_orig
    )

  df <- onetable %>%
    right_join(get_covid_df() %>% select(-who_region), by = c("iso2code" = "country_code")) %>%
    mutate(date = as.character(date)) %>%
    left_join(df_OWID, by = c("id", "date")) %>%
    left_join(df_FIND, by = c("id", "date")) %>%
    mutate(new_tests = case_when(
      is.na(new_tests_OWID) ~ new_tests_FIND,
      TRUE ~ new_tests_OWID
    ))
}