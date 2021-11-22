#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_testing
#' @description Get testing data from OWID and FIND. And adds a new_tests column where OWID NA's are filled in with FIND observations.
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing_df <- get_testing()}

get_testing <- function() {

  df_OWID <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")%>%
    dplyr::select(id             = ISO.code,
                  date           = Date,
                  new_tests_OWID = Daily.change.in.cumulative.total,
                  cum_tests_OWID = Cumulative.total)

  df_FIND <- read.csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv") %>%
    dplyr::filter(set == "country") %>%
    dplyr::select(id             = unit,
                  date           = time,
                  new_tests_FIND = new_tests_orig,
                  cum_tests_FIND = cum_tests_orig)

  df <- onetable %>%
    dplyr::right_join(get_covid_df() %>% dplyr::select(-who_region), by = c("iso2code" = "country_code")) %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::left_join(df_OWID, by = c("id", "date")) %>%
    dplyr::left_join(df_FIND, by = c("id", "date")) %>%
    dplyr::mutate(new_tests = dplyr::case_when(is.na(new_tests_OWID) ~new_tests_FIND,
                                               TRUE  ~new_tests_OWID))

}
