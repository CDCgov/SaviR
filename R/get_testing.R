#' @title get_testing_long
#' @description Download and combine full time series data related to testing as compiled by Our World in Data and FIND
#'
#' @param find_maxgap (numeric, default: 31) Gap between cumulative testing number to linearly interpolate
#' @param flag_test_increase (numeric, default: 5) Flag for increase in interpolated cumulative tests
#'
#' @return Longitudinal data frame with both FIND and OWID data sets with consistent definitions
#' @import zoo
#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing_long <- get_testing_long()
#' }
#'
get_testing_long <- function(find_maxgap = 31, flag_test_increase = 5) {



  # Download various OWID/FIND datasets
  ## The testing-specific OWID dataset may be more up to date than the overall OWID dataset
  testing_OWID <- data.table::fread(datasource_lk$owid_testing, data.table = F, showProgress = F, verbose = F) %>%
    filter(!(Entity %in% c(
      "Poland - samples tested", "Italy - people tested",
      "Canada - people tested"
    ))) %>%
    mutate(date = as.Date(Date)) %>%
    select(
      id = `ISO code`,
      date,
      test_source_url = `Source URL`,
      total_tests = `Cumulative total`,
      new_tests = `Daily change in cumulative total`,
      new_tests_per_thousand = `Daily change in cumulative total per thousand`,
      new_tests_smoothed = `7-day smoothed daily change`,
      new_tests_smoothed_per_thousand = `7-day smoothed daily change per thousand`,
      positive_rate = `Short-term positive rate`
    ) %>%
    mutate(id = recode(id, "OWID_KOS" = "XKX"))

  # Make sure you have just 1 observation per iso-code/date pairing
  n_test_iso <- testing_OWID %>%
    count(id, date) %>%
    filter(n > 1)
  if (nrow(n_test_iso) > 0) {
    stop("Check testing dataset in get_testing_long() -- multiple values per country-date")
  }

  full_OWID <- data.table::fread(datasource_lk$owid_all, data.table = F, showProgress = F, verbose = F) %>%
    rename(id = iso_code) %>%
    mutate(date = as.Date(date)) %>%
    mutate(id = recode(id, "OWID_KOS" = "XKX")) %>%
    filter(!grepl("OWID", id)) %>%
    select(
      id, location, date, population, total_cases, new_cases, new_cases_smoothed,
      tests_units
    )

  full_OWID_tests <- full_join(full_OWID, testing_OWID, by = c("id", "date"))

  full_FIND <-
    data.table::fread(datasource_lk$find_testing, data.table = F, verbose = F, showProgress = F) %>%
    filter(set == "country")

  find_meta <-
    data.table::fread(datasource_lk$find_metadata, data.table = F, verbose = F, showProgress = F) %>%
    filter(set == "country") %>%
    select(
      id = unit,
      tests_units = tests_description,
      test_definition = tests_type
    ) %>%
    mutate(
      case_definition = NA,
      posrate_definition = NA
    )

  owid_meta <- owid_testing_meta %>%
    rename(posrate_definition_old = posrate_definition) %>%
    mutate(
      posrate_definition = ifelse(
        str_detect(posrate_definition_old, "calculated by Our World"), "7-day cases / tests",
        ifelse(str_detect(posrate_definition_old, "not calculated"), "Not calculated",
          "Directly from source"
        )
      )
    ) %>%
    select(-posrate_definition_old)

  #### Clean OWID ####
  owid_countries <- full_OWID_tests %>%
    # Note -- some countries don't report cumulative, but instead have daily new test
    # See for example: https://ourworldindata.org/coronavirus-testing#sweden
    filter(!(is.na(total_tests) & is.na(new_tests)))

  iso_countries <- rbind(
    iso_nocumtest <- owid_countries %>% filter(is.na(total_tests) & !is.na(new_tests)) %>%
      group_by(id) %>% summarize(n_obs = n(), cumtest_available = F),
    iso_cumtest <- owid_countries %>% filter(!is.na(total_tests)) %>%
      group_by(id) %>% summarize(n_obs = n(), cumtest_available = T)
  )

  # Full series for countries with any new tests or cumulative tests data at any point in time
  owid_data <- full_OWID_tests %>%
    # Use above to limit to just the countries where there is at least some OWID data
    filter(id %in% iso_countries$id) %>%
    left_join(iso_countries %>% select(id, cumtest_available),
      by = "id"
    ) %>%
    select(-test_source_url) %>%
    group_by(id) %>%
    arrange(date) %>%
    mutate(
      total_tests_int = zoo::na.approx(total_tests, na.rm = F, maxgap = find_maxgap),
      new_tests_int = total_tests_int - lag(total_tests_int, 1),
      # NOTE: 8 countries report new tests to OWID instead of cumulative tests -- these are still valid
      new_tests_int2 = ifelse(is.na(total_tests) & is.na(new_tests_int) &
        !is.na(new_tests), new_tests, new_tests_int),
      # Flag if the total tests decreased in past X days due to data revision
      test_neg_value = ifelse(new_tests_int2 < 0, 1, 0), # Does new tests ever decrease?
      test_any_neg_7day = zoo::rollsumr(test_neg_value, 7, fill = NA),
      test_any_neg_14day = zoo::rollsumr(test_neg_value, 14, fill = NA),
      FLAG_negative_tests_7day = ifelse(test_any_neg_7day > 0, 1, 0),
      FLAG_negative_tests_14day = ifelse(test_any_neg_14day > 0, 1, 0),
      FLAG_negative_tests_7day_prev = lag(FLAG_negative_tests_7day, 7),
      FLAG_negative_tests_14day_prev = lag(FLAG_negative_tests_14day, 14),
      # Flag if total cases decreased in past X days due to data revision
      neg_case = ifelse(new_cases < 0, 1, 0),
      case_any_neg_7day = zoo::rollsumr(neg_case, 7, fill = NA),
      case_any_neg_14day = zoo::rollsumr(neg_case, 14, fill = NA),
      FLAG_negative_cases_7day = ifelse(case_any_neg_7day > 0, 1, 0),
      FLAG_negative_cases_14day = ifelse(case_any_neg_14day > 0, 1, 0),
      FLAG_negative_cases_7day_prev = lag(FLAG_negative_cases_7day, 7),
      FLAG_negative_cases_14day_prev = lag(FLAG_negative_cases_14day, 14),
      # Flag for large increases that are implausible based on interpolated cumulative tests
      cum_test_increase = (total_tests_int / lag(total_tests_int, 1) - 1) * 100,
      cum_test_increase_gt = ifelse(cum_test_increase > flag_test_increase, 1, 0),
      increase_any_test_7day = zoo::rollsumr(cum_test_increase_gt, 7, fill = NA),
      increase_any_test_14day = zoo::rollsumr(cum_test_increase_gt, 14, fill = NA),
      FLAG_increase_tests_7day = ifelse(increase_any_test_7day > 0, 1, 0),
      FLAG_increase_tests_14day = ifelse(increase_any_test_14day > 0, 1, 0),
      FLAG_increase_tests_7day_prev = lag(FLAG_increase_tests_7day, 7),
      FLAG_increase_tests_14day_prev = lag(FLAG_increase_tests_14day, 14),
      # Construct X-day average of new cases based on interpolated series
      new_tests_daily14 = zoo::rollmeanr(new_tests_int2, 14, fill = NA),
      new_tests_daily14_prev = lag(new_tests_daily14, 14),
      new_tests_daily14_per_1k = (new_tests_daily14 / population) * 1000,
      new_tests_daily14_per_1k_prev = lag(new_tests_daily14_per_1k, 14),
      new_tests_daily7 = new_tests_smoothed,
      new_tests_daily7_prev = lag(new_tests_daily7, 7),
      new_tests_daily7_per_1k = new_tests_smoothed_per_thousand,
      new_tests_daily7_per_1k_prev = lag(new_tests_daily7_per_1k, 7),
      # 7-day and 14-day average cases
      new_cases_daily7 = zoo::rollmeanr(new_cases, k = 7, fill = NA),
      new_cases_daily14 = zoo::rollmeanr(new_cases, k = 14, fill = NA),
      # NOTE: Positivity rate as provided in OWID is based on 7-day average cases / 7-day average tests
      # Sometimes they do NOT use JHU case data as numerator, so it's not exactly reproducible
      # This also means I cannot calculate the 14-day version for these 12 or so countries
      # In OWID meta data, see posrate_definition column
      positive_rate_7day = positive_rate,
      positive_rate_7day_prev = lag(positive_rate, 7)
    ) %>%
    ungroup()

  #### Clean FIND ####
  find_countries <- full_FIND %>%
    # Focus on the original recorded metrics of cumulative tests and new tests
    # Use these to construct our own metrics based on specifications
    filter(!(is.na(cum_tests_orig) & is.na(new_tests_orig))) %>%
    group_by(unit) %>%
    summarize(n_obs = n())

  find_data <- full_FIND %>%
    filter(unit %in% find_countries$unit) %>%
    select(-set) %>%
    rename(
      date = time,
      id = unit
    ) %>%
    mutate(date = as.Date(date)) %>%
    mutate(id = recode(id, "XK" = "XKX"))

  find_data2 <- find_data %>%
    group_by(id) %>%
    arrange(date) %>%
    mutate(
      # First, replace 0s with NAs
      new_tests_nafor0 = replace(new_tests_orig, new_tests_orig == 0, NA),
      cum_tests_alt1 = ifelse(new_tests_orig == 0, NA, cum_tests_orig),
      # Linearly interpolate values -- we can set a max value at which point we don't interpolate
      total_tests_int = zoo::na.approx(cum_tests_alt1, na.rm = F, maxgap = find_maxgap),
      new_tests_int = total_tests_int - lag(total_tests_int, 1),
      # Construct average daily series for new tests
      new_tests_daily14 = zoo::rollmeanr(new_tests_int, k = 14, fill = NA),
      new_tests_daily14_prev = lag(new_tests_daily14, 14),
      new_tests_daily14_per_1k = (new_tests_daily14 / (pop_100k * 100000)) * 1000,
      new_tests_daily14_per_1k_prev = lag(new_tests_daily14_per_1k, 14),
      new_tests_daily7 = zoo::rollmeanr(new_tests_int, k = 7, fill = NA),
      new_tests_daily7_prev = lag(new_tests_daily7, 7),
      new_tests_daily7_per_1k = (new_tests_daily7 / (pop_100k * 100000)) * 1000,
      new_tests_daily7_per_1k_prev = lag(new_tests_daily7_per_1k, 7),
      # Cases 7-day and 14-day based on new cases column
      new_cases_daily7 = zoo::rollmeanr(new_cases_orig, k = 7, fill = NA),
      new_cases_daily14 = zoo::rollmeanr(new_cases_orig, k = 14, fill = NA),
      # Positivity rates based on 7-day and 14-day cases & tests
      positive_rate_7day = new_cases_daily7 / new_tests_daily7,
      positive_rate_7day_prev = lag(positive_rate_7day, 7),
      positive_rate_14day = new_cases_daily14 / new_tests_daily14,
      positive_rate_14day_prev = lag(positive_rate_14day, 14),
      # FLAG: total cases decreased in past X days due to data revision
      neg_case = ifelse(new_cases_orig < 0, 1, 0),
      case_any_neg_7day = zoo::rollsumr(neg_case, 7, fill = NA),
      case_any_neg_14day = zoo::rollsumr(neg_case, 14, fill = NA),
      FLAG_negative_cases_7day = ifelse(case_any_neg_7day > 0, 1, 0),
      FLAG_negative_cases_14day = ifelse(case_any_neg_14day > 0, 1, 0),
      FLAG_negative_cases_7day_prev = lag(FLAG_negative_cases_7day, 7),
      FLAG_negative_cases_14day_prev = lag(FLAG_negative_cases_14day, 14),
      # FLAG: drop in cumulative tests
      test_neg_value = ifelse(new_tests_orig < 0, 1, 0),
      test_any_neg_7day = zoo::rollsumr(test_neg_value, 7, fill = NA),
      test_any_neg_14day = zoo::rollsumr(test_neg_value, 14, fill = NA),
      FLAG_negative_tests_7day = ifelse(test_any_neg_7day > 0, 1, 0),
      FLAG_negative_tests_14day = ifelse(test_any_neg_14day > 0, 1, 0),
      FLAG_negative_tests_7day_prev = lag(FLAG_negative_tests_7day, 7),
      FLAG_negative_tests_14day_prev = lag(FLAG_negative_tests_14day, 14),
      # FLAG: substantial increase in tests
      cum_test_increase = (total_tests_int / lag(total_tests_int, 1) - 1) * 100,
      cum_test_increase_gt = ifelse(cum_test_increase > flag_test_increase, 1, 0),
      increase_any_test_7day = zoo::rollsumr(cum_test_increase_gt, 7, fill = NA),
      increase_any_test_14day = zoo::rollsumr(cum_test_increase_gt, 14, fill = NA),
      FLAG_increase_tests_7day = ifelse(increase_any_test_7day > 0, 1, 0),
      FLAG_increase_tests_14day = ifelse(increase_any_test_14day > 0, 1, 0),
      FLAG_increase_tests_7day_prev = lag(FLAG_increase_tests_7day, 7),
      FLAG_increase_tests_14day_prev = lag(FLAG_increase_tests_14day, 14),
    ) %>%
    ungroup(.)

  # Create several longitudinal datasets -- save as RDS, excel, csv
  find_longdata <- left_join(find_data2, find_meta, by = "id") %>%
    rename(
      total_tests_original = cum_tests_orig,
      new_tests_original = new_tests_orig,
      new_cases_original = new_cases_orig
    ) %>%
    mutate(
      population = pop_100k * 100000,
      cumtest_available = T
    ) %>%
    select(
      id, date, population, total_tests_original, new_tests_original,
      total_tests_int, new_tests_int,
      new_tests_daily7, new_tests_daily7_prev, new_tests_daily7_per_1k, new_tests_daily7_per_1k_prev,
      new_tests_daily14, new_tests_daily14_prev, new_tests_daily14_per_1k, new_tests_daily14_per_1k_prev,
      new_cases_original, new_cases_daily7, new_cases_daily14,
      positive_rate_7day, positive_rate_7day_prev,
      starts_with("FLAG_"),
      tests_units:posrate_definition, cumtest_available
    ) %>%
    arrange(id, date)

  owid_longdata <- left_join(owid_data, owid_meta, by = "id") %>%
    mutate(
      # When positivity rate is directly calculated from source, ignore the flag
      FLAG_negative_cases_7day = replace(FLAG_negative_cases_7day, posrate_direct == TRUE, 0),
      FLAG_negative_cases_14day = replace(FLAG_negative_cases_14day, posrate_direct == TRUE, 0),
      FLAG_negative_cases_7day_prev = replace(FLAG_negative_cases_7day, posrate_direct == TRUE, 0),
      FLAG_negative_cases_14day_prev = replace(FLAG_negative_cases_14day, posrate_direct == TRUE, 0)
    ) %>%
    rename(
      total_tests_original = total_tests,
      new_tests_original = new_tests,
      new_cases_original = new_cases
    ) %>%
    select(
      id, date, population,
      total_tests_original, new_tests_original,
      total_tests_int,
      new_tests_int = new_tests_int2,
      new_tests_daily7, new_tests_daily7_prev, new_tests_daily7_per_1k, new_tests_daily7_per_1k_prev,
      new_tests_daily14, new_tests_daily14_prev, new_tests_daily14_per_1k, new_tests_daily14_per_1k_prev,
      new_cases_original, new_cases_daily7, new_cases_daily14,
      positive_rate_7day, positive_rate_7day_prev,
      starts_with("FLAG_"),
      tests_units, test_definition:posrate_definition, cumtest_available
    ) %>%
    select(-posrate_direct) %>%
    arrange(id, date)

  # Combine into a single longitudinal dataset and output
  test_long <-
    rbind(
      owid_longdata %>%
        mutate(data_source = "OWID"),
      find_longdata[, names(owid_longdata)] %>%
        mutate(data_source = "FIND")
    ) %>%
    relocate(data_source, id, date) %>%
    arrange(data_source, id, date)

  return(test_long)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_preferred_tests14
#' @description Evaluates testing data sources for quality based on new tests per 1K over the last 14 days providing a preferred choice between the two sources, OWID or FIND
#' OUTPUT:
#' - Most recent 14-day average data points for new tests per 1K based on a procedure for picking the preferred data source
#' - After accounting for flags, we pick OWID by default when both OWID and FIND are available in last X days
#' - Subject to future revisions
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tests_preferred <- get_preferred_tests14(test_long, last_X_days = 14, analysis_date = analysis_date)
#' tests_preferred <- get_preferred_tests14(test_long, last_X_days = 14, analysis_date = Sys.Date - 1)
#' }
#'
get_preferred_tests14 <- function(test_long, last_X_days = 14, analysis_date = NULL) {
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date() - 1
  }

  X_date <- analysis_date - last_X_days

  # Calculate most recent date based on what the analytical date is.
  # e.g., we run on Monday, but only want data through Sunday,
  #  so maxdate should be relative to this
  owid_df <- test_long %>%
    filter(data_source == "OWID") %>%
    filter(date <= analysis_date)
  find_df <- test_long %>%
    filter(data_source == "FIND") %>%
    filter(date <= analysis_date)

  owid_recent <- owid_df %>%
    filter(!is.na(new_tests_daily14_per_1k)) %>%
    group_by(id) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup() %>%
    # Correct FLAG for 8 countries which only report new tests daily, not cumulative
    mutate(FLAG_increase_tests_14day = replace(FLAG_increase_tests_14day, is.na(FLAG_increase_tests_14day), 0))

  find_recent <- find_df %>%
    filter(!is.na(new_tests_daily14_per_1k)) %>%
    group_by(id) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()

  # Try a full join between them, rename variables as needed
  all_recent <-
    full_join(
      owid_recent %>%
        select(id,
          owid_date = date,
          owid_total_tests_orig = total_tests_original,
          owid_total_tests_int = total_tests_int,
          owid_new_tests_orig = new_tests_original,
          owid_new_tests_int = new_tests_int,
          owid_tests_daily14 = new_tests_daily14,
          owid_tests_daily14_prev = new_tests_daily14_prev,
          owid_tests_daily14_1k = new_tests_daily14_per_1k,
          owid_tests_daily14_1k_prev = new_tests_daily14_per_1k_prev,
          owid_flag_negative_tests = FLAG_negative_tests_14day,
          owid_flag_increase_tests = FLAG_increase_tests_14day,
          owid_flag_negative_tests_prev14 = FLAG_negative_tests_14day_prev,
          owid_flag_increase_tests_prev14 = FLAG_increase_tests_14day_prev,
          owid_tests_units = tests_units,
          owid_test_definition = test_definition
        ),
      find_recent %>%
        select(id,
          find_date = date,
          find_total_tests = total_tests_original,
          find_total_tests_int = total_tests_int,
          find_new_tests_orig = new_tests_original,
          find_new_tests_int = new_tests_int,
          find_tests_daily14 = new_tests_daily14,
          find_tests_daily14_prev = new_tests_daily14_prev,
          find_tests_daily14_1k = new_tests_daily14_per_1k,
          find_tests_daily14_1k_prev = new_tests_daily14_per_1k_prev,
          find_flag_negative_tests = FLAG_negative_tests_14day,
          find_flag_increase_tests = FLAG_increase_tests_14day,
          find_flag_negative_tests_prev14 = FLAG_negative_tests_14day_prev,
          find_flag_increase_tests_prev14 = FLAG_increase_tests_14day_prev,
          find_tests_units = tests_units,
          find_test_definition = test_definition
        ),
      by = "id"
    )

  owid_recent_sub <- owid_recent %>%
    filter(date >= X_date) %>%
    filter(FLAG_increase_tests_14day == 0 & FLAG_negative_tests_14day == 0)

  find_recent_sub <- find_recent %>%
    filter(date >= X_date) %>%
    filter(FLAG_increase_tests_14day == 0 & FLAG_negative_tests_14day == 0)

  owid_first <- unique(owid_recent_sub$id)
  find_second <- setdiff(unique(find_recent_sub$id), owid_first)

  all_recent_final <- all_recent %>%
    mutate(
      preferred_source = case_when(
        id %in% owid_first ~ "OWID",
        id %in% find_second ~ "FIND",
        TRUE ~ NA_character_
      ),
      new_tests_daily14 = case_when(
        preferred_source == "OWID" ~ owid_tests_daily14,
        preferred_source == "FIND" ~ find_tests_daily14
      ),
      new_tests_daily14_prev = case_when(
        preferred_source == "OWID" ~ owid_tests_daily14_prev,
        preferred_source == "FIND" ~ find_tests_daily14_prev
      ),
      new_tests_daily14_1k = case_when(
        preferred_source == "OWID" ~ owid_tests_daily14_1k,
        preferred_source == "FIND" ~ find_tests_daily14_1k
      ),
      new_tests_daily14_1k_prev = case_when(
        preferred_source == "OWID" ~ owid_tests_daily14_1k_prev,
        preferred_source == "FIND" ~ find_tests_daily14_1k_prev
      ),
      preferred_recent_date = case_when(
        preferred_source == "OWID" ~ as.Date(owid_date),
        preferred_source == "FIND" ~ as.Date(find_date),
        TRUE ~ as.Date(NA)
      ),
      flag_negative_tests_previous14 = case_when(
        preferred_source == "OWID" ~ owid_flag_negative_tests_prev14,
        preferred_source == "FIND" ~ find_flag_negative_tests_prev14
      ),
      flag_increase_tests_previous14 = case_when(
        preferred_source == "OWID" ~ owid_flag_increase_tests_prev14,
        preferred_source == "FIND" ~ find_flag_increase_tests_prev14
      )
    ) %>%
    relocate(
      id, preferred_source, preferred_recent_date, new_tests_daily14:new_tests_daily14_1k_prev,
      starts_with("flag_")
    )


  return(all_recent_final)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_preferred_testpos7
#' @description Evaluates testing data sources for quality based on new tests per 1K and test positivity over the last 7 days providing a preferred choice between the two sources, OWID or FIND
#' OUTPUT:
# - Function argument test_long is the output from get_testing_long()
# - Most recent 7-day average data points for new tests per 1K AND positivity rate for use in risk matrix plots by WHO region.
# - After accounting for flags, we pick OWID by default when both OWID and FIND are available in last X days
# - Subject to future revisions
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tests_preferred <- get_preferred_testpos7(test_long, last_X_days = 14, analysis_date = analysis_date)
#' tests_preferred <- get_preferred_testpos7(test_long, last_X_days = 14, analysis_date = Sys.Date - 1)
#' }
#'
get_preferred_testpos7 <- function(test_long, last_X_days = 14, analysis_date = NULL) {
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date() - 1
  }
  X_date <- analysis_date - last_X_days

  # Calculate most recent date based on what the analytical date is.
  owid_df <- test_long %>%
    filter(data_source == "OWID") %>%
    filter(date <= analysis_date) %>%
    # Correct FLAG for 8 countries which only report new tests daily, not cumulative
    mutate(
      FLAG_increase_tests_7day = replace(FLAG_increase_tests_7day, is.na(FLAG_increase_tests_7day), 0),
      FLAG_increase_tests_7day_prev = replace(FLAG_increase_tests_7day_prev, is.na(FLAG_increase_tests_7day_prev), 0)
    )

  find_df <- test_long %>%
    filter(data_source == "FIND") %>%
    filter(date <= analysis_date)

  owid_recent_both <- owid_df %>%
    filter(!is.na(new_tests_daily7_per_1k) & !is.na(positive_rate_7day)) %>%
    group_by(id) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()

  owid_recent_tests <- owid_df %>%
    filter(!is.na(new_tests_daily7_per_1k)) %>%
    group_by(id) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()

  owid_test_diff <- setdiff(owid_recent_tests$id, owid_recent_both$id)
  # Why does OWID not calculate positivity for Iceland, Qatar?
  # Likely the test and case definitions are known to differ

  owid_recent <-
    rbind(
      owid_recent_both,
      owid_recent_tests %>% filter(id %in% owid_test_diff)
    )

  find_recent_both <- find_df %>%
    filter(!is.na(new_tests_daily7_per_1k) & !is.na(positive_rate_7day)) %>%
    group_by(id) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()

  find_recent_tests <- find_df %>%
    filter(!is.na(new_tests_daily7_per_1k)) %>%
    group_by(id) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()

  find_test_diff <- setdiff(find_recent_tests$id, find_recent_both$id)
  # For some countries, FIND does not have case data
  # Some of these do exist (e.g., HKG) so it's not clear why
  # TODO: Later, we may want to pull in JHU case data on our own rather than rely on FIND's data

  find_recent <-
    rbind(
      find_recent_both,
      find_recent_tests %>% filter(id %in% find_test_diff)
    )

  # Try a full join between them, rename variables as needed
  all_recent <-
    full_join(
      owid_recent %>%
        select(id,
          owid_date = date,
          owid_new_cases = new_cases_original,
          owid_new_cases_daily7 = new_cases_daily7,
          owid_total_tests_orig = total_tests_original,
          owid_total_tests_int = total_tests_int,
          owid_new_tests_orig = new_tests_original,
          owid_new_tests_int = new_tests_int,
          owid_tests_daily7 = new_tests_daily7,
          owid_tests_daily7_prev = new_tests_daily7_prev,
          owid_tests_daily7_1k = new_tests_daily7_per_1k,
          owid_tests_daily7_1k_prev = new_tests_daily7_per_1k_prev,
          owid_positive_rate = positive_rate_7day,
          owid_positive_rate_prev = positive_rate_7day_prev,
          owid_flag_negative_tests = FLAG_negative_tests_7day,
          owid_flag_increase_tests = FLAG_increase_tests_7day,
          owid_flag_negative_cases = FLAG_negative_cases_7day,
          owid_flag_negative_tests_prev7 = FLAG_negative_tests_7day_prev,
          owid_flag_increase_tests_prev7 = FLAG_increase_tests_7day_prev,
          owid_flag_negative_cases_prev7 = FLAG_negative_cases_7day_prev,
          owid_tests_units = tests_units, owid_test_definition = test_definition,
          owid_case_definition = case_definition,
          owid_posrate_definition = posrate_definition
        ),
      find_recent %>%
        select(id,
          find_date = date,
          find_new_cases = new_cases_original,
          find_new_cases_daily7 = new_cases_daily7,
          find_total_tests = total_tests_original,
          find_total_tests_int = total_tests_int,
          find_new_tests_orig = new_tests_original,
          find_new_tests_int = new_tests_int,
          find_tests_daily7 = new_tests_daily7,
          find_tests_daily7_prev = new_tests_daily7_prev,
          find_tests_daily7_1k = new_tests_daily7_per_1k,
          find_tests_daily7_1k_prev = new_tests_daily7_per_1k_prev,
          find_positive_rate = positive_rate_7day,
          find_positive_rate_prev = positive_rate_7day_prev,
          find_flag_negative_tests = FLAG_negative_tests_7day,
          find_flag_increase_tests = FLAG_increase_tests_7day,
          find_flag_negative_cases = FLAG_negative_cases_7day,
          find_flag_negative_tests_prev7 = FLAG_negative_tests_7day_prev,
          find_flag_increase_tests_prev7 = FLAG_increase_tests_7day_prev,
          find_flag_negative_cases_prev7 = FLAG_negative_cases_7day_prev,
          find_tests_units = tests_units, find_test_definition = test_definition,
          find_case_definition = case_definition,
          find_posrate_definition = posrate_definition
        ),
      by = "id"
    )

  owid_recent_sub <- owid_recent %>%
    filter(date >= X_date) %>%
    filter(FLAG_increase_tests_7day == 0 & FLAG_negative_tests_7day == 0 &
      FLAG_negative_cases_7day == 0)

  find_recent_sub <- find_recent %>%
    filter(date >= X_date) %>%
    filter(FLAG_increase_tests_7day == 0 & FLAG_negative_tests_7day == 0 &
      # Some locations in FIND do not have case data -- can include still
      (FLAG_negative_cases_7day == 0 | is.na(FLAG_negative_cases_7day)))

  owid_first <- unique(owid_recent_sub$id)
  find_second <- setdiff(unique(find_recent_sub$id), owid_first)


  all_recent_final <- all_recent %>%
    mutate(
      preferred_source = case_when(
        id %in% owid_first ~ "OWID",
        id %in% find_second ~ "FIND",
        TRUE ~ NA_character_
      ),
      new_tests_daily7 = case_when(
        preferred_source == "OWID" ~ owid_tests_daily7,
        preferred_source == "FIND" ~ find_tests_daily7
      ),
      new_tests_daily7_prev = case_when(
        preferred_source == "OWID" ~ owid_tests_daily7_prev,
        preferred_source == "FIND" ~ find_tests_daily7_prev
      ),
      new_tests_daily7_1k = case_when(
        preferred_source == "OWID" ~ owid_tests_daily7_1k,
        preferred_source == "FIND" ~ find_tests_daily7_1k
      ),
      new_tests_daily7_1k_prev = case_when(
        preferred_source == "OWID" ~ owid_tests_daily7_1k_prev,
        preferred_source == "FIND" ~ find_tests_daily7_1k_prev
      ),
      positive_rate = case_when(
        preferred_source == "OWID" ~ owid_positive_rate,
        preferred_source == "FIND" ~ find_positive_rate
      ),
      positive_rate_prev = case_when(
        preferred_source == "OWID" ~ owid_positive_rate_prev,
        preferred_source == "FIND" ~ find_positive_rate_prev
      ),
      preferred_recent_date = case_when(
        preferred_source == "OWID" ~ as.Date(owid_date),
        preferred_source == "FIND" ~ as.Date(find_date),
        TRUE ~ as.Date(NA)
      ),
      flag_negative_tests_previous7 = case_when(
        preferred_source == "OWID" ~ owid_flag_negative_tests_prev7,
        preferred_source == "FIND" ~ find_flag_negative_tests_prev7
      ),
      flag_increase_tests_previous7 = case_when(
        preferred_source == "OWID" ~ owid_flag_increase_tests_prev7,
        preferred_source == "FIND" ~ find_flag_increase_tests_prev7
      ),
      flag_negative_cases_previous7 = case_when(
        preferred_source == "OWID" ~ owid_flag_negative_cases_prev7,
        preferred_source == "FIND" ~ find_flag_negative_cases_prev7
      )
    ) %>%
    relocate(
      id, preferred_source, preferred_recent_date, positive_rate, positive_rate_prev,
      new_tests_daily7:new_tests_daily7_1k_prev, starts_with("flag_")
    )


  return(all_recent_final)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Get testing data from OWID or FIND
#' @description Pulls testing data from either OWID or FIND sources based on a 7d testing / 1K and positivity rate.
#'
#' - Most recent 7-day average data points for new tests per 1K AND positivity rate for use in risk matrix plots by WHO region.
#'
#' - After accounting for flags, we pick OWID by default when both OWID and FIND are available in last X days
#'
#' @param analysis_date (date default: Sys.Date() - 1L) Start date from which to choose OWID or FIND data
#' @return A data frame with n rows and 5 variables:
#' \describe{
#'   \item{\code{id}}{  character ISO 3166-1 alpha-3 country code}
#'   \item{\code{date}}{  date Date of testing observation}
#'   \item{\code{new_tests_smoothed_per_thousand}}{  double Tests / 1000 population (7d smooth)}
#'   \item{\code{new_tests_smoothed_per_thousand_14}}{  double Tests / 1000 population (14d smooth)}
#'   \item{\code{positive_rate}}{  double Test positivity rate (7d smooth)}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' testing <- get_testing()
#' }
#'
get_testing <- function(analysis_date = Sys.Date() - 1L) {

  # Create testing data frame
  testing_long <- get_testing_long()
  preferred <- get_preferred_testpos7(testing_long, last_X_days = 14, analysis_date = analysis_date)
  preferred_long <- testing_long %>%
    left_join(preferred %>% select(id, preferred_source), by = "id") %>%
    filter(data_source == preferred_source)
  # Time Series based on Preferred-Source Data Frame
  preferred_long_locf <- preferred_long %>%
    group_by(id) %>%
    arrange(date) %>%
    mutate(
      new_tests_smoothed_per_thousand = zoo::na.locf(new_tests_daily7_per_1k, na.rm = F, maxgap = 14),
      new_tests_smoothed_per_thousand_14 = zoo::na.locf(new_tests_daily14_per_1k, na.rm = F, maxgap = 14),
      positive_rate = zoo::na.locf(positive_rate_7day, na.rm = F, maxgap = 14)
    ) %>%
    ungroup(.) %>%
    select(id, date, new_tests_smoothed_per_thousand, new_tests_smoothed_per_thousand_14, positive_rate)

  return(preferred_long_locf)
}
