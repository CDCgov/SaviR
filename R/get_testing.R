#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_owid_meta
#' @description To scrape the OWID coronavirus testing website for data related to the source and definitions of their testing data by country
#' OUTPUT: Data set of scraped information related to data definitions and sources by country
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing_df <- get_testing() }
#'

get_owid_meta <- function(url = "https://ourworldindata.org/coronavirus-testing") {
  
  ### Data scraping technique ###
  #url = "https://ourworldindata.org/coronavirus-testing"
  content <- xml2::read_html(url)
  
  #Get List of Countries from HTML Table
  tables      <- content %>% rvest::html_table(fill = TRUE)
  first_table <- tables[[1]]
  countries   <- unique(unlist(first_table))
  countries   <- countries[!is.na(countries)]
  
  #Selector under which all the data is located
  root.selector <- "body > main > article > div.content-wrapper > div.offset-content > div > div > section:nth-child(5)"
  n.children    <- content %>% #Number of children the selector has
                   rvest::html_elements(css=root.selector) %>% 
                   rvest::html_children() %>%
                   length()
                
  #Identify elements with information
  element.list <- seq(14,n.children-26, by=2)
          #Country data starts on element 14
          #Last 26 elements are not country data but general footnotes
          #Country headings & country names are in the even elements
          #Country data are in the odd elements
  
  ### Loop scraping data by country ###
  data <- data.frame()
  for (i in element.list) {
    
    #Get Country Name (uses elements i)
    selector.country <- paste0(root.selector, " > div:nth-child(", i, ") > div:nth-child(1)")
    country.name     <- content %>%
                        html_element(css=selector.country) %>%
                        html_text
    
    #Get Data (uses elements i+1)
    selector.data <- paste0(root.selector, " > div:nth-child(", i+1, ") > div:nth-child(1)") # " > p:nth-child(4)"
    children      <- content %>%
                      html_elements(css=selector.data) %>%
                      html_children() %>%
                      html_text()
    
    #Extract elements with data
    test.def <- children[grepl("Test definition:",children, fixed=T)]
    case.def <- children[grepl("Case definition:",children, fixed=T)]
    pos.rate <- children[grepl("Positive rate:",  children, fixed=T)]
    
    #Make NA if there are no matching elements
    if (length(test.def)==0) test.def <- NA
    if (length(case.def)==0) case.def <- NA
    if (length(pos.rate)==0) pos.rate <- NA
    
    #Create Data Frame
    data.i <- data.frame(country  = country.name,
                         test.def = test.def,
                         case.def = case.def,
                         pos.rate = pos.rate)
    data <- data %>% dplyr::bind_rows(data.i)
  }
  
  # Split on "Test definition: ", "Case definition: ", "Positive rate: "
  data_cleaned <- data %>% dplyr::filter(!is.na(country))
  
  data_cleaned$test_definition <- trimws(stringr::str_split_fixed(data_cleaned$test.def, ":", n = 2)[, 2])
  data_cleaned$case_definition <- trimws(stringr::str_split_fixed(data_cleaned$case.def, ":", n = 2)[, 2])
  data_cleaned$posrate_definition <- trimws(stringr::str_split_fixed(data_cleaned$pos.rate, ":", n = 2)[, 2])
  data_cleaned <- data_cleaned %>% 
                  dplyr::relocate(test.def, test_definition, 
                                  case.def, case_definition,
                                  pos.rate, posrate_definition)
  
  # Add ISO-3 codes for easy merging with other datasets
  owid_meta <- data_cleaned %>%
               dplyr::select(-test.def, -case.def, -pos.rate) %>%
               dplyr::mutate(
                             test_definition = replace(test_definition, test_definition == "", NA_character_),
                             case_definition = replace(case_definition, case_definition == "", NA_character_),
                             iso_code = passport::parse_country(country, to = "iso3c"),
                             iso_code = ifelse(country == "Timor", "TLS", iso_code)) %>%
               dplyr::relocate(country, iso_code, test_definition, case_definition, posrate_definition)
  
  # Note some duplicated countries for some reason
  owid_meta2 <- 
               dplyr::left_join(owid_meta,
                                owid_meta %>% 
               dplyr::group_by(iso_code) %>% 
               dplyr::count(), by = "iso_code")
  
  # For Ecuador, we get 2 different test definitions. 
  # -- this is because the first one is correct, the second one is actually El Salvador
  # The webpage link for El Salvador is incorrect
  # Canada has 2 sources
  # Poland has 2 sources
  # Italy has 2 sources
  
  # If Ecuador is not counted twice, throw an error -- need to change function
  owid_mult <- owid_meta2 %>% dplyr::filter(n > 1)
 
   if (!("ECU" %in% owid_mult$iso_code)) {
    stop("Function assumes multiple Ecuador entries -- check function code")
  }
  
  owid_meta3 <- owid_meta2 %>%
                dplyr::mutate(
                              iso_code_old = iso_code,
                              country_duplicated = duplicated(iso_code_old),
                              iso_code = if_else(iso_code_old == "ECU" & country_duplicated == TRUE,
                                                 "SLV", iso_code_old),
                              country_duplicated2 = duplicated(iso_code)) %>%
                dplyr::filter(!country_duplicated2)
  
  owid_meta_final <- owid_meta3 %>%
                     dplyr::select(iso_code:posrate_definition) %>%
                     dplyr::arrange(iso_code) %>%
                     dplyr::mutate(
                                   posrate_direct = ifelse(str_detect(posrate_definition, "collected directly"), T, F))
  
  return(owid_meta_final)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_testing_long
#' @description Download and combine full time series data related to testing as compiled by Our World in Data and FIND
#' OUTPUT: Longitudinal data set with both FIND and OWID data sets with consistent definitions
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing_long <- get_testing_long()}
#'

get_testing_long <- function(owid_all_source = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", 
                             owid_test_source = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv",
                             find_source = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv",
                             find_meta_source = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/unit_info.csv",
                             find_maxgap = 31, # Gap between cumulative testing number to linearly interpolate
                             flag_test_increase = 5 # Flag for increase in interpolated cumulative tests
                             ) {
  

  
 # Download various OWID/FIND datasets
 ## The testing-specific OWID dataset may be more up to date than the overall OWID dataset
 testing_OWID <-
               data.table::fread(owid_test_source, data.table = F, showProgress = F, verbose = F) %>%
               dplyr::filter(!(Entity %in% c("Poland - samples tested", "Italy - people tested", 
                                      "Canada - people tested"))) %>%
               dplyr::mutate(date = as.Date(Date)) %>%
               dplyr::select(
                 iso_code = `ISO code`,
                 date,
                 test_source_url = `Source URL`,
                 total_tests = `Cumulative total`,
                 new_tests = `Daily change in cumulative total`,
                 new_tests_per_thousand = `Daily change in cumulative total per thousand`,
                 new_tests_smoothed = `7-day smoothed daily change`,
                 new_tests_smoothed_per_thousand = `7-day smoothed daily change per thousand`,
                 positive_rate = `Short-term positive rate`
               ) %>%
               dplyr::mutate(iso_code = dplyr::recode(iso_code, "OWID_KOS" = "XKX"))
 
 # Make sure you have just 1 observation per iso-code/date pairing
 n_test_iso <- testing_OWID %>% dplyr::count(iso_code, date) %>% dplyr::filter(n > 1)
 if (nrow(n_test_iso) > 0) {
   stop("Check testing dataset in get_testing_long() -- multiple values per country-date")
 }
  
 full_OWID <- 
             data.table::fread(owid_all_source, data.table = F, showProgress = F, verbose = F) %>%
             dplyr::mutate(date = as.Date(date)) %>%
             dplyr::mutate(iso_code = dplyr::recode(iso_code, "OWID_KOS" = "XKX")) %>%
             dplyr::filter(!grepl("OWID", iso_code)) %>%
             dplyr::select(iso_code, location, date, population, total_cases, new_cases, new_cases_smoothed,
                         tests_units)
 
 full_OWID_tests <- dplyr::full_join(full_OWID, testing_OWID, by = c("iso_code", "date"))
 
 full_FIND <- 
             data.table::fread(find_source, data.table = F, verbose = F, showProgress = F) %>%
             dplyr::filter(set == "country")
 
 find_meta <-
             data.table::fread(find_meta_source, data.table = F, verbose = F, showProgress = F) %>%
             dplyr::filter(set == "country") %>%
             dplyr::select(iso_code = unit,
                    tests_units = tests_description,
                    test_definition = tests_type) %>%
             dplyr::mutate(case_definition = NA,
                    posrate_definition = NA)
           
 owid_meta <- get_owid_meta() %>%
              dplyr::rename(posrate_definition_old = posrate_definition) %>%
              dplyr::mutate(
                            posrate_definition = ifelse(
                            str_detect(posrate_definition_old, "calculated by Our World"), "7-day cases / tests",
                            ifelse(str_detect(posrate_definition_old, "not calculated"), "Not calculated",
                                    "Directly from source"))) %>%
             dplyr::select(-posrate_definition_old)
             
 #### Clean OWID ####
 owid_countries <- full_OWID_tests %>%
   # Note -- some countries don't report cumulative, but instead have daily new test
   # See for example: https://ourworldindata.org/coronavirus-testing#sweden
   dplyr::filter(!(is.na(total_tests) & is.na(new_tests)))
 
 iso_countries <- rbind(
                  iso_nocumtest <- owid_countries %>% dplyr::filter(is.na(total_tests) & !is.na(new_tests)) %>%
                                   dplyr::group_by(iso_code) %>% dplyr::summarize(n_obs = n(), cumtest_available = F),
                  iso_cumtest <- owid_countries %>% dplyr::filter(!is.na(total_tests)) %>% 
                                 dplyr::group_by(iso_code) %>% dplyr::summarize(n_obs = n(), cumtest_available = T) )
 
 # Full series for countries with any new tests or cumulative tests data at any point in time
 owid_data <- full_OWID_tests %>%
 # Use above to limit to just the countries where there is at least some OWID data
             dplyr::filter(iso_code %in% iso_countries$iso_code) %>%
             dplyr::left_join(iso_countries %>% dplyr::select(iso_code, cumtest_available),
                       by = "iso_code") %>%
             dplyr::select(-test_source_url) %>%
             dplyr::group_by(iso_code) %>%
             dplyr::arrange(date) %>%
             dplyr::mutate(
                    total_tests_int = zoo::na.approx(total_tests, na.rm = F, maxgap = find_maxgap),
                    new_tests_int   = total_tests_int - dplyr::lag(total_tests_int, 1), 
                    # NOTE: 8 countries report new tests to OWID instead of cumulative tests -- these are still valid
                    new_tests_int2  = ifelse(is.na(total_tests) & is.na(new_tests_int) & 
                                              !is.na(new_tests), new_tests, new_tests_int),
                    # Flag if the total tests decreased in past X days due to data revision
                    test_neg_value = ifelse(new_tests_int2 < 0, 1, 0), # Does new tests ever decrease?
                    test_any_neg_7day = zoo::rollsumr(test_neg_value, 7, fill = NA), 
                    test_any_neg_14day = zoo::rollsumr(test_neg_value, 14, fill = NA),
                    FLAG_negative_tests_7day = ifelse(test_any_neg_7day > 0, 1, 0),
                    FLAG_negative_tests_14day = ifelse(test_any_neg_14day > 0, 1, 0),
                    FLAG_negative_tests_7day_prev = dplyr::lag(FLAG_negative_tests_7day, 7),
                    FLAG_negative_tests_14day_prev = dplyr::lag(FLAG_negative_tests_14day, 14),
                    # Flag if total cases decreased in past X days due to data revision
                    neg_case = ifelse(new_cases < 0, 1, 0), 
                    case_any_neg_7day = zoo::rollsumr(neg_case, 7, fill = NA),
                    case_any_neg_14day = zoo::rollsumr(neg_case, 14, fill = NA),
                    FLAG_negative_cases_7day = ifelse(case_any_neg_7day > 0, 1, 0),
                    FLAG_negative_cases_14day = ifelse(case_any_neg_14day > 0, 1, 0),
                    FLAG_negative_cases_7day_prev = dplyr::lag(FLAG_negative_cases_7day, 7),
                    FLAG_negative_cases_14day_prev = dplyr::lag(FLAG_negative_cases_14day, 14),
                    # Flag for large increases that are implausible based on interpolated cumulative tests
                    cum_test_increase = (total_tests_int/dplyr::lag(total_tests_int, 1) - 1) * 100,
                    cum_test_increase_gt = ifelse(cum_test_increase > flag_test_increase, 1, 0), 
                    increase_any_test_7day = zoo::rollsumr(cum_test_increase_gt, 7, fill = NA),
                    increase_any_test_14day = zoo::rollsumr(cum_test_increase_gt, 14, fill = NA),
                    FLAG_increase_tests_7day = ifelse(increase_any_test_7day > 0, 1, 0),
                    FLAG_increase_tests_14day = ifelse(increase_any_test_14day > 0, 1, 0),
                    FLAG_increase_tests_7day_prev = dplyr::lag(FLAG_increase_tests_7day, 7),
                    FLAG_increase_tests_14day_prev = dplyr::lag(FLAG_increase_tests_14day, 14),
                    # Construct X-day average of new cases based on interpolated series
                    new_tests_daily14 = zoo::rollmeanr(new_tests_int2, 14, fill = NA),
                    new_tests_daily14_prev = dplyr::lag(new_tests_daily14, 14),
                    new_tests_daily14_per_1k = (new_tests_daily14 / population) * 1000,
                    new_tests_daily14_per_1k_prev = dplyr::lag(new_tests_daily14_per_1k, 14),
                    new_tests_daily7 = new_tests_smoothed,
                    new_tests_daily7_prev = dplyr::lag(new_tests_daily7, 7),
                    new_tests_daily7_per_1k = new_tests_smoothed_per_thousand,
                    new_tests_daily7_per_1k_prev = dplyr::lag(new_tests_daily7_per_1k, 7),
                    # 7-day and 14-day average cases
                    new_cases_daily7 = zoo::rollmeanr(new_cases, k = 7, fill = NA),
                    new_cases_daily14 = zoo::rollmeanr(new_cases, k = 14, fill = NA),
                    # NOTE: Positivity rate as provided in OWID is based on 7-day average cases / 7-day average tests
                    # Sometimes they do NOT use JHU case data as numerator, so it's not exactly reproducible 
                    # This also means I cannot calculate the 14-day version for these 12 or so countries
                    # In OWID meta data, see posrate_definition column
                    positive_rate_7day = positive_rate,
                    positive_rate_7day_prev = dplyr::lag(positive_rate, 7) ) %>%
             dplyr::ungroup()
 
 #### Clean FIND ####
 find_countries <- full_FIND %>%
   # Focus on the original recorded metrics of cumulative tests and new tests
   # Use these to construct our own metrics based on specifications
                   dplyr::filter(!(is.na(cum_tests_orig) & is.na(new_tests_orig))) %>%
                   dplyr::group_by(unit) %>%
                   dplyr::summarize(n_obs = n())
 
 find_data <- full_FIND %>%
               dplyr::filter(unit %in% find_countries$unit) %>%
               dplyr::select(-set) %>%
               dplyr::rename(date = time, 
                      iso_code = unit) %>%
               dplyr::mutate(date = as.Date(date)) %>%
               dplyr::mutate(iso_code = dplyr::recode(iso_code, "XK" = "XKX"))
             
 find_data2 <- find_data %>%
               dplyr::group_by(iso_code) %>%
               dplyr::arrange(date) %>%
               dplyr::mutate(
                 # First, replace 0s with NAs
                 new_tests_nafor0 = replace(new_tests_orig, new_tests_orig == 0, NA),
                 cum_tests_alt1 = ifelse(new_tests_orig == 0, NA, cum_tests_orig),
                 # Linearly interpolate values -- we can set a max value at which point we don't interpolate
                 total_tests_int = zoo::na.approx(cum_tests_alt1, na.rm = F, maxgap = find_maxgap),
                 new_tests_int = total_tests_int - dplyr::lag(total_tests_int, 1),
                 # Construct average daily series for new tests
                 new_tests_daily14 = zoo::rollmeanr(new_tests_int, k = 14, fill = NA),
                 new_tests_daily14_prev = dplyr::lag(new_tests_daily14, 14),
                 new_tests_daily14_per_1k = (new_tests_daily14 / (pop_100k*100000)) *1000,
                 new_tests_daily14_per_1k_prev = dplyr::lag(new_tests_daily14_per_1k, 14),
                 new_tests_daily7 = zoo::rollmeanr(new_tests_int, k = 7, fill = NA),
                 new_tests_daily7_prev = dplyr::lag(new_tests_daily7, 7),
                 new_tests_daily7_per_1k = (new_tests_daily7 / (pop_100k*100000)) *1000,
                 new_tests_daily7_per_1k_prev = dplyr::lag(new_tests_daily7_per_1k, 7),
                 # Cases 7-day and 14-day based on new cases column
                 new_cases_daily7 = zoo::rollmeanr(new_cases_orig, k = 7, fill = NA),
                 new_cases_daily14 = zoo::rollmeanr(new_cases_orig, k = 14, fill = NA),
                 # Positivity rates based on 7-day and 14-day cases & tests
                 positive_rate_7day =  new_cases_daily7 / new_tests_daily7,
                 positive_rate_7day_prev = dplyr::lag(positive_rate_7day, 7),
                 positive_rate_14day = new_cases_daily14 / new_tests_daily14,
                 positive_rate_14day_prev = dplyr::lag(positive_rate_14day, 14),
                 # FLAG: total cases decreased in past X days due to data revision
                 neg_case = ifelse(new_cases_orig < 0, 1, 0), 
                 case_any_neg_7day = zoo::rollsumr(neg_case, 7, fill = NA),
                 case_any_neg_14day = zoo::rollsumr(neg_case, 14, fill = NA),
                 FLAG_negative_cases_7day = ifelse(case_any_neg_7day > 0, 1, 0),
                 FLAG_negative_cases_14day = ifelse(case_any_neg_14day > 0, 1, 0),
                 FLAG_negative_cases_7day_prev = dplyr::lag(FLAG_negative_cases_7day, 7),
                 FLAG_negative_cases_14day_prev = dplyr::lag(FLAG_negative_cases_14day, 14),
                 # FLAG: drop in cumulative tests
                 test_neg_value = ifelse(new_tests_orig < 0, 1, 0),
                 test_any_neg_7day = zoo::rollsumr(test_neg_value, 7, fill = NA), 
                 test_any_neg_14day = zoo::rollsumr(test_neg_value, 14, fill = NA),
                 FLAG_negative_tests_7day = ifelse(test_any_neg_7day > 0, 1, 0),
                 FLAG_negative_tests_14day = ifelse(test_any_neg_14day > 0, 1, 0),
                 FLAG_negative_tests_7day_prev = dplyr::lag(FLAG_negative_tests_7day, 7),
                 FLAG_negative_tests_14day_prev = dplyr::lag(FLAG_negative_tests_14day, 14),
                 # FLAG: substantial increase in tests
                 cum_test_increase = (total_tests_int/dplyr::lag(total_tests_int, 1) - 1) * 100,
                 cum_test_increase_gt = ifelse(cum_test_increase > flag_test_increase, 1, 0),
                 increase_any_test_7day = zoo::rollsumr(cum_test_increase_gt, 7, fill = NA),
                 increase_any_test_14day = zoo::rollsumr(cum_test_increase_gt, 14, fill = NA),
                 FLAG_increase_tests_7day = ifelse(increase_any_test_7day > 0, 1, 0),
                 FLAG_increase_tests_14day = ifelse(increase_any_test_14day > 0, 1, 0),
                 FLAG_increase_tests_7day_prev = dplyr::lag(FLAG_increase_tests_7day, 7),
                 FLAG_increase_tests_14day_prev = dplyr::lag(FLAG_increase_tests_14day, 14),
               ) %>%
               dplyr::ungroup(.)
             
 # Create several longitudinal datasets -- save as RDS, excel, csv
 find_longdata <- dplyr::left_join(find_data2, find_meta, by = "iso_code") %>%
   dplyr::rename(
     total_tests_original = cum_tests_orig,
     new_tests_original = new_tests_orig,
     new_cases_original = new_cases_orig
   ) %>%
   dplyr::mutate(population = pop_100k * 100000,
          cumtest_available = T) %>%
   dplyr::select(iso_code, date, population, total_tests_original, new_tests_original,
          total_tests_int, new_tests_int,
          new_tests_daily7, new_tests_daily7_prev, new_tests_daily7_per_1k, new_tests_daily7_per_1k_prev,
          new_tests_daily14, new_tests_daily14_prev, new_tests_daily14_per_1k, new_tests_daily14_per_1k_prev,
          new_cases_original, new_cases_daily7, new_cases_daily14, 
          positive_rate_7day, positive_rate_7day_prev, 
          starts_with("FLAG_"),
          tests_units:posrate_definition, cumtest_available) %>%
   dplyr::arrange(iso_code, date)
 
 owid_longdata <- dplyr::left_join(owid_data, owid_meta, by = "iso_code") %>%
   dplyr::mutate(
     # When positivity rate is directly calculated from source, ignore the flag
     FLAG_negative_cases_7day = replace(FLAG_negative_cases_7day, posrate_direct == TRUE, 0),
     FLAG_negative_cases_14day = replace(FLAG_negative_cases_14day, posrate_direct == TRUE, 0),
     FLAG_negative_cases_7day_prev = replace(FLAG_negative_cases_7day, posrate_direct == TRUE, 0),
     FLAG_negative_cases_14day_prev = replace(FLAG_negative_cases_14day, posrate_direct == TRUE, 0)
   ) %>%
   dplyr::rename(
     total_tests_original = total_tests,
     new_tests_original = new_tests,
     new_cases_original = new_cases
   ) %>%
   dplyr::select(
     iso_code, date, population, 
     total_tests_original, new_tests_original,
     total_tests_int, new_tests_int = new_tests_int2, 
     new_tests_daily7, new_tests_daily7_prev, new_tests_daily7_per_1k, new_tests_daily7_per_1k_prev,
     new_tests_daily14, new_tests_daily14_prev, new_tests_daily14_per_1k, new_tests_daily14_per_1k_prev,
     new_cases_original, new_cases_daily7, new_cases_daily14, 
     positive_rate_7day, positive_rate_7day_prev, 
     starts_with("FLAG_"),
     tests_units, test_definition:posrate_definition, cumtest_available
   ) %>%
   dplyr::select(-posrate_direct) %>%
   dplyr::arrange(iso_code, date)
 
 # Combine into a single longitudinal dataset and output
 test_long <- 
   rbind(
     owid_longdata %>% 
       dplyr::mutate(data_source = "OWID"),
     find_longdata[, names(owid_longdata)] %>% 
       dplyr::mutate(data_source = "FIND")
   ) %>%
   dplyr::relocate(data_source, iso_code, date) %>%
   dplyr::arrange(data_source, iso_code, date)

 return(test_long)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
#' tests_preferred <- get_preferred_tests14(test_long, last_X_days = 14, analysis_date = analysis_date)}
#' tests_preferred <- get_preferred_tests14(test_long, last_X_days = 14, analysis_date = Sys.Date - 1)}
#'


get_preferred_tests14 <- function(test_long, last_X_days = 14, analysis_date = NULL) {
  
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date() - 1
  }

  X_date <- analysis_date - last_X_days
  
  # Calculate most recent date based on what the analytical date is.
  # e.g., we run on Monday, but only want data through Sunday, 
  #  so maxdate should be relative to this
  owid_df <- test_long %>% dplyr::filter(data_source == "OWID") %>% dplyr::filter(date <= analysis_date)
  find_df <- test_long %>% dplyr::filter(data_source == "FIND") %>% dplyr::filter(date <= analysis_date)

  owid_recent <- owid_df %>%
                dplyr::filter(!is.na(new_tests_daily14_per_1k)) %>%
                dplyr::group_by(iso_code) %>%
                dplyr::arrange(date) %>%  
                dplyr::slice_max(order_by = date, n = 1) %>%
                dplyr::ungroup() %>%
    # Correct FLAG for 8 countries which only report new tests daily, not cumulative
    dplyr::mutate(FLAG_increase_tests_14day = replace(FLAG_increase_tests_14day, is.na(FLAG_increase_tests_14day), 0))
  
  find_recent <- find_df %>%
                  dplyr::filter(!is.na(new_tests_daily14_per_1k)) %>%
                  dplyr::group_by(iso_code) %>%
                  dplyr::arrange(date) %>%  
                  dplyr::slice_max(order_by = date, n = 1) %>%
                  dplyr::ungroup()
  
  # Try a full join between them, rename variables as needed
  all_recent <- 
    dplyr::full_join(
      owid_recent %>%
        dplyr::select(iso_code, 
               owid_date               = date, 
               owid_total_tests_orig   = total_tests_original,
               owid_total_tests_int    = total_tests_int, 
               owid_new_tests_orig     = new_tests_original,
               owid_new_tests_int      = new_tests_int,
               owid_tests_daily14      = new_tests_daily14,
               owid_tests_daily14_prev = new_tests_daily14_prev,
               owid_tests_daily14_1k   = new_tests_daily14_per_1k,
               owid_tests_daily14_1k_prev      = new_tests_daily14_per_1k_prev,
               owid_flag_negative_tests        = FLAG_negative_tests_14day, 
               owid_flag_increase_tests        = FLAG_increase_tests_14day,
               owid_flag_negative_tests_prev14 = FLAG_negative_tests_14day_prev,
               owid_flag_increase_tests_prev14 = FLAG_increase_tests_14day_prev,
               owid_tests_units                = tests_units, 
               owid_test_definition            = test_definition),
      find_recent %>%
        dplyr::select(iso_code, 
               find_date               = date, 
               find_total_tests        = total_tests_original,
               find_total_tests_int    = total_tests_int, 
               find_new_tests_orig     = new_tests_original,
               find_new_tests_int      = new_tests_int,
               find_tests_daily14      = new_tests_daily14,
               find_tests_daily14_prev = new_tests_daily14_prev,
               find_tests_daily14_1k   = new_tests_daily14_per_1k,
               find_tests_daily14_1k_prev      = new_tests_daily14_per_1k_prev,
               find_flag_negative_tests        = FLAG_negative_tests_14day, 
               find_flag_increase_tests        = FLAG_increase_tests_14day,
               find_flag_negative_tests_prev14 = FLAG_negative_tests_14day_prev,
               find_flag_increase_tests_prev14 = FLAG_increase_tests_14day_prev,
               find_tests_units                = tests_units, 
               find_test_definition            = test_definition),
      by = "iso_code"
    )
    
  owid_recent_sub <- owid_recent %>% 
    dplyr::filter(date >= X_date) %>%
    dplyr::filter(FLAG_increase_tests_14day == 0 & FLAG_negative_tests_14day == 0)
  
  find_recent_sub <- find_recent %>%
    dplyr::filter(date >= X_date) %>%
    dplyr::filter(FLAG_increase_tests_14day == 0 & FLAG_negative_tests_14day == 0)
  
  owid_first <- unique(owid_recent_sub$iso_code)
  find_second <- setdiff(unique(find_recent_sub$iso_code), owid_first)
  
  all_recent_final <- all_recent %>%
    dplyr::mutate(
      preferred_source = ifelse(iso_code %in% owid_first, "OWID", 
                                ifelse(iso_code %in% find_second, "FIND", NA)),
      new_tests_daily14 = ifelse(preferred_source == "OWID", owid_tests_daily14, 
                                 ifelse(preferred_source == "FIND", find_tests_daily14, NA)),
      new_tests_daily14_prev = ifelse(preferred_source == "OWID", owid_tests_daily14_prev, 
                                 ifelse(preferred_source == "FIND", find_tests_daily14_prev, NA)),
      new_tests_daily14_1k = ifelse(preferred_source == "OWID", owid_tests_daily14_1k, 
                                 ifelse(preferred_source == "FIND", find_tests_daily14_1k, NA)),
      new_tests_daily14_1k_prev = ifelse(preferred_source == "OWID", owid_tests_daily14_1k_prev, 
                                      ifelse(preferred_source == "FIND", find_tests_daily14_1k_prev, NA)),
      preferred_recent_date = as.Date(ifelse(preferred_source == "OWID", owid_date, 
                                     ifelse(preferred_source == "FIND", find_date, NA))),
      flag_negative_tests_previous14 = ifelse(preferred_source == "OWID", owid_flag_negative_tests_prev14,
                                              ifelse(preferred_source == "FIND", find_flag_negative_tests_prev14, NA)),
      flag_increase_tests_previous14 = ifelse(preferred_source == "OWID", owid_flag_increase_tests_prev14,
                                              ifelse(preferred_source == "FIND", find_flag_increase_tests_prev14, NA))
    ) %>%
    dplyr::relocate(iso_code, preferred_source, preferred_recent_date, new_tests_daily14:new_tests_daily14_1k_prev,
             starts_with("flag_"))
  

  return(all_recent_final)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
#' tests_preferred <- get_preferred_testpos7(test_long, last_X_days = 14, analysis_date = analysis_date)}
#' tests_preferred <- get_preferred_testpos7(test_long, last_X_days = 14, analysis_date = Sys.Date - 1)}
#'


get_preferred_testpos7 <- function(test_long, last_X_days = 14, analysis_date = NULL) {
  
  if (is.null(analysis_date)) {
    analysis_date <- Sys.Date() - 1
  }
  X_date <- analysis_date - last_X_days
  
  # Calculate most recent date based on what the analytical date is.
  owid_df <- test_long %>% 
    dplyr::filter(data_source == "OWID") %>% 
    dplyr::filter(date <= analysis_date) %>%
    # Correct FLAG for 8 countries which only report new tests daily, not cumulative
    dplyr::mutate(FLAG_increase_tests_7day = replace(FLAG_increase_tests_7day, is.na(FLAG_increase_tests_7day), 0),
                  FLAG_increase_tests_7day_prev = replace(FLAG_increase_tests_7day_prev, is.na(FLAG_increase_tests_7day_prev), 0))
  
  find_df <- test_long %>% 
    dplyr::filter(data_source == "FIND") %>% 
    dplyr::filter(date <= analysis_date)
  
  owid_recent_both <- owid_df %>%
    dplyr::filter(!is.na(new_tests_daily7_per_1k) & !is.na(positive_rate_7day)) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::arrange(date) %>%  
    dplyr::slice_max(order_by = date, n = 1) %>%
    dplyr::ungroup()
  
  owid_recent_tests <- owid_df %>%
    dplyr::filter(!is.na(new_tests_daily7_per_1k)) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::arrange(date) %>%  
    dplyr::slice_max(order_by = date, n = 1) %>%
    dplyr::ungroup()
  
  owid_test_diff <- setdiff(owid_recent_tests$iso_code, owid_recent_both$iso_code)
  # Why does OWID not calculate positivity for Iceland, Qatar?
  # Likely the test and case definitions are known to differ
  
  owid_recent <- 
    rbind(
      owid_recent_both,
      owid_recent_tests %>% dplyr::filter(iso_code %in% owid_test_diff)
    )
  
  find_recent_both <- find_df %>%
    dplyr::filter(!is.na(new_tests_daily7_per_1k) & !is.na(positive_rate_7day)) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::arrange(date) %>%  
    dplyr::slice_max(order_by = date, n = 1) %>%
    dplyr::ungroup()
  
  find_recent_tests <- find_df %>%
    dplyr::filter(!is.na(new_tests_daily7_per_1k)) %>%
    dplyr::group_by(iso_code) %>%
    dplyr::arrange(date) %>%  
    dplyr::slice_max(order_by = date, n = 1) %>%
    dplyr::ungroup()
  
  find_test_diff <- setdiff(find_recent_tests$iso_code, find_recent_both$iso_code)
  # For some countries, FIND does not have case data
  # Some of these do exist (e.g., HKG) so it's not clear why
  # TODO: Later, we may want to pull in JHU case data on our own rather than rely on FIND's data
  
  find_recent <- 
    rbind(
      find_recent_both,
      find_recent_tests %>% dplyr::filter(iso_code %in% find_test_diff)
    )
  
  # Try a full join between them, rename variables as needed
  all_recent <- 
    dplyr::full_join(
      owid_recent %>%
        dplyr::select(iso_code, owid_date = date, 
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
                      owid_posrate_definition = posrate_definition),
      find_recent %>%
        dplyr::select(iso_code, find_date = date, 
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
                      find_posrate_definition = posrate_definition),
      by = "iso_code"
    )
  
  owid_recent_sub <- owid_recent %>% 
    dplyr::filter(date >= X_date) %>%
    dplyr::filter(FLAG_increase_tests_7day == 0 & FLAG_negative_tests_7day == 0 &
                    FLAG_negative_cases_7day == 0)
  
  find_recent_sub <- find_recent %>%
    dplyr::filter(date >= X_date) %>%
    dplyr::filter(FLAG_increase_tests_7day == 0 & FLAG_negative_tests_7day == 0 &
                    # Some locations in FIND do not have case data -- can include still
                    (FLAG_negative_cases_7day == 0 | is.na(FLAG_negative_cases_7day)))
  
  owid_first <- unique(owid_recent_sub$iso_code)
  find_second <- setdiff(unique(find_recent_sub$iso_code), owid_first)
  
  all_recent_final <- all_recent %>%
    dplyr::mutate(
      preferred_source = ifelse(iso_code %in% owid_first, "OWID", 
                                ifelse(iso_code %in% find_second, "FIND", NA)),
      new_tests_daily7 = ifelse(preferred_source == "OWID", owid_tests_daily7, 
                                ifelse(preferred_source == "FIND", find_tests_daily7, NA)),
      new_tests_daily7_prev = ifelse(preferred_source == "OWID", owid_tests_daily7_prev, 
                                     ifelse(preferred_source == "FIND", find_tests_daily7_prev, NA)),
      new_tests_daily7_1k = ifelse(preferred_source == "OWID", owid_tests_daily7_1k, 
                                   ifelse(preferred_source == "FIND", find_tests_daily7_1k, NA)),
      new_tests_daily7_1k_prev = ifelse(preferred_source == "OWID", owid_tests_daily7_1k_prev, 
                                        ifelse(preferred_source == "FIND", find_tests_daily7_1k_prev, NA)),
      positive_rate = ifelse(preferred_source == "OWID", owid_positive_rate,
                             ifelse(preferred_source == "FIND", find_positive_rate, NA)),
      positive_rate_prev = ifelse(preferred_source == "OWID", owid_positive_rate_prev,
                                  ifelse(preferred_source == "FIND", find_positive_rate_prev, NA)),
      preferred_recent_date = as.Date(ifelse(preferred_source == "OWID", owid_date, 
                                             ifelse(preferred_source == "FIND", find_date, NA))),
      flag_negative_tests_previous7 = ifelse(preferred_source == "OWID", owid_flag_negative_tests_prev7,
                                             ifelse(preferred_source == "FIND", find_flag_negative_tests_prev7, NA)),
      flag_increase_tests_previous7 = ifelse(preferred_source == "OWID", owid_flag_increase_tests_prev7,
                                             ifelse(preferred_source == "FIND", find_flag_increase_tests_prev7, NA)),
      flag_negative_cases_previous7 = ifelse(preferred_source == "OWID", owid_flag_negative_cases_prev7,
                                             ifelse(preferred_source == "FIND", find_flag_negative_cases_prev7, NA)),
      
    ) %>%
    dplyr::relocate(iso_code, preferred_source, preferred_recent_date, positive_rate, positive_rate_prev,
                    new_tests_daily7:new_tests_daily7_1k_prev, starts_with("flag_"))
  
  
  return(all_recent_final)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_testing
#' @description Subsets the full testing data set obtained via get_testing_long to have only one preferred source per country
#' OUTPUT: 
# - Function argument test_long is the output from get_testing_long()
# - Most recent 7-day average data points for new tests per 1K AND positivity rate for use in risk matrix plots by WHO region.
# - After accounting for flags, we pick OWID by default when both OWID and FIND are available in last X days
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' testing <- get_testing()}
#'

get_testing <- function() {

analysis_date <- Sys.Date() - 1
  
#Create testing data frame
testing_long   <- get_testing_long()
preferred      <- get_preferred_testpos7(testing_long, last_X_days = 14, analysis_date = analysis_date)
preferred_long <- testing_long %>%
                  dplyr::left_join(preferred %>% select(iso_code, preferred_source), by = c("iso_code" = "iso_code")) %>%
                  dplyr::filter(data_source == preferred_source)
#Time Series based on Preferred-Source Data Frame
preferred_long_locf <- preferred_long %>%
                  dplyr::group_by(iso_code) %>% 
                  dplyr::arrange(date) %>% 
                  dplyr::mutate(new_tests_smoothed_per_thousand    = zoo::na.locf(new_tests_daily7_per_1k, na.rm=F, maxgap=14),
                                new_tests_smoothed_per_thousand_14 = zoo::na.locf(new_tests_daily14_per_1k, na.rm=F, maxgap=14),
                                positive_rate                      = zoo::na.locf(positive_rate_7day, na.rm=F, maxgap=14)) %>%
                  dplyr::ungroup(.) %>% 
                  dplyr::select(iso_code, date, new_tests_smoothed_per_thousand, new_tests_smoothed_per_thousand_14, positive_rate)

  return(preferred_long_locf)

}
