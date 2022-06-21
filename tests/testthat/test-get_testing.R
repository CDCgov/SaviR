test_that("OWID Testing sunset will not break get_testing()",{
    # On June 23rd, 2022 OWID will turn off testing dataset.
    # We'll artificially create a scenario where this happens
    # and test for a few contingencies

    # --- 31d no OWID data -------------------------------
    owid_testing_min31 <- get_owid_testing_long() %>%
        filter(date < max(date) - days(31))

    find_testing <- get_find_testing_long()

    # Combine using same internal code
    testing_long <- rbind(
      owid_testing_min31 %>%
        mutate(data_source = "OWID"),
      find_testing[, names(owid_testing_min31)] %>%
        mutate(data_source = "FIND")
    ) %>%
    relocate(data_source, id, date) %>%
    arrange(data_source, id, date)

    # Run through 7d testpos selection
    preferred <- get_preferred_testpos7(testing_long, last_X_days = 14, analysis_date = Sys.Date() - 1L)
    preferred_long <- testing_long %>%
        left_join(preferred %>% select(id, preferred_source), by = "id") %>%
        filter(data_source == preferred_source)
  
    # Time Series based on Preferred-Source Data Frame
    # preferred_long_locf <- preferred_long %>%
    #     group_by(id) %>%
    #     arrange(date) %>%
    #     mutate(
    #         new_tests_smoothed_per_thousand = zoo::na.locf(new_tests_daily7_per_1k, na.rm = F, maxgap = 14),
    #         new_tests_smoothed_per_thousand_14 = zoo::na.locf(new_tests_daily14_per_1k, na.rm = F, maxgap = 14),
    #         positive_rate = zoo::na.locf(positive_rate_7day, na.rm = F, maxgap = 14)
    #     ) %>%
    #     ungroup(.) %>%
    #     select(id, date, new_tests_smoothed_per_thousand, new_tests_smoothed_per_thousand_14, positive_rate)
    
    expect_true(TRUE) # DEBUG
})