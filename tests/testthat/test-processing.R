test_that("Calc_add_risk returns expected result", {

  # Sean NOTE:
  # I used this code to generate the test set you see below.
  # you can re-run if needed, but you'll also need to save
  # the output of calc_add_risk() with that data to compare.
  # sample_countries <- onetable %>%
  #   group_by(who_region) %>%
  #   sample_frac(0.1) %>%
  #   ungroup() %>%
  #   pull(who_country)

  # df <- onetable %>%
  #   select(-geometry) %>%
  #   right_join(get_covid_df(), by = "iso2code") %>%
  #   filter(
  #     !(country == "China" & source == "WHO"),
  #     who_country %in% sample_countries
  #   ) %>%
  #   arrange(desc(date)) %>%
  #   group_by(iso2code) %>%
  #   slice(1:21) %>% # Take 3 weeks of the most recent data
  #   ungroup() %>%
  #   arrange(iso2code)

  # NOTE: This has to be stored as RDS since the vector types
  # will read in messed up if stored as a CSV.
  # saveRDS(df, "2021-11-26-testdata-input.RDS")

  df <- readRDS("2021-11-26-testdata-input.RDS")
  output <- readRDS("2021-11-26-testdata-output.RDS")

  out <- calc_add_risk(df)

  # NOTE: This has to be stored as RDS since the vector types
  # will read in messed up if stored as a CSV.
  # saveRDS(out, "2021-11-26-testdata-output.RDS")

  expect_equal(out, output)
})

test_that("calc_vax_carryforward works as expected", {

  # value to carry forward
  cf_value <- 1000

  # Create artificial dataset where we take last two observations for each country
  # and wipe the very last one
  df <- get_vax() %>%
    arrange(id, desc(date)) %>%
    group_by(id) %>%
    slice(1:2) %>%
    mutate(
      across(
        -owid_country:-date, ~ case_when(
          date == max(date) ~ NA_real_, # The latest observation should be NA
          date == min(date) ~ cf_value # The second-to-last observation should be 1000
        )
      )
    ) %>%
    ungroup()

  # Should be 2 rows per country
  n_start <- nrow(df)

  vaccine_col_str <- c(
    "total_vaccinations", "people_vaccinated", "people_fully_vaccinated",
    "total_boosters", "total_vaccinations_per_hundred", "people_vaccinated_per_hundred",
    "people_fully_vaccinated_per_hundred", "total_boosters_per_hundred"
  )

  vaccine_cols <- lapply(vaccine_col_str, as.name)

  # This should carry forward the second-to-last observation to the first
  df_fixed <- calc_vax_carryforward(df)

  # ..and if so, we could half the dataset
  # since the totals for those columns would be identical across the two days
  df_distinct <- distinct(df_fixed, id, !!!vaccine_cols)

  # Should now be 1 row per country
  expect_equal(nrow(df_distinct), n_start / 2)

  # And each of those cols should add up to 2x whatever we assigned
  # to the second-to-last observation that got carried forward
  df_summed <- df_fixed %>%
    group_by(id) %>%
    summarize_at(vars(!!!vaccine_cols), sum)

  for (i in seq_along(vaccine_col_str)) {
    expect_true(all(df_summed[, vaccine_col_str[i]] == 2 * cf_value))
  }
})
