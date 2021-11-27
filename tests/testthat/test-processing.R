test_that("COVID Case Data is accessible", {

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
  #   right_join(get_covid_df() %>% select(-who_region), by = c("iso2code" = "country_code")) %>%
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
