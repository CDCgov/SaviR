test_that("COVID Vaccination Data is accessible", {
  df <- get_vax()

  # Should have at least 1 row and 17 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 17)
})

test_that("COVID Vaccination Manufacturer Data is accessible from OWID", {
  df <- get_vax_manufacturers()

  # Should return 218+ rows, and 6 cols per spec
  expect_gte(nrow(df), 218)
  expect_equal(ncol(df), 6)
})

test_that("COVID Vaccination latest dates are accessible", {
  df <- get_vax_dates()

  # Should return 218+ rows, and 6 cols per spec
  expect_gte(nrow(df), 218)
  expect_equal(ncol(df), 6)
})

test_that("COVID Case Data is accessible", {
  df <- get_covid_df()

  # Should have at least 1 row and 8 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 8)
})

test_that("OWID+FIND Time Series data is available", {
  df <- get_testing_long()

  # Should have at least 1 row and 38 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 38)
})

test_that("OWID+FIND Testing pipeline returns data", {
  df <- get_testing()

  # Should have at least 1 row and 5 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 5)
})

test_that("GDELT News accessor returns data", {
  df <- get_gdeltnews(2)
  dims <- dim(df)

  # Should have > 0 records
  expect_gt(dims[1], 0)

  # Should have 16 columns per spec
  expect_equal(dims[2], 16)
})

test_that("COVID hospitalization functions return data from both sources", {

  df <- get_hospdata_long()
  df_wide <- get_hospdata_wide(df)
  df_latest_long <- get_hospdata_latest(df)

  # Should have at least 1 row
  expect_gt(nrow(df), 0)

  # Should have two sources (ECDC and OWID)
  expect_equal(length(unique(df$source)), 2)

  # Should have at least 1 row and 13 cols (df_wide with both sources)
  expect_gt(nrow(df_wide), 0)
  expect_equal(ncol(df_wide), 13)

  # indicators should match names listed here
  test_cols <- c("id", "source", "date", "daily_icu_occupancy",
                 "daily_icu_occupancy_per_million", "daily_hospital_occupancy",
                 "daily_hospital_occupancy_per_million",
                 "weekly_new_hospital_admissions",
                 "weekly_new_hospital_admissions_per_100k",
                 "weekly_new_hospital_admissions_per_million",
                 "weekly_new_icu_admissions",
                 "weekly_new_icu_admissions_per_million",
                 "weekly_new_icu_admissions_per_100k")

  # will error if indicators change in either source (or if some reason other cols missing)
  expect_setequal(colnames(df_wide), test_cols)

  # check latest only returns max one date per country, source, indicator combo
  check_n <-
    df_latest_long %>%
    count(indicator, source, id) %>%
    count(n, name = "new_name")
  expect_equal(check_n$n, 1)
})
