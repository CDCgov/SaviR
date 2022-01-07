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

  # Should return 218+ rows, and 5 cols per spec
  expect_gte(nrow(df), 218)
  expect_equal(ncol(df), 5)
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
