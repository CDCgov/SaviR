test_that("COVID Vaccination Data is accessible", {
  df <- get_vax()
  expect_gt(nrow(df), 0)
})

test_that("COVID Vaccination Manufacturer Data is accessible from OWID", {
  df <- get_vax_manufacturers()
  expect_gt(nrow(df), 0)
})

test_that("COVID Case Data is accessible", {
  df <- get_covid_df()
  expect_gt(nrow(df), 0)
})

test_that("OWID Testing webscraping returns data", {
  df <- get_owid_meta()
  expect_gt(nrow(df), 0)
})

test_that("OWID+FIND Time Series data is available", {
  df <- get_testing_long()
  expect_gt(nrow(df), 0)
})

test_that("OWID+FIND Testing pipeline returns data", {
  df <- get_testing()
  expect_gt(nrow(df), 0)
})

test_that("GDELT News accessor returns data", {
  df <- get_gdeltnews(2)
  dims <- dim(df)

  # Should have > 0 records
  expect_gt(dims[1], 0)

  # Should have 17 columns per spec
  expect_equal(dims[2], 17)
})