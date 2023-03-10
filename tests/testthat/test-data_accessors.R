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

test_that("Data on count of vaccinations by manufacturer is accessible from OWID", {

  df <- get_vax_by_type()

  # Should return at least one row, and 6 cols per spec
  expect_gte(nrow(df), 0)
  expect_equal(ncol(df), 6)
})

test_that("COVID Vaccination latest dates are accessible", {
  df <- get_vax_dates()

  # Should return 218+ rows, and 6 cols per spec
  expect_gte(nrow(df), 218)
  expect_equal(ncol(df), 6)
})

test_that("All COVID Case/Death Data is accessible", {
  df <- get_covid_df("all")

  # Should have at least 1 row and 8 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 8)
})

test_that("WHO data returns correctly", {
  df <- get_covid_df("WHO")

  # Should have at least 1 row and 8 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 8)

  # Should contain only WHO data
  sources <- unique(df$source)
  sources <- sources[order(sources)]
  expect_equal(sources, "WHO")
})

test_that("WHO+JHU data returns correctly", {
  df <- get_covid_df("WHO+JHU")

  # Should have at least 1 row and 8 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 8)

  # Should contain both JHU and WHO data
  sources <- unique(df$source)
  sources <- sources[order(sources)]
  expect_equal(sources, c("WHO", "JHU"))
})

test_that("WHO+Primary data returns correctly", {
  df <- get_covid_df("WHO+Primary")

  # Should have at least 1 row and 8 cols
  expect_gt(nrow(df), 0)
  expect_equal(ncol(df), 8)

  # Should contain all sources
  sources <- unique(df$source)
  sources <- sources[order(sources)]
  expect_equal(sources, c("WHO", "JHU", "Taiwan CDC", "HK CHP"))

  # China data should be from WHO
  china_source <- filter(df, iso2code == "CN") |>
    distinct(source)
  
  expect_equal(china_source, "WHO")
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

test_that("COVID hospitalization functions returns expected indicators from both sources", {

  df <- get_hospdata()

  # Should have at least 1 row
  expect_gt(nrow(df), 0)

  # Should have two sources (ECDC and OWID)
  expect_equal(length(unique(df$source)), 2)

  # interaction(source, indicator) should match this vector
  test_cols <- c("Daily hospital occupancy.ECDC",
                 "Daily ICU occupancy.ECDC",
                 "Weekly new hospital admissions per 100k.ECDC",
                 "Weekly new ICU admissions per 100k.ECDC",
                 "Daily hospital occupancy.OWID",
                 "Daily hospital occupancy per million.OWID",
                 "Daily ICU occupancy.OWID",
                 "Daily ICU occupancy per million.OWID",
                 "Weekly new hospital admissions.OWID",
                 "Weekly new hospital admissions per million.OWID",
                 "Weekly new ICU admissions.OWID",
                 "Weekly new ICU admissions per million.OWID")

  # will error if indicators change in either source
  expect_setequal(levels(interaction(df$indicator, df$source, drop = TRUE)),
                  test_cols)


})
