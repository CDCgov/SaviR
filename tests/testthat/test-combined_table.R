test_that("Combined table returns WHO data appropriately", {
  df <- get_combined_table(type = "WHO")
  dims <- dim(df)
  # Should have at least 1 row and 55 cols
  expect_gt(dims[1], 0)
  expect_equal(dims[2], 56)

  # Should only contain WHO-sourced data
  sources <- unique(df$source)
  expect_equal(sources, "WHO")

  # ...and thus, should have no info on HK, Macau, or Taiwan

  # Create an empty table shell to compare to
  comp_shell <- filter(df, FALSE)

  # Should return an empty table if no data has made it through
  resulting <- df %>%
    semi_join(onetable_addn_countries, by = "iso2code")

  expect_identical(resulting, comp_shell)
})

test_that("Combined table returns JHU+WHO data appropriately", {
  df <- get_combined_table(type = "Both")
  dims <- dim(df)

  # Should have at least 1 row and 55 cols
  expect_gt(dims[1], 0)
  expect_equal(dims[2], 56)

  # Should contain both JHU and WHO data
  sources <- unique(df$source)
  sources <- sources[order(sources)]

  expect_equal(sources, c("JHU", "WHO"))

  # ...thus, should have HK, Macau, Taiwan, and China
  expected_countries <- c(onetable_addn_countries$iso2code, "CN")
  expected_countries <- expected_countries[order(expected_countries)]

  resulting <- df %>%
    filter(iso2code %in% expected_countries)

  resulting_countries <- unique(resulting$iso2code)
  resulting_countries <- resulting_countries[order(resulting_countries)]

  expect_gt(nrow(resulting), 0)
  expect_identical(resulting_countries, expected_countries)
})

test_that("Combined table returns geometry if requested (not recommended)", {
  df <- get_combined_table(geometry = TRUE)

  dims <- dim(df)

  expect_true("geometry" %in% names(df))
  expect_type(df[["geometry"]], "list")


  # Should be n * 57
  expect_gt(dims[1], 0)
  expect_equal(dims[2], 57)
})
