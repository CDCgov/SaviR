test_that("UN Denominator Data is in alignment with OWID", {
  # NOTE: We can only check those countries that OWID pulls from UN
  # because we expect that it won't match across the board if the source is different.
  owid_denom_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/scripts/input/un/population_latest.csv"

  # Pull OWID metadata, filter to only those which use the same UNWPP source
  df_owid_denom <- data.table::fread(owid_denom_url) %>%
    filter(source == "https://population.un.org/wpp/Download/Standard/CSV/") %>%
    select(id = iso_code, population) %>%
    mutate(across(where(is.integer64), as.numeric)) %>%
    semi_join(onetable, by = "id") %>%
    arrange(id) %>%
    as.data.frame()

  # Also filter onetable countries only to those that OWID pulls from UN
  onetable_denom <- onetable %>%
    select(id, population) %>%
    semi_join(df_owid_denom, by = "id") %>%
    arrange(id) %>%
    as.data.frame()

  # These countries should have the same (or similar, at least) population
  # Use 4 sig-fig tolerance to account for rounding errors
  # (we're also coercing from int64 to numeric, which applies some imprecision)
  expect_identical(onetable_denom, df_owid_denom, tolerance = 1e-4)
})

test_that("onetable can be reproduced and is up to date", {
  new_onetable <- get_onetable()

  # If this fails, need to either reproduce the onetable, or see what changed
  expect_identical(new_onetable, onetable)
})
