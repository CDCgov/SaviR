test_that("UN Denominator Data is in alignment with OWID", {
  # NOTE: We can only check those countries that OWID pulls from UN
  # because we expect that it won't match across the board if the source is different.
  owid_denom_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/scripts/input/un/population_latest.csv"

  # Pull OWID metadata, filter to only those which use the same UNWPP source
  df_owid_denom <- readr::read_csv(owid_denom_url) %>%
    filter(source == "https://population.un.org/wpp/Download/Standard/CSV/") %>%
    select(id = iso_code, population) %>%
    semi_join(onetable, by = "id") %>%
    arrange(id)

  # Also filter onetable countries only to those that OWID pulls from UN
  onetable_denom <- onetable %>%
    select(id, population) %>%
    semi_join(df_owid_denom, by = "id") %>%
    arrange(id)

  # These countries should have the same (or similar, at least) population
  expect_identical(onetable_denom, df_owid_denom)
})

test_that("onetable can be reproduced and hasn't changed", {
  new_onetable <- get_onetable()

  expect_identical(new_onetable, onetable)
})