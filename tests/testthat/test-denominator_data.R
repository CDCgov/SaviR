test_that("UN Denominator Data is in alignment with OWID", {
  owid_denom_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/scripts/input/un/population_latest.csv"

  # Pull OWID metadata, filter to only those which use the same UNWPP source
  df_owid_denom <- read_csv(owid_denom_url) %>%
    filter(source == "https://population.un.org/wpp/Download/Standard/CSV/") %>%
    select(id = iso_code, population) %>%
    semi_join(onetable, by = "id") %>%
    arrange(id)

  onetable_denom <- onetable %>%
    select(id, population) %>%
    semi_join(df_owid_denom, by = "id") %>%
    arrange(id)

  expect_identical(onetable_denom, df_owid_denom)
})