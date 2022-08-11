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
  skip_on_ci()
  
  new_onetable <- get_onetable()

  # If this fails, need to either reproduce the onetable, or see what changed
  expect_identical(select(new_onetable, -geometry), select(onetable, -geometry))
})

test_that("Onetable geometry matches", {
  skip_on_ci()
  new_onetable <- get_onetable()

  new_onetable <- new_onetable %>%
      mutate(
      # NOTE: This is due to a weird error where left-joining on an sf
      # object now creates an empty GEOMETRYCOLLECTION instead of MULTIPOLYGON
      # that's not really of any consequence other than for testing.
      geometry = lapply(geometry, function(x) {
          if (length(x) > 0) {
            return(x)
          }

        class(x) <- c("XY", "MULTIPOLYGON", "sfg")
        x
      }
      )
    ) %>%
    st_as_sf(crs=sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) %>%
    select(id)

  # NOTE: Comparing as SF object, because random attributes throw off test
  # also making sure CRS is consistent.
  ot <- sf::st_as_sf(onetable) %>%
    sf::st_transform(sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) %>%
    select(id)

  expect_identical(new_onetable, ot)
})
