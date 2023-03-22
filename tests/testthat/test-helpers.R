test_that("Window Calculation works for ungrouped data", {
  a <- tibble(
    my_var = rep(c("a", "b"), each = 49),
    new_cases = rep(rep(1:7, each = 7), 2)
  ) |>
    group_by(my_var) |>
    mutate(date = row_number()) |>
    ungroup()

  # ungrouped operation should summarize
  # to half-size by date
  b <- calc_window_pct_change(a, 7)

  expect_identical(dim(b), c(49L, 3L))

  # If we ask to return totals, that should be included
  d <- calc_window_pct_change(a, window = 7, return_totals = TRUE)
  
  expect_identical(dim(d), c(49L, 5L))
  expect_true(all(c("cases_current_7", "cases_prev_7") %in% colnames(d)))
})

test_that("Window calculation works for grouped data", {
  a <- tibble(
    my_var = rep(c("a", "b"), each = 49),
    new_cases = rep(rep(1:7, each = 7), 2)
  ) |>
    group_by(my_var) |>
    mutate(date = row_number()) |>
    ungroup()

  # Under the pre-grouped case:
  # - should be full sized still
  # - grouped var should still be in place
  b <- a |>
    group_by(my_var) |>
    calc_window_pct_change(window = 14)
  
  expect_identical(dim(b), c(98L, 4L))
  expect_true("my_var" %in% colnames(b))

  # If we ask to return totals, that should be included
  d <- a |>
    group_by(my_var) |>
    calc_window_pct_change(window = 14, return_totals = TRUE)
  
  expect_identical(dim(d), c(98L, 6L))
  expect_true("my_var" %in% colnames(d))
  expect_true(all(c("cases_current_14", "cases_prev_14") %in% colnames(d)))

})

test_that("Computing 7-day incidence matches other", {

  # We pre-compute 7day average incidence in get_combined_table
  # because it's used everywhere in reports. Our internal function
  # should match output
  df_who <- get_covid_df("WHO") |>
    left_join(select(onetable, -geometry), by = "iso2code") |>
    calc_add_risk() |>
    select(id, date, new_cases, population, week_case_incidence)
  
  # Picking 2 sundays back
  sunday_date <- df_who |>
    distinct(date, wk = weekdays(date)) |>
    filter(wk == "Sunday") |>
    arrange(desc(date)) |>
    slice(2) |>
    pull(date)
  

  df_comp <- df_who |>
    filter(date <= sunday_date) |>
    calc_window_incidence(window = 7)
  
  # Our two different approaches should match
  # NOTE: obviously we'd prefer to do this in one place and avoid
  # duplication altogether, but trying to limit number of major breaking API changes
  expect_identical(
    df_who |>
      filter(date == sunday_date) |>
      select(id, date, ave_incidence = week_case_incidence),
    df_comp,
    tolerance = 1e-15 # not really significant past this level
  )
})