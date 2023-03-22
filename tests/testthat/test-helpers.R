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