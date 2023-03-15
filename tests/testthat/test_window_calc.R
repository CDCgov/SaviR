test_that("Window Calculation works as expected", {
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

  expect_identical(dim(b), c(49, 3))

  # Grouped operations work
  # and return full-sized data
  d <- calc_window_pct_change(a, 7, "my_var")

  expect_identical(dim(d), c(98, 4))

  # unquoted names should work as well
  # (but aren't safe due to symbol collision)
  e <- calc_window_pct_change(a, 14, my_var)

  expect_identical(dim(b), c(98, 4))
  expect_identical(d, e)

})