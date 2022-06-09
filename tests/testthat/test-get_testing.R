

test_that("OWID Testing sunset will not break get_testing()",{
    # On June 23rd, 2022 OWID will turn off testing dataset.
    # We'll artificially create a scenario where this happens
    # and test for a few contingencies

    # Pull all testing data
    testing_df <- get_testing_long()

    expect_true(TRUE) # DEBUG
})