# Test that all data sources are still accessible at the URLs
# listed

test_that("All Data Sources Accessible", {
  # BUG: I think repeated requests here are causing
  # this test to fail on covr. We really only need to
  # run on R CMD Check, which runs 3x per push anyways
  skip_on_covr()

  # For each data source, check that we have an OK HTTP response (200)
  # to indicate that we're still able to pull data from that source
  for (datasource_name in names(datasource_lk)) {
    opts <- httr::config(nobody = 1, header = 1)
    response <- httr::HEAD(datasource_lk[[datasource_name]])$status_code

    # Try a GET request for some sources that don't support HEAD
    # but are nonetheless available
    if (response == 404) {
      response <- httr::GET(datasource_lk[[datasource_name]], opts)$status_code
    }

    expect_equal(response, 200, label = sprintf("%s response code", datasource_name))
  }
})
