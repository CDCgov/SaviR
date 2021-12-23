# Test that all data sources are still accessible at the URLs
# listed

test_that("All Data Sources Accessible", {
  for (datasource_name in names(datasource_lk)) {
    response <- httr::HEAD(datasource_lk[[datasource_name]])$status_code
    expect_equal(response, 200, label = sprintf("%s response code", datasource_name))
  }
})