
#' @title Retrieve Current and Archival Reports of Various COVID-19 Data
#' @param source Name of COVID-19 data to retrive (currently only WHO data is implemented)
#' @param last_n (numeric, default: Inf) number of historical releases to download (from the most recent)
#' 
#' @return A large dataframe with an additional `data_date` column that defines the data release date 
get_covid_hist <- function(source = "WHO", last_n = Inf) {

  match.arg(source)

  # This should only work if we have datalake access
  if (!getOption("savir.use_datalake", FALSE)) {
    stop("Datalake access required to fetch archival data.")
  }

  lookup_value <- switch(source,
    WHO = "who_all",
    # Unreachable due to match.arg(), but will still include
    stop("No other data sources implemented at this time")
  )

  out <- .fetch_data(lookup_value, past_n = last_n)

  if (lookup_value == "who_all") {
  # Run our WHO processing on the bulk data
    out <- out |>
      process_who_data()
  }

  return(out)
}