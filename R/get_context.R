# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_gdeltnews
#' @description
#' Get last 2 weeks of GDELT COVID news. Deduplicated by DateTime, URL, and DomainCountryCode.
#'
#' @param period (numeric) Number of days to look back from start_date
#' @param start_date (Date) Date from which to look back
#'
#' @return A data frame with n rows and 16 variables:
#' \itemize{
#'   \item{\code{DateTime}}{  POSIXct}
#'   \item{\code{URL}}{  character}
#'   \item{\code{Title}}{  character}
#'   \item{\code{SharingImage}}{  character}
#'   \item{\code{LangCode}}{  character}
#'   \item{\code{DocTone}}{  double}
#'   \item{\code{DomainCountryCode}}{  character}
#'   \item{\code{Location}}{  character}
#'   \item{\code{Lat}}{  double}
#'   \item{\code{Lon}}{  double}
#'   \item{\code{iso2code}}{  character}
#'   \item{\code{Adm1Code}}{  character}
#'   \item{\code{GeoType}}{  character}
#'   \item{\code{ContextualText}}{  character}
#'   \item{\code{GeoCoord}}{  character}
#'   \item{\code{Adm2Code}}{  character}
#' }
#'
#' @section Note:
#' Function may fail if start date is set to current date or in future. News data files aren't often avilable for the current date.
#'
#' @export
#' @examples
#' \dontrun{
#' gdeltnews_df <- get_gdeltnews()
#' }
#'
get_gdeltnews <- function(period = 14, start_date = Sys.Date() - 1) {
  date_range <- seq(start_date - period, start_date, by = "day")
  out <- vector("list", length = length(date_range))
  names(out) <- date_range

  for (d in date_range) {
    message(zoo::as.Date(d))

    url_str <- paste0(
      "http://data.gdeltproject.org/blog/2020-coronavirus-narrative/live_onlinenewsgeo/",
      stringr::str_remove_all(zoo::as.Date(d), "-"),
      "-onlinenewsgeo.json.gz"
    )

    container <- try(
      jsonlite::stream_in(gzcon(url(url_str))),
      silent = TRUE
    )

    if (inherits(container, "try-error")) {
      warning(as.character(container), immediate. = TRUE)
      next
    }

    out[[d]] <- container
  }

  out <- bind_rows(out) %>%
    rename(iso2code = CountryCode) %>%
    mutate(DateTime = as.POSIXct(DateTime)) %>%
    distinct() %>%
    as_tibble()

  return(out)
}
