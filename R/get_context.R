# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_gdeltnews
#' @description Get last 2 weeks of GDELT COVID news. Deduplicated by DateTime, URL, and DomainCountryCode.

#'
#' @export
#'
#' @examples
#' \dontrun{
#' gdeltnews_df <- get_gdeltnews()
#' }
#'
get_gdeltnews <- function(period = 14) {
  gdeltnews <- data.frame()

  for (d in seq(Sys.Date() - period, Sys.Date(), by = "day")) {
    message(zoo::as.Date(d))
    tryCatch(
      container <- jsonlite::stream_in(
        gzcon(url(paste0(
          "http://data.gdeltproject.org/blog/2020-coronavirus-narrative/live_onlinenewsgeo/",
          stringr::str_remove_all(zoo::as.Date(d), "-"),
          "-onlinenewsgeo.json.gz"
        )))
      ),
      error = function(e) {
        return(gdeltnews %>%
          mutate(Date = sub("T.*", "", DateTime)) %>%
          # select(-c(DateTime, URL, SharingImage, DomainCountryCode)) %>%
          unique())
      }
    )

    gdeltnews <- bind_rows(gdeltnews, container)
    gc()
  }

  return(gdeltnews %>%
    mutate(Date = sub("T.*", "", DateTime)) %>%
    # select(-c(DateTime, URL, SharingImage, DomainCountryCode)) %>%
    unique())
}