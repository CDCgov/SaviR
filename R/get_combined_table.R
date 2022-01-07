#' @title Create combined Case/Death/Vax data.frame
#'
#' @param type (character) Whether data should include only WHO information, or WHO+JHU (to include separately HK/Macau/Taiwan)
#' @param geometry (logical, default: FALSE) should the geometry column be added
#'
#' @returns An object of class \code{data.frame} with n rows and 55(56, if \code{geometry = TRUE}) columns
#'
#' @seealso [get_covid_df()], [get_vax()], and [calc_add_risk()] for full column data documentation
#'
#' @examples
#' \dontrun{
#' # get_combined_table() is identical to the following sequence:
#' onetable %>%
#'   select(-geometry) %>% # In the case that geometry = FALSE
#'   right_join(get_covid_df(), by = "iso2code") %>%
#'   filter(source == "WHO") %>% # In the case of type = "WHO"
#'   # filter(!(country == "China" & source == "WHO")) %>% # In the case of type = "Both"
#'   calc_add_risk() %>%
#'   left_join(get_vax(), by = c("id", "date"))
#' }
#' @export

get_combined_table <- function(type = c("WHO", "Both"), geometry = FALSE) {
  type <- match.arg(type)

  case_death_df <- get_covid_df()
  vax_df <- get_vax()
  meta_df <- onetable

  # Remove geometry column unless it's asked for
  # (This causes a ton of issues when the table is large)
  if (!geometry) {
    meta_df <- meta_df %>%
      select(-geometry)
  }

  # Join in case / death data
  out <- meta_df %>%
    right_join(case_death_df, by = "iso2code")

  # If we want Taiwan / HK / Macau, remove china estimates
  # and keep JHU
  if (type == "Both") {
    out <- filter(out, !(country == "China" & source == "WHO"))
  } else {
    # If we only want WHO data, remove the JHU rows
    out <- filter(out, source == "WHO")
  }

  out <- out %>%
    calc_add_risk() %>%
    left_join(vax_df, by = c("id", "date")) %>%
    calc_vax_carryforward()

  return(out)
}
