#' @title A function to retrieve a dataframe (df) with combined Case/Death/Vaccine data by country
#'
#' @param type (character) Specifies whether df should include disaggregated China data ("Both" separates China, Taiwan, Hong Kong, and Macau data) or combined China data ("WHO" combines China, Taiwan, Hong Kong, and Macau data as China)
#' @param geometry (logical, default: FALSE) Specifies whether df should include the geometry column
#'
#' @returns Returns an object of class \code{data.frame} with n rows and 56(57, if \code{geometry = TRUE}) columns
#'
#' @seealso [get_covid_df()], [get_vax()], and [calc_add_risk()] for full column data documentation
#'
#' @examples
#' \dontrun{
#' # Get the df that combines China with Taiwan, Hong Kong, and Macau data
#' df_who <- get_combined_table("WHO")
#' print(df_who)
#' # Get the df that uses both disagreggated China, Taiwan, Hong Kong, and Macau data (WHO + JHU= "Both")
#' df_both <- get_combined_table("Both")
#' print(df_both)
#'
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
