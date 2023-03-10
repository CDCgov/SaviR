#' @title A function to retrieve a dataframe (df) with combined Case/Death/Vaccine data by country
#'
#' @param type (character) Specifies what data streams to include for case/death data. See details for further information
#' @param geometry (logical, default: FALSE) Specifies whether df should include the geometry column
#'
#' @returns Returns an object of class \code{data.frame} with n rows and 56(57, if \code{geometry = TRUE}) columns
#'
#' @seealso [get_covid_df()], [get_vax()], and [calc_add_risk()] for full column data documentation
#'
#' @details 
#' The `type` argument used to take two values: "WHO" and "Both", referring to whether to take WHO data as-is, or to supplement WHO data with disaggregated China data from JHU.
#' In early Jan 2023, China CDC ceased providing daily COVID-19 updates, so the Mainland China data provided by JHU also stopped. On Mar 10, 2023 JHU closed their dashboard entirely,
#' so new sources had to be located for HK, Macau, and Taiwan data.
#' 
#' For legacy analyses, the old behavior for "Both" is now available as "legacy"
#' 
#' The new "Both" type pulls data from HK CHP, Taiwan CDC, and JHU (for Macau data thru Mar 10) in addition to the China data in WHO (which also includes Taiwan, HK, and Macau data).
#' Because data from HK and Taiwan are duplicated in this way, you should not use data from the "Both" option to compute regional or global trends.
#' @examples
#' \dontrun{
#' # Get the df that combines China with Taiwan, Hong Kong, and Macau data
#' df_who <- get_combined_table("WHO")
#' print(df_who)
#' # Get the df that combines WHO China data (aggregated) with disggregated entries for HK, Taiwan, and Macau (from JHU thru Mar 10, 2023)
#' df_both <- get_combined_table("Both")
#' print(df_both)
#' # Get the df that uses both disaggregated China, Taiwan, Hong Kong, and Macau data (WHO + JHU = "legacy")
#' # (JHU sunset on Mar 10, 2023 and China mainland data ceased earlier in the year)
#' df_both <- get_combined_table("legacy")
#' print(df_both)
#' }
#' @md
#' @export

get_combined_table <- function(type = c("WHO", "Both", "legacy"), geometry = FALSE) {
  type <- match.arg(type)
  
  case_death_df <- switch(
    type,
    WHO = get_covid_df("WHO"),
    Both = get_covid_df("WHO+Primary"),
    legacy = get_covid_df("WHO+JHU")
  )

  if (type == "legacy") {
    # How "Both" used to work before data stopped flowing in:
    # - WHO data for everything except for China where we use JHU to replace
    #   china mainland data, HK, Macau, and Taiwan.
    # As of 3/10/2023, these data won't be updated by JHU, and China mainland data
    # haven't been updating since early Jan 2023 in JHU.
    # But I'll leave in for historical analyses.
    case_death_df <- filter(
      case_death_df,
      !(country == "China" & source == "WHO")
    )
  }

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

  out <- out %>%
    calc_add_risk() %>%
    # BUG: I'm not sure we want this as a left_join
    # but I don't want to break everything by switching it to full
    left_join(vax_df, by = c("id", "date")) %>%
    calc_vax_carryforward()

  return(out)
}
