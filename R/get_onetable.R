# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_onetable
#' @description
#' One table to rule them all and in keys bind them!
#' Output is available through the package as "onetable," but this function can be used to recreate this dataset.
#'
#' Note: state regions is handled externally in a CSV file.

#' @param usaid_metadata_file (character, optional) A file path to the file containing State Department regions. Expects at least two columns, ["iso_alpha3", "state_region"]
#' @param vintage (numeric, default: 2021) The year of population projections to use from UN data
#' @param country_geometries (data.frame, default: country_coords) a data.frame/sfc with at least two columns: ["iso3code", "geometry"]

#' @return a data.frame of 238 rows and 10 columns
#'
#' @import sf
#' @import passport
#' @importFrom openxlsx read.xlsx
#' @export
#'
#' @section Note:
#' Population updates for Pitcairn Islands, Jersey, Guernsey, and Kosovo are hardcoded and must be pulled manually via CIA factbook unless another source is found.
#'
#' @seealso [onetable] for more complete data documentation
#' @examples
#' \dontrun{
#' # UPDATING ONETABLE
#' # This is the typical location of the USAID DoS file:
#' usaid_file <- file.path(Sys.getenv("USERPROFILE"), "CDC", "ITF-COVID19-SAVI - Documents", "usaid_dos_regions.csv")
#' onetable <- get_onetable(usaid_file)
#' usethis::use_data(onetable, overwrite = TRUE)
#' }
#'
get_onetable <- function(usaid_metadata_file = NULL, vintage = 2021, country_geometries = country_coords) {

  ## Country List
  # From COVID sources.
  # TODO: This is a complete bodge and needs to be fixed when #20 is pulled in
  # See: https://github.com/CDCgov/SaviR/pull/20
  country_list <- fread(datasource_lk$who_all, stringsAsFactors = FALSE, encoding = "UTF-8") %>%
    rename_all(tolower) %>%
    rename(iso2code = country_code) %>%
    select(who_region, iso2code, country) %>%
    mutate(country = recode(country, !!!who_lk)) %>%
    bind_rows(onetable_addn_countries) %>%
    mutate(
      iso2code = case_when(
        country == "Namibia" ~ "NA",
        country == "Other" ~ "OT",
        country == "Bonaire, Sint Eustatius, and Saba" ~ "BQ",
        TRUE ~ iso2code
      )
    ) %>%
    # Required, because we've combined Bonaire, Sint Eustatius, and Saba
    distinct(who_region, country, iso2code)

  ## World Bank
  # Make the API call to the World Bank's API for income classification metadata.
  res <- httr::GET(datasource_lk$wb_income)
  df_wb <- jsonlite::fromJSON(rawToChar(res$content), flatten = T)[[2]] %>%
    rename_all(tolower) %>%
    # Remove aggregates, and "Channel Islands", which is not a country
    filter(region.value != "Aggregates" | is.na(region.value), iso2code != "JG") %>%
    as_tibble()

  ## WB-WHO-Country List
  # Full join starting with World Bank's metadata to get the combined list.
  df_meta <- full_join(df_wb, country_list, by = "iso2code") %>%
    select(
      id,
      iso2code,
      incomelevel_value = incomelevel.value,
      who_region,
      who_country = country
    ) %>%
    filter(iso2code != "OT") %>%
    mutate(
      # Apply manual lookup for ISO3 codes that don't parse correctly
      id = recode(id, !!!manual_iso3_lk),
      # Parse remaining NA values for id
      # NOTE: This will throw warnings, but we've included
      id = if_else(is.na(id), parse_country(who_country, to = "iso3c"), id)
    )


  # If no file was passed, use the one saved in the package files
  if (is.null(usaid_metadata_file)) {
    usaid_metadata_file <- system.file("extdata/usaid_dos_regions.csv", package = "SaviR")
  }

  usaid_metadata <- fread(usaid_metadata_file) %>%
    distinct(id = iso_alpha3, state_region)

  df_meta <- df_meta %>%
    left_join(usaid_metadata, by = "id") %>%
    mutate(
      state_region = case_when(
        who_country == "United States of America" ~ "US",
        TRUE ~ state_region
      )
    )

  ## UN World Population
  # Getting the population numbers from UN and gaps from CIA Factbook (https://www.cia.gov/the-world-factbook/field/population/country-comparison).

  # --- Location / Country metadata ------------
  df_un_location_meta <- openxlsx::read.xlsx(datasource_lk$un_location_meta, sheet = 1, startRow = 17) %>%
    select(country = 2, LocID = 4, id = 5, type = 7) %>%
    filter(type == "Country/Area") %>%
    as_tibble()

  df_un_medium_pop_est <- data.table::fread(datasource_lk$un_overall_projections) %>%
    filter(Variant == "Medium", Time == vintage) %>%
    mutate(total = 1000 * as.numeric(PopTotal)) %>%
    distinct(LocID, Time, total)

  df_un_medium_pop_est_single_year <- data.table::fread(datasource_lk$un_age_projections) %>%
    semi_join(df_un_location_meta, by = "LocID") %>% # Filter to only countries, to speed up summarize step
    filter(Time == vintage, Variant == "Medium", AgeGrp >= 18) %>%
    group_by(LocID, Time) %>%
    summarize(`18+` = 1000 * sum(PopTotal)) %>%
    ungroup()

  # Join all UN pop estimates together and add the manual CIA ones
  df_all_un_pop_est <- df_un_location_meta %>%
    left_join(df_un_medium_pop_est, by = "LocID") %>%
    left_join(df_un_medium_pop_est_single_year, by = c("LocID", "Time")) %>%
    select(country, id = id, total, `18+`) %>%
    # Add in data from CIA world factbook
    bind_rows(cia_wfb_addn_countries)

  ## WB-WHO-Country-Population-List
  # Joined by id.
  df_meta <- left_join(df_meta, df_all_un_pop_est, by = "id")

  ## Add Geometries
  df_meta <- df_meta %>%
    left_join(country_geometries, by = c("id" = "iso3code")) # country_coords

  # Add "pretty" WHO region names
  df_meta <- df_meta %>%
    mutate(who_region_desc = who_region_lk[who_region])

  df_meta <- select(df_meta, id, iso2code, state_region, who_region, who_region_desc, who_country, incomelevel_value, population = total, eighteenplus = `18+`, geometry)

  return(df_meta)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_country_coords
#' @description Add dates to country data
#' @param world User prompt to import shapefile.
#' Output is available through the package as "country_coords," but this function can be used to recreate this dataset.
#' To regenerate and make the data available again for the package, run the following in dev and rebuild package:
#' 1. country_coords <- get_country_coords()
#' 2. usethis::use_data(country_coords, overwrite=T)
#'

#'
#' @export
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform
#' @examples
#' \dontrun{
#' country_coords <- get_country_coords()
#' }
#'
get_country_coords <- function(world = file.choose()) {
  df <- rgdal::readOGR(world) %>%
    sp::spTransform(sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) %>%
    sf::st_as_sf() %>%
    select(TYPE, ADMIN, ISO_A3) %>%
    mutate(id = passport::parse_country(ADMIN, to = "iso3c")) %>%
    mutate(id = if_else(ADMIN == "eSwatini", "SWZ", id)) %>%
    mutate(id = if_else(ADMIN == "Kosovo", "XKX", id)) %>%
    filter(!id == "ATA" & !id == "FJI") %>%
    # remove Antarctica and Fiji
    filter(!ADMIN == "Northern Cyprus") # remove Northern Cyprus

  return(df)
}
