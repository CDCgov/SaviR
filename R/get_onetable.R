# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_onetable
#' @description One table to rule them all and in keys bind them!
#' Output is available through the package as "onetable," but this function can be used to recreate this dataset.
#' Note: state regions is handled externally by an excel file (two column table: id -iso3code and state_region).
#' User will be prompted to import this file by the function!
#' To regenerate and make the data available again for the package, run the following in dev and rebuild package:

#' @param usaid_metadata_file (character) A file path to the file containing State Department regions. Expects at least two columns, ["iso_alpha3", "state_region"]
#' @param vintage (numeric, default: 2021) The year of population projections to use from UN data
#' @param country_geometries (data.frame, default: country_coords) a data.frame/sfc with at least two columns: ["iso3code", "geometry"]

#' @import sf
#' @import passport
#' @export
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
get_onetable <- function(usaid_metadata_file, vintage = 2021, country_geometries = country_coords) {

  ## Country List
  # From COVID sources.
  country_list <- get_covid_df() %>%
    select(who_region, region, country, country_code) %>%
    unique()

  ## World Bank
  # Make the API call to the World Bank's API for income classification metadata.
  res <- httr::GET(paste0("http://api.worldbank.org/v2/country?format=json&per_page=300"))
  df_wb <- jsonlite::fromJSON(rawToChar(res$content), flatten = T)[[2]]

  ## WB-WHO-Country List
  # Full join starting with World Bank's metadata to get the combined list.
  df_meta <- full_join(df_wb, country_list,
    by = c("iso2Code" = "country_code")
  ) %>%
    rename_all(tolower) %>%
    filter(region.value != "Aggregates" | is.na(region.value)) %>%
    select(id,
      iso2code,
      wb_country        = name,
      wb_region_name    = region.value,
      incomelevel_id    = incomelevel.id,
      incomelevel_value = incomelevel.value,
      who_region,
      who_region_name   = region,
      who_country       = country
    ) %>%
    filter(iso2code != "OT") %>%
    rowwise() %>%
    mutate(id = ifelse(is.na(id), passport::parse_country(who_country, to = "iso3c", language = c("en")), id)) %>%
    ungroup()

  ## State Department Regions
  # Allow user to choose CSV file.
  # this is currently stored in the top-level files directory for SAVI,
  # called usaid_dos_regions.csv
  usaid_metadata <- readr::read_csv(usaid_metadata_file) %>%
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
  un_location_meta_url <- "https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX"

  df_un_location_meta <- openxlsx::read.xlsx(un_location_meta_url, sheet = 1, startRow = 17) %>%
    select(country = 2, LocID = 4, id = 5, type = 7) %>%
    filter(type == "Country/Area") %>%
    as_tibble()

  un_medium_pop_est_url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv"
  df_un_medium_pop_est <- data.table::fread(un_medium_pop_est_url) %>%
    filter(Variant == "Medium", Time == vintage) %>%
    mutate(total = 1000 * as.numeric(PopTotal)) %>%
    distinct(LocID, Time, total)

  un_medium_pop_est_single_year <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv"

  df_un_medium_pop_est_single_year <- data.table::fread(un_medium_pop_est_single_year) %>%
    semi_join(df_un_location_meta, by = "LocID") %>% # Filter to only countries, to speed up summarize step
    filter(Time == vintage, Variant == "Medium", AgeGrp >= 18) %>%
    group_by(LocID, Time) %>%
    summarize(`18+` = 1000 * sum(PopTotal)) %>%
    ungroup()

  df_all_un_pop_est <- df_un_location_meta %>%
    left_join(df_un_medium_pop_est, by = "LocID") %>%
    left_join(df_un_medium_pop_est_single_year, by = c("LocID", "Time")) %>%
    select(country, id, total, `18+`) # %>%
  # add_row(country = "Guernsey", id = "GGY", total = 67334) %>%
  # # CIA
  # add_row(country = "Jersey", id = "JEY", total = 101476) %>%
  # # CIA
  # add_row(country = "Pitcairn Islands", id = "PCN", total = 50) %>%
  # # CIA
  # add_row(country = "Kosovo", id = "XKX", total = 1935259) # CIA

  ## WB-WHO-Country-Population-List
  # Joined by iso3code.
  df_meta <- left_join(df_meta, df_all_un_pop_est, by = "id")

  ## Add Geometries
  df_meta <- df_meta %>%
    left_join(country_geometries, by = c("id" = "iso3code")) # country_coords

  df_meta <- select(df_meta, id, iso2code, state_region, who_region, who_country, incomelevel_value, population = total, eighteenplus = `18+`, geometry)
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
    mutate(iso3code = passport::parse_country(ADMIN, to = "iso3c")) %>%
    mutate(iso3code = if_else(ADMIN == "eSwatini", "SWZ", iso3code)) %>%
    mutate(iso3code = if_else(ADMIN == "Kosovo", "XKX", iso3code)) %>%
    filter(!iso3code == "ATA" & !iso3code == "FJI") %>%
    # remove Antarctica and Fiji
    filter(!ADMIN == "Northern Cyprus") # remove Northern Cyprus

  return(df)
}