# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title get_onetable
#' @description One table to rule them all and in keys bind them!
#' Output is available through the package as "onetable," but this function can be used to recreate this dataset.
#' Note: state regions is handled externally by an excel file (two column table: id -iso3code and state_region).
#' User will be prompted to import this file by the function!
#' To regenerate and make the data available again for the package, run the following in dev and rebuild package:
#' 1. onetable <- get_onetable()
#' 2. usethis::use_data(onetable, overwrite=T)
#' @param country_geometries Default is country_coords which is incomplete. Replacement should have an iso3code column!

#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#' onetable <- get_onetable()
#' }
#'
get_onetable <- function(country_geometries = country_coords) {

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
  df_meta <- left_join(df_meta, openxlsx::read.xlsx(file.choose()), by = "id") %>%
    mutate(state_region = case_when(
      who_country == "United States of America" ~ "US",
      TRUE ~ state_region
    ))

  ## UN World Population
  # Getting the population numbers from UN and gaps from CIA Factbook (https://www.cia.gov/the-world-factbook/field/population/country-comparison).
  df_un <- openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
    sheet = 1, startRow = 17
  ) %>%
    filter(Type == "Country/Area") %>%
    select(un_country = 3, un_countrycode = 5, `2020`) %>%
    mutate(`2020` = as.numeric(`2020`) * 1000)

  df_un2 <- openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F08_1_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_BOTH_SEXES.xlsx",
    sheet = 1, startRow = 17
  ) %>%
    filter(`Reference.date.(as.of.1.July)` == 2020 & Type == "Country/Area") %>%
    select(c(un_country = 3, un_countrycode = 5, 9, 48)) %>%
    mutate(
      `Total` = as.numeric(`Total`) * 1000,
      `18+` = as.numeric(`18+`) * 1000
    )

  df_un3 <- full_join(df_un, df_un2, by = "un_countrycode") %>%
    left_join(
      openxlsx::read.xlsx("https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX",
        sheet = 1, startRow = 17
      ) %>%
        select(country = 2, 4, 5),
      by = c("un_countrycode" = "Location.code")
    ) %>%
    select(country, `ISO3.Alpha-code`, un_countrycode, `2020`, `18+`) %>%
    add_row(country = "Guernsey", `ISO3.Alpha-code` = "GGY", `2020` = 67334) %>%
    # CIA
    add_row(country = "Jersey", `ISO3.Alpha-code` = "JEY", `2020` = 101476) %>%
    # CIA
    add_row(country = "Pitcairn Islands", `ISO3.Alpha-code` = "PCN", `2020` = 50) %>%
    # CIA
    add_row(country = "Kosovo", `ISO3.Alpha-code` = "XKX", `2020` = 1935259) # CIA

  ## WB-WHO-Country-Population-List
  # Joined by iso3code.
  df_meta <- left_join(df_meta, df_un3, by = c("id" = "ISO3.Alpha-code"))

  ## Add Geometries
  df_meta <- df_meta %>%
    left_join(country_geometries, by = c("id" = "iso3code")) # country_coords

  df_meta <- select(df_meta, id, iso2code, state_region, who_region, who_country, incomelevel_value, population = `2020`, eighteenplus = `18+`, geometry)
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
#'
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