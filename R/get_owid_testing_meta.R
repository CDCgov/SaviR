#' @title get_owid_meta
#' @description To scrape the OWID coronavirus testing website for data related to the source and definitions of their testing data by country

#' @return Data set of scraped information related to data definitions and sources by country

#' @importFrom magrittr `%>%`
#' @import rvest
#' @import xml2
#' @import dplyr
#' @import stringr
#'
#' @examples
#' \dontrun{
#' owid_testing_meta <- get_owid_testing_meta()
#' usethis::use_data(owid_testing_meta, overwrite = T)
#' }
#'
get_owid_testing_meta <- function(url = "https://ourworldindata.org/coronavirus-testing") {

  ### Data scraping technique ###
  # url = "https://ourworldindata.org/coronavirus-testing"
  content <- xml2::read_html(url)

  # Get List of Countries from HTML Table
  tables <- content %>% html_table(fill = TRUE)
  first_table <- tables[[1]]
  countries <- unique(unlist(first_table))
  countries <- countries[!is.na(countries)]

  # Selector under which all the data is located
  root.selector <- "body > main > article > div.content-wrapper > div.offset-content > div.content-and-footnotes > div.article-content > section:nth-child(4)"
  n.children <- content %>%
    # Number of children the selector has
    html_elements(css = root.selector) %>%
    html_children() %>%
    length()

  # Identify elements with information
  element.list <- seq(13, n.children - 26, by = 2)
  # Country data starts on element 14
  # Last 26 elements are not country data but general footnotes
  # Country headings & country names are in the even elements
  # Country data are in the odd elements

  ### Loop scraping data by country ###
  data <- data.frame()
  for (i in element.list) {

    # Get Country Name (uses elements i)
    selector.country <- paste0(root.selector, " > div:nth-child(", i, ") > div:nth-child(1)")
    country.name <- content %>%
      html_element(css = selector.country) %>%
      html_text()

    # Get Data (uses elements i+1)
    selector.data <- paste0(root.selector, " > div:nth-child(", i + 1, ") > div:nth-child(1)") # " > p:nth-child(4)"
    children <- content %>%
      html_elements(css = selector.data) %>%
      html_children() %>%
      html_text()

    # Extract elements with data
    test.def <- children[grepl("Test definition:", children, fixed = T)]
    case.def <- children[grepl("Case definition:", children, fixed = T)]
    pos.rate <- children[grepl("Positive rate:", children, fixed = T)]

    # Make NA if there are no matching elements
    if (length(test.def) == 0) test.def <- NA
    if (length(case.def) == 0) case.def <- NA
    if (length(pos.rate) == 0) pos.rate <- NA

    # Create Data Frame
    data.i <- data.frame(
      country = country.name,
      test.def = test.def,
      case.def = case.def,
      pos.rate = pos.rate
    )
    data <- data %>% bind_rows(data.i)
  }

  # Split on "Test definition: ", "Case definition: ", "Positive rate: "
  data_cleaned <- data %>% filter(!is.na(country))

  data_cleaned$test_definition <- trimws(str_split_fixed(data_cleaned$test.def, ":", n = 2)[, 2])
  data_cleaned$case_definition <- trimws(str_split_fixed(data_cleaned$case.def, ":", n = 2)[, 2])
  data_cleaned$posrate_definition <- trimws(str_split_fixed(data_cleaned$pos.rate, ":", n = 2)[, 2])
  data_cleaned <- data_cleaned %>%
    relocate(
      test.def, test_definition,
      case.def, case_definition,
      pos.rate, posrate_definition
    )

  # Add ISO-3 codes for easy merging with other datasets
  owid_meta <- data_cleaned %>%
    select(-test.def, -case.def, -pos.rate) %>%
    mutate(
      test_definition = replace(test_definition, test_definition == "", NA_character_),
      case_definition = replace(case_definition, case_definition == "", NA_character_),
      # NOTE: Passport doesn't parse Timor, so it'll throw a warning
      # it'll be handled in the next ifelse() line, though
      id = passport::parse_country(country, to = "iso3c"),
      id = ifelse(country == "Timor", "TLS", id)
    ) %>%
    relocate(country, id, test_definition, case_definition, posrate_definition)

  # Note some duplicated countries for some reason
  owid_meta2 <-
    left_join(owid_meta,
      owid_meta %>%
        group_by(id) %>%
        count(),
      by = "id"
    )

  # For Ecuador, we get 2 different test definitions.
  # -- this is because the first one is correct, the second one is actually El Salvador
  # The webpage link for El Salvador is incorrect
  # Canada has 2 sources
  # Poland has 2 sources
  # Italy has 2 sources

  # If Ecuador is not counted twice, throw an error -- need to change function
  owid_mult <- owid_meta2 %>% filter(n > 1)

  if (!("ECU" %in% owid_mult$id)) {
    stop("Function assumes multiple Ecuador entries -- check function code")
  }

  owid_meta3 <- owid_meta2 %>%
    mutate(
      id_old = id,
      country_duplicated = duplicated(id_old),
      id = if_else(id_old == "ECU" & country_duplicated == TRUE,
        "SLV", id_old
      ),
      country_duplicated2 = duplicated(id)
    ) %>%
    filter(!country_duplicated2)

  owid_meta_final <- owid_meta3 %>%
    select(id:posrate_definition) %>%
    arrange(id) %>%
    mutate(
      posrate_direct = ifelse(str_detect(posrate_definition, "collected directly"), T, F)
    )

  return(owid_meta_final)
}
