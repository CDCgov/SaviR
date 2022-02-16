# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_countriesofconcern
#' @description Table for displaying stats for list of countries of concern.
#' @param df A dataframe with the following: id, who_country, date, new_cases, week_case_incidence, week_case, prev_week_case, percent_change_case,
#' new_deaths, week_death_incidence, week_death, prev_week_death, percent_change_death, people_vaccinated_per_hundred, total_vaccinations_per_hundred
#' @param df_vax_man A dataframe with the following: id, date, vaccines
#' @param country_list (character) A vector of ISO 3166-1 alpha-3 country codes for countries to highlight
#' @import flextable
#' @import officer
#' @importFrom purrr set_names
#' @importFrom tibble rownames_to_column
#'
#' @section Notes:
#' \code{Most Recent Date for Vaccinations} column is computed internally via a call to [get_vax()]
#'
#' @examples
#' \dontrun{
#' # Get case/death/vax data
#' df_both <- get_combined_table("Both")
#'
#' # Select some countries
#' c_list <- c("United Kingdom", "Denmark", "United States of America")
#' # The function expects ISO3 codes, so parse if input is raw
#' c_list_iso <- parse_country(c_list, to = "iso3c")
#'
#' # Take the latest observation for each country (vax + case + deaths)
#' df_both_latest <- df_both %>%
#'   group_by(id) %>%
#'   filter(date == max(date)) %>%
#'   ungroup()
#'
#' # Pull metadata on vax manufacturers
#' vax_man <- get_vax_manufacturers()
#'
#' # Produce table
#' table_countriesofconcern(df_both_latest, vax_man, c_list_iso)
#' }
#' @export

table_countriesofconcern <- function(df, df_vax_man, country_list) {
  str_border <- officer::fp_border(color = "#808080")

  # Pull latest dates for vaccine data
  vax_latest_dates <- get_vax_dates() %>%
    filter(id %in% country_list) %>%
    pivot_longer(ends_with("date"), names_to = "type", values_to = "date") %>%
    group_by(id) %>%
    summarize(`Most Recent Date for Vaccinations` = max(date))

  as.data.frame(
    t(
      dplyr::filter(df, id %in% country_list) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::mutate(
          Country = who_country,
          Date = date,
          `New Cases 7 Day Average\n(7 Day Average Case Incidence per 100,000)` = paste0(format(round(new_cases_7dav, 1), format = "f", big.mark = ",", drop0trailing = TRUE), "\n(", round(week_case_incidence, 2), ")"),
          `7 Day Cases` = scales::comma(round(week_case)),
          `Previous 7 Day Cases` = scales::comma(round(prev_week_case)),
          `% Change in Cases from Previous 7 Days` = scales::percent(percent_change_case, scale = 1, drop0trailing = TRUE),
          `New Deaths 7 Day Average\n(7 Day Average Death Incidence per 100,000)` = paste0(format(round(new_deaths_7dav, 1), format = "f", big.mark = ",", drop0trailing = TRUE), "\n(", round(week_death_incidence, 2), ")"),
          `7 Day Deaths` = scales::comma(round(week_death)),
          `Previous 7 Day Deaths` = scales::comma(round(prev_week_death)),
          `% Change in Deaths from Previous 7 Days` = scales::percent(percent_change_death, scale = 1, drop0trailing = TRUE),
          `People Vaccinated Per 100 People` = people_vaccinated_per_hundred,
          `People Fully Vaccinated Per 100 People` = people_fully_vaccinated_per_hundred,
          `Total Vaccinations Per 100 People` = total_vaccinations_per_hundred
        ) %>%
        dplyr::select(id, Country:`Total Vaccinations Per 100 People`) %>%
        left_join(vax_latest_dates, by = "id") %>%
        left_join(
          filter(df_vax_man, id %in% country_list) %>%
            group_by(id) %>%
            filter(last_observation_date == max(last_observation_date)) %>%
            mutate(
              `Vaccines in Use` = vaccines,
              `% Delta` = ""
            ) %>%
            select(id, `Vaccines in Use`, `% Delta`),
          by = "id"
        ) %>%
        select(
          Country,
          Date,
          `New Cases 7 Day Average\n(7 Day Average Case Incidence per 100,000)`,
          `7 Day Cases`,
          `Previous 7 Day Cases`,
          `% Change in Cases from Previous 7 Days`,
          `New Deaths 7 Day Average\n(7 Day Average Death Incidence per 100,000)`,
          `7 Day Deaths`,
          `Previous 7 Day Deaths`,
          `% Change in Deaths from Previous 7 Days`,
          `Most Recent Date for Vaccinations`,
          `People Vaccinated Per 100 People`,
          `People Fully Vaccinated Per 100 People`,
          `Total Vaccinations Per 100 People`,
          `Vaccines in Use`,
          `% Delta`
        )
    )
  ) %>%
    tibble::rownames_to_column(" ") %>%
    purrr::set_names(.[1, ]) %>%
    filter(Country != "Country") %>%
    flextable::flextable() %>%
    flextable::font(fontname = "Calibri", part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::color(color = "white", part = "header") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::bg(bg = "#1F497D", part = "header") %>%
    flextable::bg(bg = "#D6D6D6", j = 1) %>%
    flextable::width(width = 1, unit = "in") %>%
    flextable::width(j = 1, 2.5, unit = "in") %>%
    flextable::hline(border = str_border) %>%
    flextable::vline(border = str_border) %>%
    flextable::border_outer(border = str_border)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10mostcases
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - cases, value2 - percent change

#' @import gt
#' @export

table_10mostcases <- function(df, type = "Global", run_date = "Enter a date") {
  if (type == "Global") {
    title_label <- gt::html(paste0("<b>10 Countries/ Areas with Most \nNew Cases", "</b>"))
  } else {
    title_label <- gt::html(paste0("<b>10 (", type, ") Countries/ Areas with Most \nNew Cases", "</b>"))
  }

  gt::gt(df) %>%
    gt::tab_header(title = title_label) %>%
    gt::data_color(
      columns = c(value2),
      colors = scales::col_bin(
        palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
        bins = c(-Inf, -50, 0, 50, 100, 200, Inf)
      )
    ) %>%
    gt::fmt_number(
      columns = c(value1),
      sep_mark = ",",
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns = c(value2),
      decimals = 1
    ) %>%
        fmt_missing(
      columns = c(value1, value2),
      missing_text = "-"
    ) %>%
    gt::cols_label(
      country = gt::html("Country/ Area"),
      value1 = gt::html("New Cases<br>This Week"),
      value2 = gt::html("% Change<br>Last Week")
    ) %>%
    gt::cols_align("center") %>%
    gt::cols_width(
      c(country) ~ gt::px(175),
      c(value1) ~ gt::px(100),
      c(value2) ~ gt::px(100)
    ) %>%
    gt::tab_options(
      table.width = gt::px(400),
      column_labels.font.weight = "bold",
      table.font.weight = "bold",
      footnotes.font.size = gt::pct(70),
      source_notes.font.size = gt::pct(70),
      source_notes.padding = 0,
      footnotes.padding = 0
    ) %>%
    gt::tab_source_note(source_note = gt::md("Data Source: WHO Coronavirus Disease (COVID-19) Dashboard")) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", run_date)) %>%
    gt::tab_footnote(
      footnote = "Percent change in cases of most recent 7 days to 7 days prior",
      locations = gt::cells_column_labels(columns = c(value2))
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10incidence
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - incidence, value2 - percent change, date

#'
#' @export

table_10incidence <- function(df, type = "Global", run_date = "Enter a date") {
  if (type == "Global") {
    title_label <- gt::html(paste0("<b>10 Countries/ Areas with Highest Incidence", "</b>"))
  } else {
    title_label <- gt::html(paste0("<b>10 (", type, ") Countries/ Areas with Highest Incidence", "</b>"))
  }

  gt::gt(df) %>%
    gt::tab_header(title = title_label) %>%
    gt::data_color(
      columns = c(value1),
      colors = scales::col_bin(
        palette = c("#f1e5a1", "#e7b351", "#d26230", "#aa001e"),
        bins = c(0, 1, 10, 25, Inf),
        na.color = "white"
      )
    ) %>%
    gt::data_color(
      columns = c(value2),
      colors = scales::col_bin(
        palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
        bins = c(-Inf, -50, 0, 50, 100, 200, Inf)
      )
    ) %>%
    gt::fmt_number(
      columns = c(value1),
      sep_mark = ",",
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns = c(value2),
      sep_mark = ",",
      decimals = 1
    ) %>%
        fmt_missing(
      columns = c(value1, value2),
      missing_text = "-"
    ) %>%
    gt::cols_label(
      country = gt::html("Country/ Area"),
      value1 = gt::html("Incidence<br>Per 100,000"),
      value2 = gt::html("% Change<br>Last Week")
    ) %>%
    gt::cols_align("center") %>%
    gt::cols_width(
      c(country) ~ gt::px(175),
      c(value1) ~ gt::px(100),
      c(value2) ~ gt::px(100)
    ) %>%
    gt::tab_options(
      table.width = gt::px(400),
      column_labels.font.weight = "bold",
      table.font.weight = "bold",
      footnotes.font.size = gt::pct(70),
      source_notes.font.size = gt::pct(70),
      source_notes.padding = 0,
      footnotes.padding = 0
    ) %>%
    gt::tab_source_note(source_note = gt::md("Data Source: WHO Coronavirus Disease (COVID-19) Dashboard ")) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", run_date)) %>%
    gt::tab_footnote(
      footnote = "Percent change in cases of most recent 7 days to 7 days prior",
      locations = cells_column_labels(columns = c(value2))
    ) %>%
    gt::tab_footnote(
      footnote = "Average daily incidence per 100,000 in past 7 days",
      locations = cells_column_labels(columns = c(value1))
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10percentchange
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - last week percent change, value2 - 4 week percent change, date

#'
#' @export

table_10percentchange <- function(df, type = "Global", run_date = "Enter a date") {
  if (type == "Global") {
    title_label <- gt::html(paste0("<b>10 Countries/ Areas with <br> Highest Percent Change Last Week", "</b>"))
    exclude_note <- "Note: Countries with population size less than 10 million were excluded"
  } else {
    title_label <- gt::html(paste0("<b>10 (", type, ") Countries/ Areas with <br> Highest Percent Change Last Week", "</b>"))
    exclude_note <- "Note: Countries with population size less than 100,000 were excluded"
  }

  gt::gt(df) %>%
    gt::tab_header(title = title_label) %>%
    gt::data_color(
      columns = c(value1),
      colors = scales::col_bin(
        palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
        bins = c(-Inf, -50, 0, 50, 100, 200, Inf)
      )
    ) %>%
    gt::data_color(
      columns = c(value2),
      colors = scales::col_bin(
        palette = c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316"),
        bins = c(-Inf, -50, 0, 50, 100, 200, Inf)
      )
    ) %>%
    gt::fmt_number(
      columns = c(value1, value2),
      decimals = 1
    ) %>%
    gt::cols_label(
      country = gt::html("Country/ Area"),
      value1 = gt::html("% Change<br>Last Week"),
      value2 = gt::html("% Change<br> 4 Weeks")
    ) %>%
      fmt_missing(
      columns = c(value1, value2),
      missing_text = "-"
    ) %>%
    gt::cols_align("center") %>%
    gt::cols_width(
      c(country) ~ gt::px(175),
      c(value1) ~ gt::px(125),
      c(value2) ~ gt::px(125)
    ) %>%
    gt::tab_options(
      table.width = gt::px(400),
      column_labels.font.weight = "bold",
      table.font.weight = "bold",
      footnotes.font.size = pct(70),
      source_notes.font.size = pct(70),
      source_notes.padding = 0,
      footnotes.padding = 0
    ) %>%
    gt::tab_source_note(source_note = exclude_note) %>%
    gt::tab_source_note(source_note = gt::md("Data Source: WHO Coronavirus Disease (COVID-19) Dashboard")) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", run_date)) %>%
    gt::tab_footnote(
      footnote = "Percent change in cases of most recent 7 days to 7 days prior",
      locations = cells_column_labels(columns = c(value1))
    ) %>%
    gt::tab_footnote(
      footnote = "Percent change in cases of most recent 7 days to 4 weeks prior",
      locations = cells_column_labels(columns = c(value2))
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title table_10vaccinations
#' @description Table for displaying top 10's.
#' @param df A dataframe with the following and in this order: country, value1 - people vaccinated per 100, value2 - daily vaccines administered per 100 people, date
#' @param vac_type (character, default: "Partial") one of ["Partial", "Fully"] depending on vaccination status being tabulated
#' @param type (character, default: "Global") Text name for subset of data \code{df} is, to be included in title
#' @param run_date (character, default: "Enter a date") Run date to include in table source
#'
#' @examples
#' \dontrun{
#' sunday_date <- lubridate::floor_date(Sys.Date(), "week", week_start = 7)
#' df_who <- get_combined_table("WHO")
#'
#' # Take global data for countries with population > 1,000,000
#' df_who %>%
#'   filter(date <= sunday_date, population > 1000000) %>%
#'   group_by(country) %>%
#'   filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
#'   filter(date == max(date)) %>%
#'   ungroup() %>%
#'   select(country = who_country, value1 = people_fully_vaccinated_per_hundred, value2 = daily_vaccinations_per_hundred) %>%
#'   arrange(desc(value1)) %>%
#'   head(10) %>%
#'   table_10vaccinations(., run_date = format(sunday_date, "%B %d, %Y"))
#' }
#' @export

table_10vaccinations <- function(df, vac_type = c("People", "Fully", "Booster"), type = "Global", run_date = "Enter a date") {
  if (type == "Global") {
    if (vac_type == "People") {
      title_label <- gt::html(paste0("<b>Top 10 Countries/ Areas with Highest <br> Vaccination per 100 People", "</b>"))
    } else if (vac_type == "Fully") {
      title_label <- gt::html(paste0("<b>Top 10 Countries/ Areas with Highest <br> Fully Vaccination per 100 People", "</b>"))
    } else if (vac_type == "Booster") {
      title_label <- gt::html(paste0("<b>Top 10 Countries/ Areas with Highest <br> Booster Vaccination per 100 People", "</b>"))
    }
    exclude_note <- "Countries with population size less than or equal to 1 million were excluded"
  } else {
    if (vac_type == "People") {
      title_label <- gt::html(paste0("<b>10 (", type, ") Countries/ Areas with Highest <br> Vaccination per 100 People", "</b>"))
    } else if (vac_type == "Fully") {
      title_label <- gt::html(paste0("<b>10 (", type, ") Countries/ Areas with Highest <br> Fully Vaccination per 100 People", "</b>"))
    } else if (vac_type == "Booster") {
      title_label <- gt::html(paste0("<b>10 (", type, ") Countries/ Areas with Highest <br> Booster Vaccination per 100 People", "</b>"))
    }
    exclude_note <- "Countries with population size less than or equal to 100,000 were excluded"
  }

  if (vac_type == "People") {
    cols_label1 <- gt::html("People Vaccinated <br> per 100 People")
    vax_palette <- c("#b1eeec", "#98d1cf", "#7eb3b2", "#659695", "#4c7877", "#335b5a", "#193d3d", "#002020")
  } else if (vac_type == "Fully") {
    cols_label1 <- gt::html("People Fully Vaccinated <br> per 100 People")
    vax_palette <- c("#ccece6", "#afdacb", "#92c8b1", "#75b696", "#57a37c", "#3a9161", "#1d7f47", "#006d2c")
  } else if (vac_type == "Booster") {
    cols_label1 <- gt::html("Total Boosters <br> per 100 People")
    vax_palette <- c("#DEC9E9", "#CCB6E0", "#BBA4D7", "#A991CE", "#977FC5", "#856CBC", "#745AB3", "#6247AA")
  }

  t <- gt::gt(df) %>%
    gt::tab_header(title = title_label) %>%
    gt::fmt_number(
      columns = c(value1),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns = c(value2),
      sep_mark = ",",
      decimals = 2
    ) %>%
    gt::data_color(
      columns = c(value1),
      colors = scales::col_bin(
        palette = vax_palette,
        bins = c(0, 3, 10, 20, 30, 40, 60, 70, Inf)
      )
    ) %>%
    gt::cols_label(
      country = "Country/ Area",
      value1 = cols_label1,
      value2 = gt::html("Daily Vaccines <br> Administered <br> per 100 People")
    ) %>%
    gt::cols_align("center") %>%
    gt::tab_options(
      column_labels.font.weight = "bold",
      table.font.size = 20,
      table.font.weight = "bold",
      footnotes.font.size = pct(70),
      source_notes.font.size = pct(70),
      source_notes.padding = 0,
      footnotes.padding = 0
    ) %>%
    gt::tab_footnote(
      footnote = "Vaccine doses administered per day (7 day rolling average); does not represent number of people vaccinated",
      locations = cells_column_labels(columns = c(value2))
    ) %>%
    gt::tab_footnote(
      footnote = exclude_note,
      locations = cells_title()
    ) %>%
    gt::tab_footnote(
      footnote = "People vaccinated per 100 people represents total population (all ages)",
      locations = cells_title()
    ) %>%
    gt::tab_source_note(source_note = paste0("Data as of ", run_date))

  if (vac_type == "People") {
    t <- gt::tab_footnote(t,
      footnote = "Number of people out of 100 who received at least one vaccine dose; does not represent percent of population fully vaccinated",
      locations = cells_column_labels(columns = c(value1))
    )
  }
  return(t)
}
