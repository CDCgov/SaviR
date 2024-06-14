# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_template
#' @description Cross-sectional map. Uses the stored country_coords data as the base map.
#' @param df A dataframe with the following: country, geometry, date, result = factor value
#' @param category_color_labels List of labels that should map to the factor values of the df. Use "None" if no categories.
#' @param category_color_values List of color values for mapping the labels. Needs to have the same length as category_color_labels!

#' @import sf
#' @import ggplot2
#' @export

map_template <- function(df, category_color_labels = "None", category_color_values) {

  # if(length(category_color_labels) != length(category_color_values)){
  #   stop("Your category labels are of different lengths!")
  # }

  if (length(category_color_labels) == 1 && category_color_labels == "None") {
    ggplot2::ggplot(df) + # Param
      ggplot2::geom_sf(
        data = country_coords, # Param
        aes(geometry = geometry),
        size = 0.3
      ) +
      ggplot2::geom_sf(
        data = df,
        aes(
          geometry = geometry,
          fill = result
        ), # Param
        size = 0.2
      ) +
      ggplot2::theme_void() +
      ggplot2::scale_fill_manual(
        values = category_color_values, # Param
        na.value = "#cccccc",
        drop = F,
        na.translate = T
      ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold", family = "Arial"),
      plot.subtitle = ggplot2::element_text(size = 8, family = "Arial", margin = margin(0, 0, 5, 0)),
      plot.caption = ggplot2::element_text(size = 6, family = "Arial", hjust = 0, vjust = 2),
      plot.caption.position = "plot",
      legend.position = c(0.01, 0),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      legend.key.size = unit(0.5, "cm"),
      legend.margin = ggplot2::margin(2, 2, 2, 2),
      legend.title = ggplot2::element_text(size = 8, family = "Arial"),
      legend.text = ggplot2::element_text(size = 6, family = "Arial"),
      legend.background = element_rect(fill = scales::alpha("white", 0.5), colour = "white")
      )
  } else {
    ggplot2::ggplot(df) + # Param
    ggplot2::geom_sf(
      data = country_coords, # Param
      aes(geometry = geometry),
      size = 0.3
    ) +
    ggplot2::geom_sf(
      data = df,
      aes(
        geometry = geometry,
        fill = result
      ), # Param
      size = 0.2
    ) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(
      values = category_color_values, # Param
      na.value = "#cccccc",
      drop = F,
      labels = category_color_labels, # Param
      na.translate = T
     ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold", family = "Arial"),
      plot.subtitle = ggplot2::element_text(size = 8, family = "Arial", margin = margin(0, 0, 5, 0)),
      plot.caption = ggplot2::element_text(size = 6, family = "Arial", hjust = 0, vjust = 2),
      plot.caption.position = "plot",
      legend.position = c(0.01, 0),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      legend.key.size = unit(0.5, "cm"),
      legend.margin = ggplot2::margin(2, 2, 2, 2),
      legend.title = ggplot2::element_text(size = 8, family = "Arial"),
      legend.text = ggplot2::element_text(size = 6, family = "Arial"),
      legend.background = element_rect(fill = scales::alpha("white", 0.5), colour = "white")
    )
  }


}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Burden Map
#' @description A Cross-sectional map: Average daily incidence for the past number of days specified for each country.
#' @param df A data.frame with at least the following columns [id, date, new_cases]
#' @param region (optional) a character string specifying a DoS or WHO region to zoom to, or NULL if none
#' @param time_step (numeric) number of days to average incidence over
#' @param bin_breaks (numeric) a vector of incidence cut points interpretable by `cut`
#' @param bin_colors (character) a vector of hex or ggplot colors for the legend. If named, names will be used to label `bin_breaks`.
#' @return
#' Produces a map of burden (incidence per 100,000)
#'
#' @details
#' map_burden always produces an average incidence map based on the latest date included in the input data.frame.
#' You should ensure that data are completely observed for each timestep, as average incidence is computed based
#' on index rather than calendar date.

#' @export

map_burden <- function(
  df,
  region = NULL,
  time_step = 7,
  bin_breaks = c(0, 1, 10, 25, Inf),
  bin_colors = c("0- <1" = "#f1e5a1", "1- <10" = "#e7b351", "10- <25" = "#d26230", "25+" = "#aa001e")
) {

  # Pretty labels for incidence bins, NULL otherwise
  bin_labels <- names(bin_colors)

  map_df <- df |>
    group_by(id) |>
    calc_window_incidence(type = "cases", window = time_step) |>
    filter(date == max(date)) |>
    mutate(result = cut(ave_incidence, bin_breaks, labels = bin_labels))
  
  # gut-check that the colors we've passed match the levels we've requested
  stopifnot(length(levels(map_df[["result"]])) == length(bin_colors))
  
  map_out <- map_df |>
    full_join(country_coords, by = "id") |>
    map_template(levels(map_df[["result"]]), unname(bin_colors)) +
    ggplot2::labs(fill = sprintf("Average \nDaily \nIncidence \n(past %d days) \nper 100,000", time_step)) +
    ggplot2::labs(
      title = "Burden",
      subtitle = sprintf(
        "Average daily incidence over the past %d days per 100,000 population as of %s",
        time_step,
        str_squish(format(max(map_df$date, na.rm = TRUE), "%B %e, %Y"))
      )
    )

  if (!missing(region)) {

    bbox <- bbox_fun(region, left_join(map_df, country_coords, by = "id"))

    map_out <- map_out +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )
  }

  return(map_out)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_trend
#' @description Cross-sectional map: Average daily incidence for the past 7 days for each country.
#' @param df A data.frame with at least the following columns: id, date, new_cases
#' @param region (optional) a character string specifying a DoS or WHO region to zoom to, or NULL if none
#' @param time_step (default: 7) time step in days the percent-change represents
#'
#' @return
#' Produces a map of trend (% change in the past `time_step` days)
#'
#' @details
#' percent change is always computed relative to the latest date in the data.frame passed, so pre-filter as needed.
#' 

#' @export

map_trend <- function(df, region = NULL, time_step = 7) {

  # Assert that we have the appropriate columns
  stopifnot(all(c("id", "date", "new_cases") %in% colnames(df)))

  cat_labs <- c(">=50% decrease", "0 - <50% decrease", ">0 - <=50% increase", ">50 - <=100% increase", ">100 - <=200% increase", ">200% increase")
  cat_vals <- c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316")

  map_df <- df |>
    group_by(id) |>
    calc_window_pct_change(type = "cases", window = time_step, return_totals = TRUE) |>
    ungroup() |>
    filter(date == max(date)) |>
    mutate(
      # pct change will already be NaN if cases were 0 in the previous period
      # due to division, but we want to also NA out any observations that
      # are not reporting in the current period that were in the previous
      # since we can't ascertain the trajectory
      pct_change = if_else(is.infinite(pct_change), NA_real_, pct_change),
      pct_change = if_else(current == 0, NA_real_, pct_change),
      result = cut((pct_change - 1) * 100, breaks = c(-Inf, -50, 0, 50, 100, 200, Inf))
    ) |>
    left_join(country_coords, by = "id")

  map_out <- map_template(map_df, cat_labs, cat_vals) +
    ggplot2::labs(
      title = "Recent Trends",
      subtitle = paste0(
        "Percent change in cases from ", time_step, "-day period ending ",
        format((unique(map_df$date)), "%B %d, %Y"),
        "\ncompared to previous ", time_step, "-day period ending ",
        format(max(map_df$date) - time_step, "%B %d, %Y")
      ),
      fill = sprintf("Percent \nChange From \nPrevious %d Days", time_step)
    )
  
    if (!missing(region)) {

    bbox <- bbox_fun(region, df)

    map_out <- map_out +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )
  }

  return(map_out)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_vaccinations
#' @description Cross-sectional map: People vaccinated per 100 for each country or Fully vaccinated.
#' @param df A dataframe with the following: region, country, date, people vaccinated per 100 AS 8-level factors (<3, 3-<10, 10-<20, 20-<30, 30-<40, 40-<60, 60-<70, 70+).
#' @param region one of "WHO Region" or "State Region"
#' @param vac_type one of "People", "Fully", "Booster", or "Pop18" specifying the vaccine metric desired
#'
#' @return
#' Produces a map of vaccination coverage by country
#'
#'
#' @export

map_vaccinations <- function(df, region = c("WHO Region", "State Region"), vac_type = c("People", "Fully", "Booster", "Pop18")) {

  region <- match.arg(region)
  vac_type <- match.arg(vac_type)

  if (region == "State Region") {
    who_region <- unique(df$state_region)
  } else {
    who_region <- unique(df$who_region)
  }

  who_regs <- length(who_region)

  # If we pass more than one region, then we set the value to "None"
  # So switch works correctly
  if (who_regs > 1) {
    who_region <- "None"
  }

  if (length(unique(df$date)) > 1) {
    warning("Your dataframe has more than 1 date! This is a cross-sectional visualization!")
  }

  bbox <- bbox_fun(who_region, df)

  cat_labs <- c("<3", "3- <10", "10- <20", "20- <30", "30- <40", "40- <60", "60- <70", "70+")

  if (vac_type == "People") {
    cat_vals <- c("#b1eeec", "#98d1cf", "#7eb3b2", "#659695", "#4c7877", "#335b5a", "#193d3d", "#002020")

    map_template(df, cat_labs, cat_vals) +
      labs(
        title = "People Who Received at Least One Vaccine Dose per 100 People",
        subtitle = paste0("Data as of ", format(max(df$date), "%B %d, %Y"), "\nNumber of people out of 100 who received at least one vaccine dose; \ndoes not represent percent of population who completed primary vaccination series"),
        caption = "Note:
       -Countries in white do not have data reported for total people vaccinated
       -Vaccine data are incomplete and data may be out of date
       -People vaccinated per 100 people represents total population (all ages)"
      ) +
      guides(fill = guide_legend(title = "People \nVaccinated \nper 100 \nPeople")) +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )

  } else if (vac_type == "Fully") {
    cat_vals <- c("#ccece6", "#afdacb", "#92c8b1", "#75b696", "#57a37c", "#3a9161", "#1d7f47", "#006d2c")
    map_template(df, cat_labs, cat_vals) +
      labs(
        title = "People Who Completed Primary Vaccination Series \nper 100 People",
        subtitle = paste0("Data as of ", format(max(df$date), "%B %d, %Y"), "\nRepresents percent of population who completed primary vaccination series"),
        caption = "Note:
       -Countries in white do not have data reported for completed primary vaccination series
       -Vaccine data are incomplete and data may be out of date
       -People vaccinated per 100 people represents total population (all ages)
       -Completed primary vaccination series means a person has received all recommended doses in their primary series of COVID-19 vaccine"
      ) +
      guides(fill = guide_legend(title = "People Who \nCompleted Primary \nVaccination Series \nper 100 People")) +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )
  } else if (vac_type == "Booster") {
    cat_vals <- c("#DEC9E9", "#CCB6E0", "#BBA4D7", "#A991CE", "#977FC5", "#856CBC", "#745AB3", "#6247AA")
    map_template(df, cat_labs, cat_vals) +
      labs(
        title = "Total Boosters per 100 People",
        subtitle = paste0("Data as of ", format(max(df$date), "%B %d, %Y"), "\nNumber of boosters administered per 100 people; \ndoes not represent percent of population boosted"),
        caption = "Note:
         -Countries in white do not have data reported for boosters
         -Vaccine data are incomplete and data may be out of date
         -Total boosters per 100 people represents total population (all ages)"
      ) +
      guides(fill = guide_legend(title = "Total \nBoosters \nper 100 \nPeople")) +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )
  } else if (vac_type == "Pop18") {
    cat_vals <- c("#e6f3e8", "#a7efd9", "#15A8C9", "#137DB8", "#1151A7", "#0f2696", "#0a1a68", "#050d3a")
    map_template(df, cat_labs, cat_vals) +
      labs(
        title = "People Vaccinated per 100 Eligible People",
        subtitle = paste0("Data as of ", format(max(df$date), "%B %d, %Y"), "\nNumber of eligible people out of 100 who received at least one vaccine dose; does not represent percent of \npopulation who completed primary vaccination series"),
        caption =  "Note: Eligible population represents adult population for ages >=18; some countries may be vaccinating ages 12+"
      ) +
      guides(fill = guide_legend(title = "People \nVaccinated \nper 100 \nEligible People")) +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )
  }
}

