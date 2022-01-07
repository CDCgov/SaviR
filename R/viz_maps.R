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

  if (category_color_labels == "None") {
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
        na.value = "white",
        drop = F,
        na.translate = T
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15, face = "bold", family = "Calibri"),
        plot.subtitle = ggplot2::element_text(size = 10, family = "Calibri", margin = margin(0, 0, 5, 0)),
        plot.caption = ggplot2::element_text(size = 8, family = "Calibri", hjust = 0),
        plot.caption.position = "plot",
        legend.position = c(0.01, 0.00),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.key.size = unit(0.5, "cm"),
        legend.margin = ggplot2::margin(3, 3, 3, 3),
        legend.title = ggplot2::element_text(size = 10, family = "Calibri"),
        legend.text = ggplot2::element_text(size = 8, family = "Calibri")
      )
  }

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
      na.value = "white",
      drop = F,
      labels = category_color_labels, # Param
      na.translate = T
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold", family = "Calibri"),
      plot.subtitle = ggplot2::element_text(size = 10, family = "Calibri", margin = margin(0, 0, 5, 0)),
      plot.caption = ggplot2::element_text(size = 8, family = "Calibri", hjust = 0),
      plot.caption.position = "plot",
      legend.position = c(0.01, 0.00),
      legend.justification = c("left", "bottom"),
      legend.box.just = "left",
      legend.key.size = unit(0.5, "cm"),
      legend.margin = ggplot2::margin(3, 3, 3, 3),
      legend.title = ggplot2::element_text(size = 10, family = "Calibri"),
      legend.text = ggplot2::element_text(size = 8, family = "Calibri")
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_burden
#' @description Cross-sectional map: Average daily incidence for the past 7 days for each country.
#' @param df A dataframe with the following: region, country, date, incidence as 4-level factors (0- <1, 1- <10, 10- <25, 25+)
#' Produces an epi curve, region stacked bar plot for each epi-week (Monday-Sunday).
#' Input df SHOULD ONLY HAVE ONE DATE!

#'
#' @export

map_burden <- function(df, region = "WHO Region") {
  if (length(unique(df$date)) > 1) {
    warning("Your dataframe has more than 1 date! This is a cross-sectional visualization!")
  }

  if (length(unique(df$who_region)) == 1) {
    if (grepl("WHO", region, fixed = TRUE)) {
      if (df$who_region == "EURO") {
        bbox <- sf::st_bbox(c(xmin = -2500000, ymin = 2000000, xmax = 7000000, ymax = 8500000))
      } else if (df$who_region == "AMRO") {
        bbox <- sf::st_bbox(c(xmin = -10775454, ymin = -5900074, xmax = -3072374, ymax = 5318372))
      } else if (df$who_region == "SEARO") {
        bbox <- sf::st_bbox(c(xmin = 6284395, ymin = -1808021, xmax = 13315540, ymax = 3796098))
      } else if (df$who_region == "EMRO") {
        bbox <- sf::st_bbox(c(xmin = -2500688.1, ymin = -850026.8, xmax = 6918436.7, ymax = 6245846.3))
      } else if (df$who_region == "AFRO") {
        bbox <- sf::st_bbox(c(xmin = -1800000, ymin = -3900074, xmax = 4700000, ymax = 4018372))
      } else {
        bbox <- sf::st_bbox(sf::st_as_sf(df))
      }
    } else if (grepl("State", region, fixed = TRUE)) {
      bbox <- sf::st_bbox(sf::st_as_sf(df))
    }
    subt <- paste0("Average daily incidence over the past 7 days per 100,000 \npopulation ", str_squish(format(max(df$date), "%B %e, %Y")))
  } else {
    bbox <- sf::st_bbox(sf::st_as_sf(df))
    subt <- paste0("Average daily incidence over the past 7 days per 100,000 population ", str_squish(format(max(df$date), "%B %e, %Y")))
  }

  cat_labs <- c("0- <1", "1- <10", "10- <25", "25+")
  cat_vals <- c("#f1e5a1", "#e7b351", "#d26230", "#aa001e")

  map_template(df, cat_labs, cat_vals) +
    ggplot2::coord_sf(
      xlim = bbox[c(1, 3)],
      ylim = bbox[c(2, 4)]
    ) +
    ggplot2::labs(fill = "Average \nDaily \nIncidence \n(past 7 days) \nper 100,000") +
    ggplot2::labs(
      title = "Burden",
      subtitle = subt
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_trend
#' @description Cross-sectional map: Average daily incidence for the past 7 days for each country.
#' @param df A dataframe with the following: region, country, date, percent_change as 6-level factors (0- <1, 1- <10, 10- <25, 25+).
#' Input df SHOULD ONLY HAVE ONE DATE!

#'
#' @export

map_trend <- function(df, region = "WHO Region") {
  if (length(unique(df$date)) > 1) {
    warning("Your dataframe has more than 1 date! This is a cross-sectional visualization!")
  }

  if (length(unique(df$who_region)) == 1) {
    if (grepl("WHO", region, fixed = TRUE)) {
      if (df$who_region == "EURO") {
        bbox <- sf::st_bbox(c(xmin = -2500000, ymin = 2000000, xmax = 7000000, ymax = 8500000))
      } else if (df$who_region == "AMRO") {
        bbox <- sf::st_bbox(c(xmin = -10775454, ymin = -5900074, xmax = -3072374, ymax = 5318372))
      } else if (df$who_region == "SEARO") {
        bbox <- sf::st_bbox(c(xmin = 6284395, ymin = -1808021, xmax = 13315540, ymax = 3796098))
      } else if (df$who_region == "EMRO") {
        bbox <- sf::st_bbox(c(xmin = -2500688.1, ymin = -850026.8, xmax = 6918436.7, ymax = 6245846.3))
      } else if (df$who_region == "AFRO") {
        bbox <- sf::st_bbox(c(xmin = -1800000, ymin = -3900074, xmax = 4700000, ymax = 4018372))
      } else {
        bbox <- sf::st_bbox(sf::st_as_sf(df))
      }
    } else if (grepl("State", region, fixed = TRUE)) {
      bbox <- sf::st_bbox(sf::st_as_sf(df))
    }
  } else {
    bbox <- sf::st_bbox(sf::st_as_sf(df))
  }

  cat_labs <- c(">=50% decrease", "0 - <50% decrease", ">0 - <=50% increase", ">50 - <=100% increase", ">100 - <=200% increase", ">200% increase")
  cat_vals <- c("#1f9fa9", "#c0ebec", "#e57e51", "#d26230", "#c92929", "#7c0316")

  map_template(df, cat_labs, cat_vals) +
    ggplot2::coord_sf(
      xlim = bbox[c(1, 3)],
      ylim = bbox[c(2, 4)]
    ) +
    ggplot2::labs(fill = "Percent \nChange From \nPrevious Week") +
    ggplot2::labs(
      title = "Recent Trends",
      subtitle = paste0("Percent change in cases from 7-day period ending ", format((unique(df$date)), "%B %d, %Y"), "\ncompared to previous 7-day period ending ", format(max(df$date) - 7, "%B %d, %Y"))
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title map_vaccinations
#' @description Cross-sectional map: People vaccinated per 100 for each country or Fully vaccinated.
#' @param df A dataframe with the following: region, country, date, people vaccinated per 100 AS 8-level factors (<3, 3-<10, 10-<20, 20-<30, 30-<40, 40-<60, 60-<70, 70+).

#'
#' @export

map_vaccinations <- function(df, region = "WHO Region", vac_type = c("People", "Fully")) {
  if (length(unique(df$who_region)) == 1) {
    if (grepl("WHO", region, fixed = TRUE)) {
      if (df$who_region == "EURO") {
        bbox <- sf::st_bbox(c(xmin = -2500000, ymin = 2000000, xmax = 7000000, ymax = 8500000))
      } else if (df$who_region == "AMRO") {
        bbox <- sf::st_bbox(c(xmin = -10775454, ymin = -5900074, xmax = -3072374, ymax = 5318372))
      } else if (df$who_region == "SEARO") {
        bbox <- sf::st_bbox(c(xmin = 6284395, ymin = -1808021, xmax = 13315540, ymax = 3796098))
      } else if (df$who_region == "EMRO") {
        bbox <- sf::st_bbox(c(xmin = -2500688.1, ymin = -850026.8, xmax = 6918436.7, ymax = 6245846.3))
      } else if (df$who_region == "AFRO") {
        bbox <- sf::st_bbox(c(xmin = -1800000, ymin = -3900074, xmax = 4700000, ymax = 4018372))
      } else {
        bbox <- sf::st_bbox(sf::st_as_sf(df))
      }
    } else if (grepl("State", region, fixed = TRUE)) {
      bbox <- sf::st_bbox(sf::st_as_sf(df))
    }
  } else {
    bbox <- sf::st_bbox(sf::st_as_sf(df))
  }

  cat_labs <- c("<3", "3- <10", "10- <20", "20- <30", "30- <40", "40- <60", "60- <70", "70+")

  if (vac_type == "People") {
    cat_vals <- c("#b1eeec", "#98d1cf", "#7eb3b2", "#659695", "#4c7877", "#335b5a", "#193d3d", "#002020")
    map_template(df, cat_labs, cat_vals) +
      labs(
        title = "People Vaccinated per 100 People",
        subtitle = paste0("Data as of ", format(max(df$date), "%B %d, %Y"), "\nNumber of people out of 100 who received at least one vaccine dose; \ndoes not represent percent of population fully vaccinated"),
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
  } else {
    cat_vals <- c("#ccece6", "#afdacb", "#92c8b1", "#75b696", "#57a37c", "#3a9161", "#1d7f47", "#006d2c")
    map_template(df, cat_labs, cat_vals) +
      labs(
        title = "People Fully Vaccinated per 100 People",
        subtitle = paste0("Data as of ", format(max(df$date), "%B %d, %Y"), "\nRepresents percent of population fully vaccinated"),
        caption = "Note:
       -Countries in white do not have data reported for fully vaccinated
       -Vaccine data are incomplete and data may be out of date
       -People vaccinated per 100 people represents total population (all ages)"
      ) +
      guides(fill = guide_legend(title = "People \nFully \nVaccinated \nper 100 \nPeople")) +
      ggplot2::coord_sf(
        xlim = bbox[c(1, 3)],
        ylim = bbox[c(2, 4)]
      )
  }
}
