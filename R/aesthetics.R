#' Aesthetics for WHO regions
who_values <- data.frame(
  cat_values = c("AMRO", "EURO", "SEARO", "EMRO", "AFRO", "WPRO"),
  cat_names = c("Americas", "Europe", "Southeast Asia", "Eastern Mediterranean", "Africa", "Western Pacific"),
  cat_colors = c("#aa001e", "#e7b351", "#00818a", "#d26230", "#005e70", "#9c4f9f"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid", "solid")
)

#' Aesthetics for State regions
state_values <- data.frame(
  cat_values = c(
    "East Asia and the Pacific",
    "Europe and Eurasia",
    "Near East (Middle East and Northern Africa)",
    "South and Central Asia",
    "Sub-Saharan Africa",
    "Western Hemisphere",
    "US",
    "None-state"
  ),
  cat_names = c(
    "East Asia and the Pacific",
    "Europe and Eurasia",
    "Near East (Middle East and North Africa)",
    "South and Central Asia",
    "Sub-Saharan Africa",
    "Western Hemisphere (not incl US)",
    "US",
    "None-state"
  ),
  cat_colors = c("#d00000", "#ffba08", "#3f88c5", "#032b43", "#136f63", "#a5c651", "#d64550", "#808080"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid", "solid", "dashed", "solid")
)

#' Income Aesthetics
income_values <- data.frame(
  cat_values = c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"),
  cat_names = c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"),
  cat_colors = c("#045a8d", "#74a9cf", "#fdbb84", "#d7301f", "#808080"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid")
)