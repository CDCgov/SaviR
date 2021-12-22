#' Aesthetics for WHO regions
who_aes <- data.frame(
  cat_values = c("AMRO", "EURO", "SEARO", "EMRO", "AFRO", "WPRO"),
  cat_names = c("Americas", "Europe", "Southeast Asia", "Eastern Mediterranean", "Africa", "Western Pacific"),
  cat_colors = c("#aa001e", "#e7b351", "#00818a", "#d26230", "#005e70", "#9c4f9f"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid", "solid")
)

#' Aesthetics for State regions
state_aes <- data.frame(
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
income_aes <- data.frame(
  cat_values = c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"),
  cat_names = c("High income", "Upper middle income", "Lower middle income", "Low income", "Not classified"),
  cat_colors = c("#045a8d", "#74a9cf", "#fdbb84", "#d7301f", "#808080"),
  cat_lines = c("solid", "solid", "solid", "solid", "solid")
)

#' OWID location renamings
owid_lk <- c(
  "United Kingdom"            = "The United Kingdom",
  "Syria"                     = "Syrian Arab Republic",
  "South Korea"               = "Republic of Korea",
  "Sint Maarten (Dutch part)" = "Sint Maarten",
  "Russia"                    = "Russian Federation",
  "Pitcairn"                  = "Pitcairn Islands",
  "Moldova"                   = "Republic of Moldova",
  "Macao"                     = "Macau",
  "Laos"                      = "Lao People's Democratic Republic",
  "Iran"                      = "Iran (Islamic Republic of)",
  "Curacao"                   = "Curaçao",
  "Cape Verde"                = "Cabo Verde"
)

#' WHO Country renamings / rebinnings
who_lk <- c(
  "Kosovo[1]" = "Kosovo",
  "Bonaire" = "Bonaire, Sint Eustatius, and Saba",
  "Sint Eustatius" = "Bonaire, Sint Eustatius, and Saba",
  "Saba" = "Bonaire, Sint Eustatius, and Saba",
  "Côte d\u2019Ivoire" = "Cote d'Ivoire",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Democratic Republic of the Congo" = "Congo DR",
  "Falkland Islands (Malvinas)" = "Falkland Islands",
  "Iran (Islamic Republic of)" = "Iran",
  "Democratic People's Republic of Korea" = "Korea (North)",
  "Lao People's Democratic Republic" = "Laos",
  "Micronesia (Federated States of)" = "Micronesia",
  "Northern Mariana Islands (Commonwealth of the)" = "Northern Mariana Islands",
  "occupied Palestinian territory, including east Jerusalem" = "Palestinian Territory",
  "Myanmar" = "Burma",
  "Republic of Korea" = "Korea (South)",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Syrian Arab Republic" = "Syria",
  "United Republic of Tanzania" = "Tanzania",
  "United States Virgin Islands" = "Virgin Islands, US",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "The United Kingdom" = "United Kingdom"
)