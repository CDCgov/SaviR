# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve
#' @description (EPI WEEK) Visualize epi curve by epi-weeks (Monday-Sunday) and by WHO region(s), State region(s), or Income levels.
#'
#' @param df A dataframe with the following: date, new_cases and one of these columns for by_cat: who_region, state_region, or incomelevel_value.
#' Produces an epi curve, stacked bar plot for each epi-week (Monday-Sunday).
#' @param by_cat = "WHO Region" (default), "State Region" or "Income Level"
#' @param legend Default "in" - position legend inside the plot area.
#' @param transparent Default TRUE - returns a transparent plot.
#'

#' @import scales
#' @export

plot_epicurve <- function(df, type = "cases", by_cat = "WHO Region", legend = "in", transparent = T) {
  if (!type %in% c("cases", "deaths")) {
    stop("Wrong Type! You must use either cases or deaths!")
  }
  if (type == "cases") {
    ylab <- "Weekly Cases"
    heading <- "Confirmed COVID-19 Cases"
  } else if (type == "deaths") {
    ylab <- "Weekly Deaths"
    heading <- "COVID-19 Deaths"
  }
  if (grepl("WHO", by_cat, fixed = TRUE)) {
    col_master <- who_aes
    df_c <- df %>% mutate(cat = factor(who_region, levels = col_master$cat_values))
  } else if (grepl("State", by_cat, fixed = TRUE)) {
    col_master <- state_aes
    df_c <- df %>% mutate(cat = factor(state_region, levels = col_master$cat_values))
  } else if (grepl("Income", by_cat, fixed = TRUE)) {
    col_master <- income_aes
    df_c <- df %>% mutate(cat = factor(incomelevel_value, levels = col_master$cat_values))
  }


  if (length(unique(df_c$cat)) > 1) {
    category_color_labels <- col_master$cat_names
    category_color_values <- col_master$cat_colors
    gtitle <- paste0(heading, " by Week of Report and ", by_cat)
  } else {
    category_color_labels <- col_master[col_master$cat_values == as.character(unique(df_c$cat)), ]$cat_names
    category_color_values <- col_master[col_master$cat_values == as.character(unique(df_c$cat)), ]$cat_colors
    gtitle <- paste0(heading, " - ", category_color_labels)
  }

  df_sum <- df_c %>%
    mutate(week = lubridate::floor_date(date, "week", week_start = 1)) %>%
    group_by(week, cat) %>%
    summarize(val = ifelse(type == "cases", sum(new_cases, na.rm = TRUE), sum(new_deaths, na.rm = TRUE)))

  g <- ggplot2::ggplot(
    data = df_sum,
    mapping = aes(
      x = week,
      y = val,
      fill = cat
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "stack",
      alpha = 0.9
    ) +
    ggplot2::labs(
      title = gtitle,
      subtitle = paste0(
        str_squish(format(min(df_c$date, na.rm = T), "%B %e, %Y")), " - ",
        str_squish(format(max(df_c$date, na.rm = T), "%B %e, %Y"))
      ),
      fill = by_cat
    ) +
    ggplot2::ylab(ylab) +
    ggplot2::xlab("Week of Reporting") +
    ggplot2::scale_x_date(
      limits = c(
        lubridate::floor_date(min(df_c$date, na.rm = T) - 7, "week", week_start = 1),
        lubridate::floor_date(max(df_c$date, na.rm = T) + 7, "week", week_start = 1)
      ),
      breaks = seq.Date(
        from = as.Date(lubridate::floor_date(min(df_c$date, na.rm = T), "week", week_start = 1)),
        to = as.Date(lubridate::floor_date(max(df_c$date, na.rm = T) + 7, "week", week_start = 1)),
        by = "4 weeks"
      ),
      date_labels = "%e %b",
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = scales::comma
    ) +
    ggplot2::scale_fill_manual(
      values = category_color_values,
      labels = category_color_labels
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 17, face = "bold", family = "Calibri"),
      plot.margin = unit(c(5.5, 11, 5.5, 5.5), "points"),
      axis.text.x = ggplot2::element_text(size = 9, family = "Calibri", angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 10, family = "Calibri"),
      axis.title = ggplot2::element_text(size = 12, family = "Calibri"),
      legend.title = ggplot2::element_text(size = 12, face = "bold", family = "Calibri"),
      legend.text = ggplot2::element_text(size = 9, family = "Calibri"),
      legend.key.size = unit(0.5, "cm")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(overide.aex = list(size = 9)))

  if (length(unique(df_c$cat)) > 1) {
    if (legend == "in") {
      g <- g + ggplot2::theme(
        legend.justification = c("left", "top"),
        legend.position = c(0, 1)
      )
    } else {
      g <- g + ggplot2::theme(legend.position = legend)
    }
  } else {
    g <- g + ggplot2::theme(legend.position = "none")
  }

  if (transparent == T) {
    return(g +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "transparent"),
        plot.background = ggplot2::element_rect(fill = "transparent"),
        panel.grid = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(fill = "transparent")
      ))
  } else {
    return(g)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_ind
#' @description (DAILY) Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, date, cases and/or deaths
#' @param type Default cases.
#' @param incidence Default TRUE. Specify inputs are incidence values or not.
#'

#'
#' @export

plot_epicurve_ind <- function(df, type = "cases", incidence = T) {
  if (!type %in% c("cases", "deaths")) {
    stop("Wrong Type! You must use either cases or deaths!")
  }

  if (!incidence %in% c(T, F)) {
    stop("Wrong Incidence! You must use either TRUE or FALSE!")
  }

  if (incidence == F) {
    df %>%
      ggplot2::ggplot(aes(x = date, y = if (type == "cases") {
        cases
      } else {
        deaths
      })) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9, fill = if (type == "cases") {
        "dodgerblue4"
      } else {
        "red4"
      }) +
      ggplot2::theme_classic() +
      ggplot2::ylab(if (type == "cases") {
        "Daily Cases"
      } else {
        "Daily Deaths"
      }) +
      ggplot2::xlab("Date of Reporting") +
      ggplot2::scale_x_date(
        limits = c(
          lubridate::floor_date(min(df$date, na.rm = T) - 7, "week", week_start = 1),
          lubridate::floor_date(max(df$date, na.rm = T) + 7, "week", week_start = 1)
        ),
        breaks = seq.Date(
          from = as.Date(lubridate::floor_date(min(df$date, na.rm = T), "week", week_start = 1)),
          to = as.Date(lubridate::floor_date(max(df$date, na.rm = T) + 7, "week", week_start = 1)),
          by = "3 weeks"
        ),
        date_labels = "%d\n%b"
      ) +
      ggplot2::scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0.01, .1))
      ) +
      ggplot2::labs(
        title = if (type == "cases") {
          paste0("COVID-19 Cases: ", unique(df$country))
        } else {
          paste0("COVID-19 Deaths:", unique(df$country))
        },
        subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ", format(max(df$date, na.rm = T), "%B %d, %Y"))
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", family = "Calibri"),
        axis.text = ggplot2::element_text(size = 10, family = "Calibri"),
        axis.title = ggplot2::element_text(size = 12, family = "Calibri"),
        legend.title = ggplot2::element_text(size = 12, face = "bold", family = "Calibri"),
        legend.text = ggplot2::element_text(size = 9, family = "Calibri")
      )
  } else {
    df %>%
      ggplot2::ggplot(aes(x = date, y = if (type == "cases") {
        cases
      } else {
        deaths
      })) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9, fill = if (type == "cases") {
        "dodgerblue4"
      } else {
        "red4"
      }) +
      ggplot2::theme_classic() +
      ggplot2::ylab(if (type == "cases") {
        "Daily Cases per 100,000 People"
      } else {
        "Daily Deaths per 100,000 People"
      }) +
      ggplot2::xlab("Date of Reporting") +
      ggplot2::scale_x_date(
        limits = c(
          lubridate::floor_date(min(df$date, na.rm = T) - 7, "week", week_start = 1),
          lubridate::floor_date(max(df$date, na.rm = T) + 7, "week", week_start = 1)
        ),
        breaks = seq.Date(
          from = as.Date(lubridate::floor_date(min(df$date, na.rm = T), "week", week_start = 1)),
          to = as.Date(lubridate::floor_date(max(df$date, na.rm = T) + 7, "week", week_start = 1)),
          by = "3 weeks"
        ),
        date_labels = "%d\n%b"
      ) +
      ggplot2::scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0.01, .1))
      ) +
      ggplot2::labs(
        title = if (type == "cases") {
          paste0("COVID-19 Cases per 100,000 People: ", unique(df$country))
        } else {
          paste0("COVID-19 Deaths per 100,000 People: ", unique(df$country))
        },
        subtitle = paste0(format(min(df$date, na.rm = T), "%B %d, %Y"), " - ", format(max(df$date, na.rm = T), "%B %d, %Y"))
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", family = "Calibri"),
        axis.text = ggplot2::element_text(size = 8, family = "Calibri"),
        axis.title = ggplot2::element_text(size = 10, family = "Calibri"),
        legend.title = ggplot2::element_text(size = 12, face = "bold", family = "Calibri"),
        legend.text = ggplot2::element_text(size = 9, family = "Calibri")
      )
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_epidouble
#' @description (EPI WEEK) Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, weekdate, cases and deaths
#'

#'
#' @export

plot_epicurve_epidouble <- function(df) {
  ylim.prim <- c(
    min(df$case, na.rm = T),
    max(df$case, na.rm = T)
  )

  ylim.sec <- c(
    min(df$death, na.rm = T),
    max(df$death, na.rm = T)
  )

  b <- diff(ylim.prim) / diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1]

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes(x = weekdate, y = case, color = "Cases"), stat = "identity", alpha = 0.9, fill = "lightblue") +
    ggplot2::geom_line(aes(x = weekdate, y = a + death * b, group = 1, color = "Deaths"), size = 1) +
    ggplot2::scale_color_manual(
      breaks = c("Cases", "Deaths"),
      values = c("lightblue", "red")
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_date(
      breaks = c(by = "4 weeks"),
      date_labels = "%d\n%b"
    ) +
    ggplot2::scale_y_continuous("Weekly Cases",
      labels = comma,
      sec.axis = sec_axis(~ (. - a) / b, name = "Weekly Deaths", labels = comma)
    ) +
    ggplot2::xlab("Date of Reporting") +
    ggplot2::labs(
      title = paste0("COVID-19: ", unique(df$country)),
      subtitle = paste0("Week of:", format(min(df$weekdate, na.rm = T), "%B %d, %Y"), " - ", format(max(df$weekdate, na.rm = T), "%B %d, %Y"))
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", family = "Calibri"),
      axis.text = ggplot2::element_text(size = 14, family = "Calibri"),
      axis.title = ggplot2::element_text(size = 14, family = "Calibri"),
      legend.position = "top",
      legend.key = element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12, family = "Calibri")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = c("lightblue", NA))))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_epicurve_dailydouble
#' @description (DAILY) Visualize epi curve by cases and deaths.
#' Default viz for individual countries.
#'
#' @param df A dataframe with the following: country, date, cases and deaths
#'

#'
#' @export

plot_epicurve_dailydouble <- function(df) {
  ylim.prim <- c(
    min(df$case, na.rm = T),
    max(df$case, na.rm = T)
  )

  ylim.sec <- c(
    min(df$death, na.rm = T),
    max(df$death, na.rm = T)
  )

  b <- diff(ylim.prim) / diff(ylim.sec)
  a <- ylim.prim[1] - b * ylim.sec[1]

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(aes(x = date, y = case, color = "Cases"), stat = "identity", alpha = 0.9, fill = "lightblue") +
    ggplot2::geom_line(aes(x = date, y = a + death * b, group = 1, color = "Deaths"), size = 1) +
    ggplot2::scale_color_manual(
      breaks = c("Cases", "Deaths"),
      values = c("lightblue", "red")
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous("Cases",
      labels = comma,
      sec.axis = sec_axis(~ (. - a) / b, name = "Deaths", labels = comma)
    ) +
    ggplot2::xlab("Date of Reporting") +
    ggplot2::labs(
      title = paste0("COVID-19: ", unique(df$country)),
      subtitle = paste0("Week of:", format(min(df$weekdate, na.rm = T), "%B %d, %Y"), " - ", format(max(df$weekdate, na.rm = T), "%B %d, %Y"))
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", family = "Calibri"),
      axis.text = ggplot2::element_text(size = 14, family = "Calibri"),
      axis.title = ggplot2::element_text(size = 14, family = "Calibri"),
      legend.position = "top",
      legend.key = element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12, family = "Calibri")
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = c("lightblue", NA))))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_riskmatrix
#' @description Plot risk matrix.
#' @param df A dataframe with riskmatrix stats.
#' @import ggrepel

#'
#' @export

plot_riskmatrix <- function(df, region = "WHO Region", v = T, h = T) {
  if (grepl("WHO", region, fixed = TRUE)) {
    col_master <- who_aes
    df_r <- df %>% mutate(reg = factor(who_region, levels = col_master$cat_values))
  } else if (grepl("State", region, fixed = TRUE)) {
    col_master <- state_aes
    df_r <- df %>% mutate(reg = factor(state_region, levels = col_master$cat_values))
  }

  category_color_labels <- col_master$cat_names
  category_color_values <- col_master$cat_colors

  r <- ggplot2::ggplot(data = df_r, aes(x = percent_change_case, y = week_case_incidence)) +
    ggplot2::geom_point(aes(size = week_case, color = reg), alpha = 0.7) +
    ggplot2::scale_color_manual(
      values = category_color_values,
      labels = category_color_labels
    ) +
    ggrepel::geom_text_repel(aes(label = labels),
      color              = "black",
      size               = 2.7,
      min.segment.length = 0,
      seed               = 42,
      box.padding        = 0.6
    ) +
    ggplot2::scale_size(
      name = "Weekly Cases",
      range = c(2, 12),
      breaks = c(100, 1000, 10000, 100000, 250000, 500000, 750000),
      labels = scales::comma
    ) +
    ggplot2::guides(color = guide_legend(override.aes = list(size = 6), order = 1)) +
    ggplot2::xlim(min(df$percent_change_case, na.rm = T), max(df$percent_change_case, na.rm = T)) +
    ggplot2::ylim(0, max(df$week_case_incidence, na.rm = T)) +
    ggplot2::xlab("% Change in Weekly Cases") +
    labs(color = region) +
    ggplot2::ylab("Average Daily Incidence per 100,000") +
    ggplot2::annotate(geom = "text", x = -133, y = 0.6, label = "< 1.0 per 100k", color = "green3", size = 3) +
    ggplot2::annotate(geom = "text", x = -125, y = 1.7, label = "1.0 - 9.9 per 100k", color = "goldenrod1", size = 3) +
    ggplot2::annotate(geom = "text", x = -122, y = 10.7, label = "10.0 - 24.9 per 100k", color = "orange2", size = 3) +
    ggplot2::annotate(geom = "text", x = -133, y = 25.7, label = "25.0+ per 100k", color = "red3", size = 3) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = element_text(size = 8, family = "Calibri"),
      axis.title = element_text(size = 10, family = "Calibri"),
      legend.text = element_text(size = 7, family = "Calibri"),
      legend.title = element_text(size = 9, family = "Calibri"),
      plot.title = element_text(size = 16, face = "bold", family = "Calibri"),
      plot.subtitle = element_text(size = 11, family = "Calibri"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.caption = element_text(hjust = 0, size = 11, family = "Calibri")
    ) +
    ggplot2::labs(
      title = "Burden and Recent Trends",
      subtitle = paste0(
        "Average daily incidence per 100,000 population and 7-day percent change, by new cases in past 7 days\n",
        format(max(df$date) - 13, "%B %d, %Y"), " - ", format(max(df$date) - 7, "%B %d, %Y"), " to ",
        format(max(df$date) - 6, "%B %d, %Y"), " - ", format(max(df$date), "%B %d, %Y")
      ),
      caption = "Notes:
      - Includes countries with a population greater than 10 million people and more than 100 cases in the last week
      - Countries with a population over 10 million are labeled if they are among the top ten highest countries for cases,
        incidence, or weekly percent change in cases."
    )

  if (v == T) {
    r <- r + ggplot2::geom_vline(xintercept = 0, color = "gray50", lty = 2)
  }

  if (h == T) {
    r <- r + ggplot2::geom_hline(yintercept = 0, color = "green3", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 1.0, color = "goldenrod1", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 10.0, color = "orange2", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 25.0, color = "red3", linetype = "dashed")
  }

  return(r)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_vaxcoverage
#' @description Plot vaccination coverage (partial or fully) by WHO region(s), State region(s), or Income levels.
#' @param df A dataframe with vaccination stats.
#'

#'
#' @export

plot_vaxcoverage <- function(df, type = "partial", by_cat = "State Region") {
  if (by_cat == "WHO Region") {
    col_master <- who_aes
    df_c <- df %>% mutate(cat = factor(who_region, levels = who_aes$cat_values))
  } else if (by_cat == "State Region") {
    col_master <- state_aes
    df_c <- df %>% mutate(cat = factor(state_region, levels = col_master$cat_values))
  } else if (by_cat == "Income Level") {
    col_master <- income_aes
    df_c <- df %>% mutate(cat = factor(incomelevel_value, levels = col_master$cat_values))
  }

  category_color_labels <- col_master$cat_names
  category_color_values <- col_master$cat_colors

  if (type == "partial") {
    df_c <- df_c %>%
      group_by(cat) %>%
      mutate(
        rank_people = dense_rank(-people_vaccinated_per_hundred),
        rank_total = dense_rank(-total_vaccinations)
      ) %>%
      mutate(country_labels = case_when(
        rank_people %in% 1:3 ~ country,
        rank_total %in% 1:3 ~ country
      )) %>%
      ungroup()
    ptitle <- paste0("People Vaccinated per 100 people by ", by_cat, ", ", format(max(df$date), "%B %d, %Y"))
    xlabel <- "People Vaccinated per 100"
    cap <- "Notes:
    - People Vaccinated per 100: number of people who received at least one vaccine dose; does not represent
      percent of population fully vaccinated
    - Total vaccine doses administered: total doses given, does not represent number of people vaccinated
    - Countries are labeled such that within each group, labeled countries are those that are the top 3 ranking countries
      for people vaccinated per 100 and the top 3 ranking countries for total vaccine doses administered
    - Vaccine data are incomplete and data may be out of date"
  } else {
    df_c <- df_c %>%
      group_by(cat) %>%
      mutate(
        rank_people = dense_rank(-people_fully_vaccinated_per_hundred),
        rank_total = dense_rank(-total_vaccinations)
      ) %>%
      mutate(country_labels = case_when(
        rank_people %in% 1:3 ~ country,
        rank_total %in% 1:3 ~ country
      )) %>%
      ungroup()
    ptitle <- paste0("People Fully Vaccinated per 100 people by ", by_cat, ", ", format(max(df$date), "%B %d, %Y"))
    xlabel <- "People Fully Vaccinated per 100"
    cap <- "Notes:
    - Total vaccine doses administered: total doses given, does not represent number of people fully vaccinated
    - Countries are labeled such that within each group, labeled countries are those that are the top 3 ranking countries
      for people fully vaccinated per 100 and the top 3 ranking countries for total vaccine doses administered
    - Vaccine data are incomplete and data may be out of date"
  }

  my_pal_vax <- function(range = c(3, 25)) {
    force(range)
    function(x) scales::rescale(x, to = range, from = c(0, 1))
  }

  ggplot2::ggplot(df_c, aes(
    x = if (type == "partial") {
      people_vaccinated_per_hundred
    } else {
      people_fully_vaccinated_per_hundred
    },
    y = cat
  )) +
    ggplot2::geom_point(aes(size = total_vaccinations, fill = cat),
      shape = 21,
      color = "gray60",
      alpha = 0.8
    ) +
    ggrepel::geom_text_repel(aes(label = country_labels, point.size = total_vaccinations),
      color              = "gray25",
      min.segment.length = 0,
      max.overlaps       = Inf,
      size               = 3,
      force              = 0.7,
      force_pull         = 0.7,
      direction          = "both",
      box.padding        = 0.4,
      point.padding      = 0
    ) +
    ggplot2::continuous_scale(
      aesthetics = c("size", "point.size"), scale_name = "size", palette = my_pal_vax(),
      labels = scales::comma, breaks = c(1000000, 50000000, 300000000, 750000000),
      guide = guide_legend(override.aes = list(label = "")),
      name = "Total vaccine \ndoses administered"
    ) +
    ggplot2::scale_fill_manual(
      name = by_cat,
      values = category_color_values,
      labels = category_color_labels
    ) +
    ggplot2::scale_x_continuous(name = xlabel) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 8))) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = ptitle,
      caption = cap,
      legend.title = element_text(size = 10, face = "bold", family = "Calibri")
    ) +
    ggplot2::theme(
      plot.title = element_text(size = 14, face = "bold", family = "Calibri"),
      axis.title = element_text(size = 12, family = "Calibri"),
      plot.caption = element_text(hjust = 0, size = 12, family = "Calibri")
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title plot_vaxcurve
#' @description Visualize vaccine coverage by date of reporting and by WHO region(s), State region(s), or Income levels.
#'
#' @param df A dataframe with the following: date, people_vaccinated_per_hundred or people_fully_vaccinated_per_hundred, and one of these columns for by_cat: who_region, state_region, or incomelevel_value.
#' @param type = "partial" (default) for partial vaccinated or "full" for fully vaccinated
#' @param by_cat = "State Region" (default), "WHO Region" or "Income Level"
#' @param countries = "All" (default) for all countries or "AMC/AU" for AMC/AU countries (n=100)
#'

#'
#' @export

plot_vaxcurve <- function(df, type = "partial", by_cat = "Dept. of State Region", countries = "All") {
  if (grepl("WHO", by_cat, fixed = TRUE)) {
    col_master <- who_aes
    df_c <- df %>% mutate(cat = factor(who_region, levels = who_aes$cat_values))
  } else if (grepl("State", by_cat, fixed = TRUE)) {
    col_master <- state_aes

    if (countries == "AMC/AU") {
      col_master <- col_master %>%
        filter(cat_values != "US")
    }
    df_c <- df %>% mutate(cat = factor(state_region, levels = col_master$cat_values))
  } else if (grepl("Income", by_cat, fixed = TRUE)) {
    col_master <- income_aes
    df_c <- df %>% mutate(cat = factor(incomelevel_value, levels = col_master$cat_values))
  }

  category_labels <- col_master$cat_names
  category_color_values <- col_master$cat_colors
  category_line_values <- col_master$cat_lines
  if (type == "full") {
    gtitle <- "People fully vaccinated per 100 people"
  } else {
    gtitle <- "People with at least one vaccine dose per 100 people"
  }
  if (countries != "All") {
    gtitle <- paste0(gtitle, " in ", countries, " countries")
  }

  g <- ggplot2::ggplot(
    data = df_c,
    mapping = aes(
      x = date,
      y = if (type == "full") {
        people_fully_vaccinated_per_hundred_r
      } else {
        people_vaccinated_per_hundred_r
      },
      colour = cat,
      linetype = cat
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = gtitle,
      subtitle = paste0("by ", by_cat),
      color = by_cat,
      linetype = by_cat
    ) +
    ggplot2::xlab("Date of Reporting") +
    ggplot2::ylab(if (type == "full") {
      "People fully vaccinated per 100"
    } else {
      "People vaccinated with at least one dose per 100"
    }) +
    ggplot2::scale_x_date(
      labels = function(x) format(x, "%b%e,\n%Y"),
      limits = c(min(df_c$date), max(df_c$date)),
      breaks = "1 month"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    ggplot2::scale_color_manual(
      values = category_color_values,
      labels = category_labels
    ) +
    ggplot2::scale_linetype_manual(
      values = category_line_values,
      labels = category_labels
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = element_text(size = 15, family = "Calibri", face = "bold"),
      plot.subtitle = element_text(size = 14, family = "Calibri", face = "bold", margin = margin(0, 0, 10, 0)),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      axis.title.x = element_text(size = 12, family = "Calibri", margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(size = 12, family = "Calibri", margin = margin(0, 10, 0, 0)),
      axis.text = element_text(size = 10, family = "Calibri"),
      legend.title = element_text(size = 12, family = "Calibri", face = "bold"),
      legend.text = element_text(size = 8, family = "Calibri"),
      legend.position = c(0.2, 0.7)
    )

  if (type == "full") {
    return(g + ggplot2::geom_hline(yintercept = 20, color = "black"))
  } else {
    return(g)
  }
}
