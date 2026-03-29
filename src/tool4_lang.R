tool4_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_tool4")
  )

tool4_df <- tool4_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

tool4_data <- tool4_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_tool4 <- tool4_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)

trend_data <- combined_tool4 |>
  dplyr::filter(!is.na(tool4can)) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::count(year, tool4can, name = "n")

trend_data <- trend_data |>
  dplyr::group_by(year) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

plot_data_full <- tidyr::complete(
  trend_data,
  year = 2016:2022,
  tool4can,
  fill = list(prop = NA)
)

plot_data_full <- plot_data_full |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    ypos = base::cumsum(prop) - 0.5 * prop
  ) |>
  dplyr::ungroup()

plot_data_full <- plot_data_full |>
  dplyr::mutate(
    tool4can = dplyr::case_when(
      tool4can %in% c("Python", "R", "C++", "Java") ~ tool4can,
      TRUE ~ "Other"
    )
  )

gg <- ggplot2::ggplot(
  plot_data_full,
  ggplot2::aes(x = factor(year), y = prop, fill = tool4can)
) +

  ggplot2::geom_bar(stat = "identity", position = "fill", size = 0.3) +

  # Labels
  ggplot2::geom_text(
    ggplot2::aes(
      y = ypos,
      label = scales::percent(prop, accuracy = 1)
    ),
    size = 3,
    na.rm = TRUE
  ) +

  # Shade missing years
  ggplot2::annotate(
    "rect",
    xmin = 4 - 0.5, xmax = 6 + 0.5,
    ymin = 0, ymax = 1,
    fill = "grey80", alpha = 1
  ) +

  ggplot2::annotate(
    "text",
    x = 5, y = 0.5,
    label = "No survey data (2019–2021)",
    size = 4
  ) +

  ggplot2::scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +

  ggplot2::labs(
    x = "Year",
    y = "Proportion of Responses",
    fill = "Programming Language",
    title = "Programming Languages Used at Work"
  ) +

  ggthemes::theme_economist() +

  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 10)
  )

gg

# the data needs to be cleaned since multiple entries like "python", "Python",
# "python programming", etc. so it needs to make sure the case-sensitivity
# is considered and different ways of writing as well

ggplot2::ggsave("fig/tool4_languages.pdf", gg,
                width = 11, height = 5, units = "in")

ggplot2::ggsave("fig/tool4_languages.png", gg,
                width = 11, height = 5, units = "in", dpi = 300)
