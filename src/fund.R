fund2_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_fund2")
  )

fund2_df <- fund2_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

fund2_data <- fund2_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_fund2 <- fund2_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)

trend_data <- combined_fund2 |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::count(year, fund2, name = "n") |>
  dplyr::group_by(year) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

plot_data_full <- tidyr::complete(
  trend_data,
  year = 2016:2022,
  fund2,
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
    fund2 = dplyr::case_when(
      fund2 %in% c("Grants", 
      "Institutional support", 
      "Industry support",
      "Consulting & services",
      "I volunteer my time") ~ fund2,
      TRUE ~ "Other"
    )
  )

gg <- ggplot2::ggplot(
  plot_data_full,
  ggplot2::aes(x = factor(year), y = prop, fill = fund2)
) +

  ggplot2::geom_bar(stat = "identity", position = "fill", size = 0.3) +

  ggplot2::geom_text(
    ggplot2::aes(
      y = ypos,
      label = scales::percent(prop, accuracy = 1)
    ),
    size = 3,
    na.rm = TRUE
  ) +

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
    size = 5
  ) +

  ggplot2::scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +

  ggplot2::labs(
    x = "Year",
    y = "Proportion of Responses",
    fill = "Funding Source",
    title = "Funding Sources for RSE Work"
  ) +

  ggthemes::theme_economist() +

  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 10)
  )

gg

ggplot2::ggsave("fig/fund2_funding_sources.pdf", gg,
                width = 11, height = 5, units = "in")

ggplot2::ggsave("fig/fund2_funding_sources.png", gg,
                width = 11, height = 5, units = "in", dpi = 300)
