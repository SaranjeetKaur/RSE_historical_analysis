proj4_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_proj4")
  )

proj4_df <- proj4_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

proj4_data <- proj4_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_proj4 <- proj4_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)

trend_data <- combined_proj4 |>
  dplyr::filter(!is.na(proj4can)) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::count(year, proj4can, name = "n")

trend_data <- trend_data |>
  dplyr::group_by(year) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

plot_data_full <- tidyr::complete(
  trend_data,
  year = 2016:2022,
  proj4can,
  fill = list(prop = NA)
)

plot_data_full <- plot_data_full |>
  dplyr::mutate(
    proj4can = dplyr::recode(
      proj4can,
      "test engineers conduct testing" = "Test engineers conduct testing",
      "The developers do their own testing" = "Developers conduct testing"
    )
  )

plot_data_full <- plot_data_full |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    ypos = base::cumsum(prop) - 0.5 * prop
  ) |>
  dplyr::ungroup()

gg <- ggplot2::ggplot(plot_data_full, ggplot2::aes(x = factor(year), y = prop, fill = proj4can)) +

  ggplot2::geom_bar(stat = "identity", position = "fill", size = 0.3) +

  ggplot2::annotate(
    "rect",
    xmin = 4 - 0.5, xmax = 6 + 0.5,
    ymin = 0, ymax = 1,
    fill = "grey80", alpha = 1.4
  ) +

  ggplot2::annotate(
    "text",
    x = 6, y = 0.5,
    label = "No survey data (2019–2021)",
    size = 4, hjust = 0.9
  ) +

  ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  ggplot2::labs(
    x = "Year",
    y = "Proportion of Responses",
    fill = "Testing Status",
    title = "Testing Practices Over the Years"
  ) +

  ggthemes::theme_economist() +
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 10)
  )

gg

ggplot2::ggsave("fig/proj4_testing_practices.pdf", gg, width = 11, height = 5, units = "in")
ggplot2::ggsave("fig/proj4_testing_practices.png", gg, width = 11, height = 5, units = "in", dpi = 300)
