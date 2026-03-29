proj5_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_proj5")
  )

proj5_df <- proj5_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

proj5_data <- proj5_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_proj5 <- proj5_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)

trend_data_proj5 <- combined_proj5 |>
  dplyr::filter(!is.na(proj5zaf)) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::count(year, proj5zaf, name = "n") |>
  dplyr::group_by(year) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

plot_data_proj5 <- tidyr::complete(
  trend_data_proj5,
  year = 2016:2022,
  proj5zaf,
  fill = list(prop = NA)
)

plot_data_proj5 <- plot_data_proj5 |>
  dplyr::mutate(
    proj5zaf = dplyr::recode(
      proj5zaf,
      "GIT" = "Git",
    )
  )

# Compute position for labels inside stacks
plot_data_proj5 <- plot_data_proj5 |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    ypos = base::cumsum(prop) - 0.5 * prop
  ) |>
  dplyr::ungroup()

gg_proj5 <- ggplot2::ggplot(
  plot_data_proj5,
  ggplot2::aes(x = factor(year), y = prop, fill = proj5zaf)
) +

  ggplot2::geom_bar(stat = "identity", position = "fill", size = 0.3) +

  ggplot2::annotate(
    "rect",
    xmin = 4 - 0.5, xmax = 6 + 0.5,
    ymin = 0, ymax = 1,
    fill = "grey80", alpha = 1.2
  ) +

  ggplot2::annotate(
    "text",
    x = 5, y = 0.5,
    label = "No survey data (2019–2021)",
    size = 4, hjust = 0.5
  ) +

  ggplot2::geom_text(
    ggplot2::aes(label = ifelse(!is.na(prop), scales::percent(prop, accuracy = 1), "")),
    position = ggplot2::position_fill(vjust = 0.5),
    size = 3
  ) +

  ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  ggplot2::labs(
    x = "Year",
    y = "Proportion of Responses",
    fill = "Version Control Tool",
    title = "Version Control Tools Used"
  ) +

  ggthemes::theme_economist() +
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 10)
  )

gg_proj5

ggplot2::ggsave("fig/proj5_version_control.pdf", gg_proj5, width = 11, height = 5, units = "in")
ggplot2::ggsave("fig/proj5_version_control.png", gg_proj5, width = 11, height = 5, units = "in", dpi = 300)
