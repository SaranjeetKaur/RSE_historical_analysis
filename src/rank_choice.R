prevEmp2_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_prevEmp2")
  )

prevEmp2_df <- prevEmp2_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

prevEmp2_data <- prevEmp2_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_prevEmp2 <- prevEmp2_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)

ranking_data <- combined_prevEmp2 |>
  dplyr::filter(!is.na(Choice), !is.na(Ranking)) |>
  dplyr::mutate(
    year = as.numeric(year),
    Ranking = as.numeric(Ranking)
  )

mean_rank <- ranking_data |>
  dplyr::group_by(year, Choice) |>
  dplyr::summarise(
    avg_rank = base::mean(Ranking, na.rm = TRUE),
    .groups = "drop"
  )

mean_rank_full <- tidyr::complete(
  mean_rank,
  year = 2016:2022,
  Choice,
  fill = list(avg_rank = NA)
)

gg_rank <- ggplot2::ggplot(
  mean_rank_full,
  ggplot2::aes(
    x = factor(year),
    y = Choice,
    fill = avg_rank
  )
) +

  # Heatmap tiles
  ggplot2::geom_tile(color = "white") +

  # Shade missing years (2019–2021)
  ggplot2::annotate(
    "rect",
    xmin = 4 - 0.5, xmax = 6 + 0.5,
    ymin = -Inf, ymax = Inf,
    fill = "grey85", alpha = 0.8
  ) +

  # Add annotation text
  ggplot2::annotate(
    "text",
    x = 5,
    y = base::length(unique(mean_rank_full$Choice)) - 3,
    label = "No survey data (2019–2021)",
    size = 5,
    hjust = 0.5
  ) +

  # Color scale (NA is blank)
  ggplot2::scale_fill_gradient(
    low = "#1a9641",  # lower rank, more important
    high = "#d7191c", # higher rank, less important
    na.value = "white",
    name = "Avg Rank\n(1 = most important)"
  ) +

  ggplot2::labs(
    x = "Year",
    y = "Factor",
    title = "Factors Influencing Job Acceptance (Ranking choices)"
  ) +

  ggthemes::theme_economist() +

  ggplot2::theme(
    legend.position = "right",
    axis.text.y = ggplot2::element_text(size = 9)
  )

gg_rank

ggplot2::ggsave("fig/rank_choice.pdf", gg_rank, width = 11, height = 5, units = "in")
ggplot2::ggsave("fig/rank_choice.png", gg_rank, width = 11, height = 5, units = "in", dpi = 300)
