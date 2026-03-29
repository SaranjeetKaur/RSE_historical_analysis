ukrse3_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_ukrse3")
  )

ukrse3_df <- ukrse3_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

ukrse3_data <- ukrse3_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_ukrse3 <- ukrse3_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)
View(combined_ukrse3)

pathways <- combined_ukrse3 |>
  dplyr::select(year, tidyselect::matches("^ukrse3")) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "ukrse3_0",
    values_to = "value"
  ) |>
  dplyr::filter(value == 1) |>
  dplyr::mutate(
    pathway = stringr::str_remove(ukrse3_0, "^ukrse3[^_]*_?"),
    pathway = stringr::str_replace_all(pathway, "_", " ")
  )

skills <- combined_ukrse3 |>
  dplyr::select(year, tidyselect::matches("^skill2")) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "skill2_0",
    values_to = "value"
  ) |>
  dplyr::filter(value == 1) |>
  dplyr::mutate(
    skill = stringr::str_remove(skill2_0, "^skill2[^_]*_?"),
    skill = stringr::str_replace_all(skill, "_", " ")
  )

pathways <- pathways |> dplyr::mutate(id = dplyr::row_number())
skills   <- skills   |> dplyr::mutate(id = dplyr::row_number())

pathway_skill <- dplyr::inner_join(pathways, skills, by = c("year", "id"))

heatmap_data <- pathway_skill |>
  dplyr::count(pathway, skill, name = "n")

heatmap_data <- heatmap_data |>
  dplyr::group_by(pathway) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

gg_heat <- ggplot2::ggplot(
  heatmap_data,
  ggplot2::aes(x = skill, y = pathway, fill = prop)
) +

  ggplot2::geom_tile(color = "white") +

  ggplot2::geom_text(
    ggplot2::aes(label = scales::percent(prop, accuracy = 1)),
    size = 3
  ) +

  ggplot2::scale_fill_gradient(
    low = "white",
    high = "steelblue",
    labels = scales::percent_format()
  ) +

  ggplot2::labs(
    x = "Skills",
    y = "Learning Pathway",
    fill = "Proportion",
    title = "How RSEs Learn Different Skills"
  ) +

  ggthemes::theme_economist() +

  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

gg_heat

top_data <- heatmap_data |>
  dplyr::filter(prop > 0.1)

ggplot2::ggplot(
  top_data,
  ggplot2::aes(x = skill, y = pathway, size = prop)
) +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Strongest Pathway–Skill Relationships",
    x = "Skill",
    y = "Pathway"
  ) +
  ggthemes::theme_economist()