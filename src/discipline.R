currentEmp13_df <- full_filename_check |>
  dplyr::filter(
    status == "found",
    stringr::str_detect(filename, "^\\d{4}_currentEmp13")
  )

currentEmp13_df <- currentEmp13_df |>
  dplyr::mutate(
    raw_url = paste0(
      "https://raw.githubusercontent.com/",
      user, "/", repo, "/", branch, "/", matched_file
    )
  )

currentEmp13_data <- currentEmp13_df |>
  dplyr::mutate(
    data = purrr::map(raw_url, ~ tryCatch(
      readr::read_csv(.x, show_col_types = FALSE),
      error = function(e) NULL
    ))
  ) |>
  dplyr::filter(!purrr::map_lgl(data, is.null))

combined_currentEmp13 <- currentEmp13_data |>
  dplyr::mutate(year = folder) |>
  tidyr::unnest(data)

disc_df <- combined_currentEmp13 |>
  dplyr::filter(!is.na(currentEmp13)) |>
  dplyr::mutate(year = as.numeric(year))

create_wordcloud_df <- function(data, yr) {
  data |>
    dplyr::filter(year == yr) |>
    dplyr::count(currentEmp13, sort = TRUE) |>
    dplyr::rename(word = currentEmp13, freq = n)
}

wc_2022 <- create_wordcloud_df(disc_df, 2022)

plot_wordcloud <- function(df, yr) {
  n_words <- nrow(df)
  palette <- RColorBrewer::brewer.pal(min(n_words, 12), "Set3")
  df <- df |> dplyr::mutate(color = rep(palette, length.out = n_words))
  
  ggplot2::ggplot(df, ggplot2::aes(label = word, size = freq, color = color)) +
    ggwordcloud::geom_text_wordcloud() +
    ggplot2::scale_size_area(max_size = 15) +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = paste("Disciplines -", yr)) +
    ggplot2::theme_minimal()
}

wc_2022_plot <- plot_wordcloud(wc_2022, 2022)
wc_2022_plot

ggplot2::ggsave("fig/discipline.pdf", wc_2022_plot,
                width = 11, height = 5, units = "in")

ggplot2::ggsave("fig/discipline.png", wc_2022_plot,
                width = 11, height = 5, units = "in", dpi = 300)
