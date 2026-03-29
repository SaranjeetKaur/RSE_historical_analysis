data <- readr::read_csv(
  "https://raw.githubusercontent.com/softwaresaved/RSE_survey_longitudinal/main/2022/2022_cols.csv",
  show_col_types = FALSE
)

que_code <- data |>
  tidyr::separate(
    Old_name,
    into = c("code", "question"),
    sep = "\\.\\s*"
  ) |>
  dplyr::select("code", "question") |>
  dplyr::mutate(
    question = stringr::str_remove(question, "\\s*\\[.*\\]$")
  ) |>
  dplyr::distinct(code, .keep_all = TRUE)

code_break <- que_code |>
  tidyr::separate(
  code,
  into = c("base_code", "sub_other"),
  sep = "(?=\\[)",
  remove = TRUE,
  extra = "merge",
  fill = "right"
) |>
  tidyr::extract(
    col = base_code,
    into = c("code", "country"),
    regex = "^(.*?\\d+)(.*)$",
    remove = TRUE
  ) |>
  tidyr::extract(
    col = code,
    into = c("code", "que_num"),
    regex = "^([A-Za-z]+)(\\d+)$",
    remove = TRUE
  )
readr::write_csv(code_break, "data/code_break.csv")

user <- "softwaresaved"
repo <- "RSE_survey_longitudinal"
path <- ""
branch <- "main"
url <- paste0("https://api.github.com/repos/", user, "/", repo, "/git/trees/", branch, "?recursive=1")
res <- httr::GET(url)
tree <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
files <- tree$tree$path
folders <- c("2022", "2018", "2017", "2016")

result <- code_break |>
  dplyr::rowwise() |>
  dplyr::mutate(
    base_name = paste0(
      paste(na.omit(c(code, que_num, country, sub_other)), collapse = "")
    ),
    filename = list(paste0(folders, "_", base_name))
  ) |>
  dplyr::ungroup() |>
  tidyr::unnest(filename) |>
  dplyr::select(filename, question)

full_filename_check <- result |>
  dplyr::mutate(
    folder = stringr::str_extract(filename, "^[^_]+"),
    match_info = purrr::map(filename, function(fname){
      folder <- stringr::str_extract(fname, "^[^_]+")
      folder_files <- stringr::str_subset(files, paste0("^", folder, "/"))
      matches <- folder_files[
        stringr::str_detect(
          base::basename(folder_files),
          paste0("^", fname)
        )
      ]
      if(length(matches) > 0){
        list(
          status = "found",
          matched_file = matches[1]
        )
      } else {
        list(
          status = "missing",
          matched_file = NA_character_
        )
      }
    }) 
  ) |>
  tidyr::unnest_wider(match_info) |>
  dplyr::filter(status == "found") |>
  dplyr::select(folder, filename, matched_file, status, question)

readr::write_csv(full_filename_check, "data/full_filename_check.csv")
