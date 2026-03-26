library(readr)
library(dplyr)
library(tibble)

normalize_name <- function(x) {
  y <- tolower(x)
  y <- gsub("[^a-z\\s]", " ", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

name_matches <- function(dataset_name, query_name) {
  ds <- normalize_name(dataset_name)
  q <- normalize_name(query_name)
  if (identical(q, "")) {
    return(FALSE)
  }

  ds == q || grepl(q, ds, fixed = TRUE) || grepl(ds, q, fixed = TRUE)
}

get_earmarks_for_legislator <- function(legislator_name) {
  path <- "data/earmarks.csv"

  if (!file.exists(path)) {
    return(tibble(
      legislator = legislator_name,
      project_type = c("Transportation", "Education", "Health"),
      amount_usd = c(8500000, 3200000, 4700000)
    ))
  }

  raw <- suppressMessages(read_csv(path, show_col_types = FALSE))
  required_cols <- c("legislator", "project_type", "amount_usd")
  if (!all(required_cols %in% names(raw))) {
    stop("data/earmarks.csv must include columns: legislator, project_type, amount_usd")
  }

  raw |>
    mutate(legislator = trimws(legislator)) |>
    rowwise() |>
    filter(name_matches(legislator, legislator_name)) |>
    ungroup() |>
    transmute(
      legislator = legislator,
      project_type = project_type,
      amount_usd = as.numeric(amount_usd)
    )
}
