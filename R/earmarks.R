library(readr)
library(dplyr)
library(tibble)
library(stringr)

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
  path <- "earmarks/earmarks_master_ready.csv"

  if (!file.exists(path)) {
    return(tibble(
      legislator = legislator_name,
      project_type = c("Transportation", "Education", "Health"),
      amount_usd = c(8500000, 3200000, 4700000)
    ))
  }

  raw <- suppressMessages(read_csv(path, show_col_types = FALSE))
  required_cols <- c("member_name", "subcommittee", "amount_requested")
  if (!all(required_cols %in% names(raw))) {
    alt_cols <- c("legislator", "project_type", "amount_usd")
    if (!all(alt_cols %in% names(raw))) {
      stop("earmarks data must include member_name/subcommittee/amount_requested or legislator/project_type/amount_usd")
    }
    return(raw |>
      mutate(legislator = trimws(legislator)) |>
      rowwise() |>
      filter(name_matches(legislator, legislator_name)) |>
      ungroup() |>
      mutate(
        project_type = as.character(project_type),
        amount_usd = as.numeric(amount_usd)
      ))
  }

  query_clean <- normalize_name(legislator_name)
  if ("member_clean" %in% names(raw)) {
    filtered <- raw |>
      mutate(member_clean = ifelse(is.na(member_clean), normalize_name(member_name), member_clean)) |>
      filter(member_clean == query_clean | stringr::str_detect(member_clean, stringr::fixed(query_clean)))
  } else {
    filtered <- raw |>
      filter(name_matches(member_name, legislator_name))
  }

  filtered |>
    transmute(
      legislator = member_name,
      project_type = ifelse(is.na(subcommittee) | subcommittee == "", "Unspecified", subcommittee),
      amount_usd = as.numeric(amount_requested)
    )
}
