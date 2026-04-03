library(readr)
library(readxl)
library(dplyr)
library(tibble)
library(stringr)

normalize_name <- function(x) {
  y <- tolower(x)
  y <- gsub("[^a-z\\s]", " ", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

earmarks_cache <- new.env(parent = emptyenv())

load_earmarks_data <- function() {
  path <- "fy2_All_Earmarks.xlsx"
  if (exists("get_project_root", mode = "function")) {
    root_path <- file.path(get_project_root(), "fy2_All_Earmarks.xlsx")
    if (file.exists(root_path)) {
      path <- root_path
    }
  }
  if (!file.exists(path)) {
    alt_path <- file.path(getwd(), "fy2_All_Earmarks.xlsx")
    if (file.exists(alt_path)) {
      path <- alt_path
    } else {
      return(tibble())
    }
  }

  mtime <- file.info(path)$mtime
  if (exists("data", earmarks_cache, inherits = FALSE) &&
      exists("mtime", earmarks_cache, inherits = FALSE) &&
      identical(earmarks_cache$mtime, mtime)) {
    return(earmarks_cache$data)
  }

  raw <- read_excel(path, sheet = "Combined bills")
  raw <- raw |>
    mutate(across(where(is.character), ~ trimws(.)))

  earmarks_cache$data <- raw
  earmarks_cache$mtime <- mtime
  raw
}

extract_last_name_tokens <- function(request_str) {
  if (is.null(request_str) || is.na(request_str) || request_str == "") {
    return(character(0))
  }
  cleaned <- tolower(request_str)
  cleaned <- gsub("\\(.*?\\)", " ", cleaned, perl = TRUE)
  cleaned <- gsub("\\[.*?\\]", " ", cleaned, perl = TRUE)
  cleaned <- gsub("\\s+and\\s+", ",", cleaned, perl = TRUE)
  parts <- unlist(strsplit(cleaned, ","))
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  if (length(parts) == 0) {
    return(character(0))
  }
  out <- character(0)
  for (part in parts) {
    token <- gsub("[^a-z\\s-]", " ", part)
    token <- gsub("\\s+", " ", token)
    token <- trimws(token)
    if (token == "") next
    tokens <- unlist(strsplit(token, " "))
    tokens <- tokens[tokens != ""]
    if (length(tokens) == 0) next
    out <- c(out, tokens[[length(tokens)]])
  }
  out[out != ""]
}

get_earmark_totals_by_lastname <- function() {
  raw <- load_earmarks_data()
  if (nrow(raw) == 0) {
    return(tibble(last_name = character(), total_amount = numeric()))
  }

  amounts <- raw$`Total Amount`
  house_names <- raw$`House Requesting Member(s)`
  senate_names <- raw$`Senate Requesting Member(s)`

  collect <- function(names_vec) {
    last_names <- character(0)
    last_amounts <- numeric(0)
    for (i in seq_along(names_vec)) {
      tokens <- extract_last_name_tokens(names_vec[[i]])
      if (length(tokens) == 0) next
      last_names <- c(last_names, tokens)
      last_amounts <- c(last_amounts, rep(amounts[[i]], length(tokens)))
    }
    tibble(last_name = last_names, amount = last_amounts)
  }

  house_tbl <- collect(house_names)
  senate_tbl <- collect(senate_names)
  combined <- bind_rows(house_tbl, senate_tbl)
  if (nrow(combined) == 0) {
    return(tibble(last_name = character(), total_amount = numeric()))
  }

  combined |>
    group_by(last_name) |>
    summarize(total_amount = sum(amount, na.rm = TRUE), .groups = "drop")
}

build_name_candidates <- function(legislator_name) {
  if (is.null(legislator_name) || is.na(legislator_name) || legislator_name == "") {
    return(character(0))
  }
  name_clean <- tolower(legislator_name)
  name_clean <- gsub("\\b(rep\\.?|representative|sen\\.?|senator|delegate|del\\.?|hon\\.?|mr\\.?|mrs\\.?|ms\\.?|commish\\.?|commissioner)\\b", "", name_clean)
  name_clean <- gsub("\\[.*?\\]", " ", name_clean, perl = TRUE)
  name_clean <- gsub("\\s*\\(.*?\\)\\s*", " ", name_clean)
  name_clean <- gsub("[^a-z\\s-]", " ", name_clean)
  name_clean <- gsub("\\s+", " ", name_clean)
  name_clean <- trimws(name_clean)
  if (name_clean == "") {
    return(character(0))
  }
  tokens <- unlist(strsplit(name_clean, " "))
  tokens <- tokens[tokens != ""]
  if (length(tokens) > 0) {
    drop_tokens <- c("d", "r", "i", "ind", tolower(state.abb), "dc")
    tokens <- tokens[!tokens %in% drop_tokens]
  }
  if (length(tokens) == 0) {
    return(character(0))
  }

  candidates <- character(0)
  if (length(tokens) >= 1) {
    candidates <- c(candidates, tokens[[length(tokens)]])
  }
  if (length(tokens) >= 2) {
    candidates <- c(candidates, paste(tokens[(length(tokens) - 1):length(tokens)], collapse = " "))
  }
  if (length(tokens) >= 3) {
    candidates <- c(candidates, paste(tokens[(length(tokens) - 2):length(tokens)], collapse = " "))
  }
  unique(candidates)
}

match_requesting_member <- function(request_str, candidates) {
  if (is.null(request_str) || is.na(request_str) || request_str == "") {
    return(FALSE)
  }
  if (length(candidates) == 0) {
    return(FALSE)
  }
  cleaned <- tolower(request_str)
  cleaned <- gsub("\\s*\\(.*?\\)\\s*", " ", cleaned)
  cleaned <- gsub("[^a-z\\s,]", " ", cleaned)
  cleaned <- gsub("\\s+", " ", cleaned)
  cleaned <- trimws(cleaned)

  for (cand in candidates) {
    if (cand == "") next
    pattern <- paste0("\\b", cand, "\\b")
    if (stringr::str_detect(cleaned, pattern)) {
      return(TRUE)
    }
  }
  FALSE
}

get_earmarks_for_legislator <- function(legislator_name, chamber = NA_character_) {
  raw <- load_earmarks_data()
  if (nrow(raw) == 0) {
    return(tibble())
  }

  candidates <- build_name_candidates(legislator_name)
  if (length(candidates) == 0) {
    return(tibble())
  }

  house_col <- "House Requesting Member(s)"
  senate_col <- "Senate Requesting Member(s)"
  if (!house_col %in% names(raw) || !senate_col %in% names(raw)) {
    return(tibble())
  }

  chamber_clean <- tolower(as.character(chamber))
  use_house <- chamber_clean == "house"
  use_senate <- chamber_clean == "senate"

  flagged <- raw |>
    mutate(
      match_house = if (use_house || !use_senate) vapply(.data[[house_col]], match_requesting_member, logical(1), candidates = candidates) else FALSE,
      match_senate = if (use_senate || !use_house) vapply(.data[[senate_col]], match_requesting_member, logical(1), candidates = candidates) else FALSE
    ) |>
    filter(match_house | match_senate) |>
    mutate(
      requesting_members = ifelse(match_house, .data[[house_col]], .data[[senate_col]]),
      requesting_chamber = ifelse(match_house, "House", "Senate")
    )

  if (nrow(flagged) == 0) {
    return(tibble())
  }

  flagged |>
    transmute(
      Chamber = requesting_chamber,
      Requesting_Members = requesting_members,
      Project = .data$Project,
      Recipient = .data$Recipient,
      Location = .data$Location,
      Agency = .data$Agency,
      Account = .data$Account,
      Bill = .data$Bill,
      Total_Amount = .data$`Total Amount`
    )
}
