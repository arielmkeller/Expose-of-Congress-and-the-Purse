library(httr2)
library(dplyr)
library(tibble)
library(readr)

get_project_root <- function() {
  src <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(src) && nzchar(src)) {
    return(normalizePath(file.path(dirname(src), ".."), winslash = "/"))
  }
  getwd()
}

legislators_cache_path <- file.path(get_project_root(), "data", "legislators_cache.csv")

get_legislators_reference <- function() {
  req <- request("https://www.govtrack.us/api/v2/role") |>
    req_url_query(current = "true", limit = 600)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) >= 400) {
    return(load_legislators_cache_or_fallback())
  }

  payload <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload) || is.null(payload$objects) || length(payload$objects) == 0) {
    return(load_legislators_cache_or_fallback())
  }

  rows <- lapply(payload$objects, function(x) {
    district_val <- NA_character_
    if (!is.null(x$district) && !is.na(x$district)) {
      district_val <- sprintf("%02d", as.integer(x$district))
    }
    list(
      legislator = as.character(x$person$name),
      party = as.character(x$party),
      state = as.character(x$state),
      chamber = ifelse(as.character(x$role_type) == "senator", "Senate", "House"),
      district = district_val
    )
  })

  out <- bind_rows(rows) |>
    filter(!is.na(legislator), legislator != "") |>
    distinct(legislator, .keep_all = TRUE) |>
    arrange(legislator)

  if (nrow(out) == 0) {
    return(load_legislators_cache_or_fallback())
  }

  dir.create(dirname(legislators_cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(write_csv(out, legislators_cache_path), error = function(e) NULL)
  out
}

load_legislators_cache_or_fallback <- function() {
  if (file.exists(legislators_cache_path)) {
    cached <- tryCatch(read_csv(legislators_cache_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(cached) && nrow(cached) > 0) {
      return(cached)
    }
  }
  fallback_legislators_reference()
}

fallback_legislators_reference <- function() {
  tibble(
    legislator = c("Alexandria Ocasio-Cortez", "Chuck Schumer", "Ted Cruz", "Cory Booker"),
    party = c("Democrat", "Democrat", "Republican", "Democrat"),
    state = c("NY", "NY", "TX", "NJ"),
    chamber = c("House", "Senate", "Senate", "Senate"),
    district = c("14", NA_character_, NA_character_, NA_character_)
  )
}
