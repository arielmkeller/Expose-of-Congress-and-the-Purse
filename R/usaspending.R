library(httr2)
library(dplyr)
library(tibble)

get_usaspending_context <- function(state = NULL, district = NULL) {
  current_year <- format(Sys.Date(), "%Y")
  start_date <- paste0(current_year, "-01-01")
  end_date <- paste0(current_year, "-12-31")

  location_filters <- list()
  if (!is.null(state) && !is.na(state) && nzchar(state)) {
    location <- list(country = "USA", state = toupper(state))
    location_filters <- list(recipient_locations = list(location))
  }

  geo_scope <- NULL
  if (!is.null(state) && !is.na(state) && nzchar(state)) {
    geo_scope <- paste0("State ", toupper(state))
  }

  build_body <- function(category, include_geo = TRUE, include_district = TRUE) {
    filters <- list(time_period = list(list(start_date = start_date, end_date = end_date)))
    if (include_geo && length(location_filters) > 0) {
      filters <- c(filters, location_filters)
    }
    list(
      category = category,
      filters = filters,
      limit = 10,
      page = 1,
      subawards = FALSE
    )
  }

  body_agencies <- build_body("awarding_agency")
  body_recipients <- build_body("recipient")
  body_districts <- build_body("district")
  if (length(location_filters) > 0) {
    body_districts$filters <- c(body_districts$filters, list(recipient_locations = location_filters$recipient_locations))
  }
  body_counties <- build_body("county")

  req_agencies <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(body_agencies, auto_unbox = TRUE)

  req_recipients <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(body_recipients, auto_unbox = TRUE)

  req_districts <- request("https://api.usaspending.gov/api/v2/search/spending_by_category/district/") |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(body_districts, auto_unbox = TRUE)

  req_counties <- request("https://api.usaspending.gov/api/v2/search/spending_by_category/county/") |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(body_counties, auto_unbox = TRUE)

  live_agencies_resp <- tryCatch(req_perform(req_agencies), error = function(e) e)
  live_recipients_resp <- tryCatch(req_perform(req_recipients), error = function(e) e)
  live_districts_resp <- tryCatch(req_perform(req_districts), error = function(e) e)
  live_counties_resp <- tryCatch(req_perform(req_counties), error = function(e) e)

  geo_failed_agencies <- FALSE
  geo_failed_recipients <- FALSE
  geo_failed_districts <- FALSE
  geo_failed_counties <- FALSE

  if (inherits(live_agencies_resp, "error") || (!is.null(live_agencies_resp) && resp_status(live_agencies_resp) >= 400)) {
    geo_failed_agencies <- TRUE
    body_agencies <- build_body("awarding_agency", include_geo = TRUE, include_district = FALSE)
    req_agencies <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
      req_method("POST") |>
      req_headers(`Content-Type` = "application/json") |>
      req_body_json(body_agencies, auto_unbox = TRUE)
    live_agencies_resp <- tryCatch(req_perform(req_agencies), error = function(e) e)
    if (inherits(live_agencies_resp, "error") || (!is.null(live_agencies_resp) && resp_status(live_agencies_resp) >= 400)) {
      body_agencies <- build_body("awarding_agency", include_geo = FALSE)
      req_agencies <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(body_agencies, auto_unbox = TRUE)
      live_agencies_resp <- tryCatch(req_perform(req_agencies), error = function(e) e)
    }
  }

  if (inherits(live_recipients_resp, "error") || (!is.null(live_recipients_resp) && resp_status(live_recipients_resp) >= 400)) {
    geo_failed_recipients <- TRUE
    body_recipients <- build_body("recipient", include_geo = TRUE, include_district = FALSE)
    req_recipients <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
      req_method("POST") |>
      req_headers(`Content-Type` = "application/json") |>
      req_body_json(body_recipients, auto_unbox = TRUE)
    live_recipients_resp <- tryCatch(req_perform(req_recipients), error = function(e) e)
    if (inherits(live_recipients_resp, "error") || (!is.null(live_recipients_resp) && resp_status(live_recipients_resp) >= 400)) {
      body_recipients <- build_body("recipient", include_geo = FALSE)
      req_recipients <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(body_recipients, auto_unbox = TRUE)
      live_recipients_resp <- tryCatch(req_perform(req_recipients), error = function(e) e)
    }
  }

  if (inherits(live_districts_resp, "error") || (!is.null(live_districts_resp) && resp_status(live_districts_resp) >= 400)) {
    geo_failed_districts <- TRUE
    body_districts <- build_body("district", include_geo = TRUE, include_district = FALSE)
    req_districts <- request("https://api.usaspending.gov/api/v2/search/spending_by_category/district/") |>
      req_method("POST") |>
      req_headers(`Content-Type` = "application/json") |>
      req_body_json(body_districts, auto_unbox = TRUE)
    live_districts_resp <- tryCatch(req_perform(req_districts), error = function(e) e)
    if (inherits(live_districts_resp, "error") || (!is.null(live_districts_resp) && resp_status(live_districts_resp) >= 400)) {
      body_districts <- build_body("district", include_geo = FALSE)
      req_districts <- request("https://api.usaspending.gov/api/v2/search/spending_by_category/district/") |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(body_districts, auto_unbox = TRUE)
      live_districts_resp <- tryCatch(req_perform(req_districts), error = function(e) e)
    }
  }

  if (inherits(live_counties_resp, "error") || (!is.null(live_counties_resp) && resp_status(live_counties_resp) >= 400)) {
    geo_failed_counties <- TRUE
    body_counties <- build_body("county", include_geo = TRUE, include_district = FALSE)
    req_counties <- request("https://api.usaspending.gov/api/v2/search/spending_by_category/county/") |>
      req_method("POST") |>
      req_headers(`Content-Type` = "application/json") |>
      req_body_json(body_counties, auto_unbox = TRUE)
    live_counties_resp <- tryCatch(req_perform(req_counties), error = function(e) e)
    if (inherits(live_counties_resp, "error") || (!is.null(live_counties_resp) && resp_status(live_counties_resp) >= 400)) {
      body_counties <- build_body("county", include_geo = FALSE)
      req_counties <- request("https://api.usaspending.gov/api/v2/search/spending_by_category/county/") |>
        req_method("POST") |>
        req_headers(`Content-Type` = "application/json") |>
        req_body_json(body_counties, auto_unbox = TRUE)
      live_counties_resp <- tryCatch(req_perform(req_counties), error = function(e) e)
    }
  }

  parse_live <- function(resp, field_name, note_text) {
    if (inherits(resp, "error") || is.null(resp) || resp_status(resp) >= 400) {
      return(NULL)
    }
    payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(payload$results) || length(payload$results) == 0) {
      return(NULL)
    }
    out <- as_tibble(payload$results) |>
      transmute(
        name = as.character(name),
        amount_usd = as.numeric(amount),
        note = note_text
      )
    names(out)[[1]] <- field_name
    out
  }

  note_suffix <- if (!is.null(geo_scope)) paste0(" (", geo_scope, ")") else ""
  if (geo_failed_agencies || geo_failed_recipients || geo_failed_districts || geo_failed_counties) {
    note_suffix <- paste0(note_suffix, " (geo filter failed; showing national)")
  }
  live_agencies <- parse_live(
    live_agencies_resp,
    "agency",
    paste0("Live USAspending result", note_suffix, ".")
  )
  live_recipients <- parse_live(
    live_recipients_resp,
    "recipient",
    paste0("Live USAspending result", note_suffix, ".")
  )
  live_districts <- parse_live(
    live_districts_resp,
    "district",
    paste0("Live USAspending result", note_suffix, ".")
  )
  live_counties <- parse_live(
    live_counties_resp,
    "county",
    paste0("Live USAspending result", note_suffix, ".")
  )

  api_detail <- function(resp) {
    if (inherits(resp, "error")) {
      return(resp$message)
    }
    if (is.null(resp)) {
      return(NULL)
    }
    status <- resp_status(resp)
    if (status < 400) {
      return(NULL)
    }
    body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
    if (nchar(body_text) > 160) {
      body_text <- paste0(substr(body_text, 1, 160), "…")
    }
    paste0("HTTP ", status, ifelse(nchar(body_text) > 0, paste0(": ", body_text), ""))
  }

  fallback_note <- "Fallback estimate. USAspending API unavailable."
  detail_agencies <- api_detail(live_agencies_resp)
  if (!is.null(detail_agencies) && nchar(detail_agencies) > 0) {
    fallback_note <- paste0(fallback_note, " ", detail_agencies)
  }

  fallback_agencies <- tibble(
    agency = c("Department of Transportation", "Department of Education", "HHS"),
    amount_usd = c(NA_real_, NA_real_, NA_real_),
    note = fallback_note
  )
  fallback_agencies$note <- ifelse(seq_len(nrow(fallback_agencies)) == 1, fallback_agencies$note, "")

  detail_recipients <- api_detail(live_recipients_resp)
  fallback_note_recipients <- "Fallback estimate. USAspending API unavailable."
  if (!is.null(detail_recipients) && nchar(detail_recipients) > 0) {
    fallback_note_recipients <- paste0(fallback_note_recipients, " ", detail_recipients)
  }

  fallback_recipients <- tibble(
    recipient = c("Local governments", "Hospitals", "Universities"),
    amount_usd = c(NA_real_, NA_real_, NA_real_),
    note = fallback_note_recipients
  )
  fallback_recipients$note <- ifelse(seq_len(nrow(fallback_recipients)) == 1, fallback_recipients$note, "")

  fallback_districts <- tibble(
    district = c("District 01", "District 02", "District 03"),
    amount_usd = c(NA_real_, NA_real_, NA_real_),
    note = fallback_note_recipients
  )
  fallback_districts$note <- ifelse(seq_len(nrow(fallback_districts)) == 1, fallback_districts$note, "")

  fallback_counties <- tibble(
    county = c("County A", "County B", "County C"),
    amount_usd = c(NA_real_, NA_real_, NA_real_),
    note = fallback_note_recipients
  )
  fallback_counties$note <- ifelse(seq_len(nrow(fallback_counties)) == 1, fallback_counties$note, "")

  agencies_out <- if (!is.null(live_agencies) && nrow(live_agencies) > 0) {
    live_agencies$note <- ifelse(seq_len(nrow(live_agencies)) == 1, live_agencies$note, "")
    live_agencies
  } else {
    fallback_agencies
  }

  recipients_out <- if (!is.null(live_recipients) && nrow(live_recipients) > 0) {
    live_recipients$note <- ifelse(seq_len(nrow(live_recipients)) == 1, live_recipients$note, "")
    live_recipients
  } else {
    fallback_recipients
  }

  if (!is.null(live_districts) && nrow(live_districts) > 0) {
    if (!is.null(state) && !is.na(state) && nzchar(state) && "district" %in% names(live_districts)) {
      state_prefix <- paste0(toupper(state), "-")
      live_districts <- live_districts |>
        filter(grepl(state_prefix, district, fixed = TRUE))
      if (nrow(live_districts) == 0) {
        live_districts <- NULL
      }
    }
  }

  districts_out <- if (!is.null(live_districts) && nrow(live_districts) > 0) {
    live_districts$note <- ifelse(seq_len(nrow(live_districts)) == 1, live_districts$note, "")
    live_districts
  } else {
    fallback_districts
  }

  counties_out <- if (!is.null(live_counties) && nrow(live_counties) > 0) {
    live_counties$note <- ifelse(seq_len(nrow(live_counties)) == 1, live_counties$note, "")
    live_counties
  } else {
    fallback_counties
  }


  total_amount <- sum(agencies_out$amount_usd, na.rm = TRUE)
  if (is.na(total_amount) || total_amount == 0) {
    total_amount <- sum(recipients_out$amount_usd, na.rm = TRUE)
  }

  list(
    agencies = agencies_out,
    recipients = recipients_out,
    districts = districts_out,
    counties = counties_out,
    total_amount = total_amount,
    diagnostics = list(
      state = state,
      district = district,
      geo_failed_agencies = geo_failed_agencies,
      geo_failed_recipients = geo_failed_recipients,
      geo_failed_districts = geo_failed_districts,
      geo_failed_counties = geo_failed_counties,
      status_agencies = if (inherits(live_agencies_resp, "error")) "error" else resp_status(live_agencies_resp),
      status_recipients = if (inherits(live_recipients_resp, "error")) "error" else resp_status(live_recipients_resp),
      status_districts = if (inherits(live_districts_resp, "error")) "error" else resp_status(live_districts_resp),
      status_counties = if (inherits(live_counties_resp, "error")) "error" else resp_status(live_counties_resp),
      body_agencies = body_agencies,
      body_recipients = body_recipients
    )
  )
}
