library(httr2)
library(dplyr)
library(tibble)

get_usaspending_context <- function(earmarks_df) {
  # Fetch all awarding agencies for the time period, regardless of earmarks.
  base_body <- list(
    filters = list(
      time_period = list(list(start_date = "2024-01-01", end_date = "2024-12-31"))
    ),
    category = "awarding_agency",
    limit = 100,
    subawards = FALSE
  )

  live_resp <- NULL
  live_payloads <- list()
  page <- 1
  max_pages <- 50
  while (page <= max_pages) {
    body <- base_body
    body$page <- page

    req <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
      req_method("POST") |>
      req_headers(`Content-Type` = "application/json") |>
      req_body_json(body, auto_unbox = TRUE)

    live_resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(live_resp, "error") || resp_status(live_resp) >= 400) {
      break
    }

    payload <- tryCatch(
      resp_body_json(live_resp, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(payload$results) || length(payload$results) == 0) {
      break
    }

    live_payloads[[length(live_payloads) + 1]] <- payload$results
    page <- page + 1
  }

  if (length(live_payloads) > 0) {
    live <- bind_rows(lapply(live_payloads, as_tibble)) |>
      transmute(
        agency = as.character(name),
        amount_usd = as.numeric(amount),
        note = "Live USAspending result for all awarding agencies."
      ) |>
      distinct(agency, .keep_all = TRUE) |>
      arrange(desc(amount_usd))
    if (nrow(live) > 0) {
      live$note <- ifelse(seq_len(nrow(live)) == 1, live$note, "")
      return(live)
    }
  }

  api_detail <- NULL
  if (inherits(live_resp, "error")) {
    api_detail <- live_resp$message
  } else if (!is.null(live_resp)) {
    status <- resp_status(live_resp)
    body_text <- tryCatch(resp_body_string(live_resp), error = function(e) "")
    if (nchar(body_text) > 160) {
      body_text <- paste0(substr(body_text, 1, 160), "…")
    }
    api_detail <- paste0("HTTP ", status, ifelse(nchar(body_text) > 0, paste0(": ", body_text), ""))
  }

  fallback_note <- "USAspending API unavailable."
  if (!is.null(api_detail) && nchar(api_detail) > 0) {
    fallback_note <- paste0(fallback_note, " ", api_detail)
  }

  fallback <- tibble(
    agency = "USAspending API unavailable",
    amount_usd = NA_real_,
    note = fallback_note
  )
  fallback
}
