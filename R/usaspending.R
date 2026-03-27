library(httr2)
library(dplyr)
library(tibble)

get_usaspending_context <- function(earmarks_df) {
  if (nrow(earmarks_df) == 0) {
    return(tibble(
      agency = character(),
      amount_usd = numeric(),
      note = character()
    ))
  }

  top_types <- earmarks_df |>
    group_by(project_type) |>
    summarize(amount_usd = sum(amount_usd, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(amount_usd)) |>
    head(3)

  # Use top earmark categories as a lightweight keyword query against USAspending.
  query_text <- paste(unique(top_types$project_type), collapse = " ")
  body <- list(
    filters = list(
      time_period = list(list(start_date = "2024-01-01", end_date = "2024-12-31"))
    ),
    category = "awarding_agency",
    limit = 5,
    page = 1,
    subawards = FALSE,
    keyword = query_text
  )

  req <- request("https://api.usaspending.gov/api/v2/search/spending_by_category") |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(body, auto_unbox = TRUE)

  live_resp <- tryCatch(req_perform(req), error = function(e) e)
  if (!inherits(live_resp, "error") && resp_status(live_resp) < 400) {
    payload <- tryCatch(
      resp_body_json(live_resp, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.null(payload$results) && length(payload$results) > 0) {
      live <- as_tibble(payload$results) |>
        transmute(
          agency = as.character(name),
          amount_usd = as.numeric(amount),
          note = "Live USAspending result based on earmark category keywords."
        )
      if (nrow(live) > 0) {
        live$note <- ifelse(seq_len(nrow(live)) == 1, live$note, "")
        return(live)
      }
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

  fallback_note <- "Fallback estimate. USAspending API unavailable."
  if (!is.null(api_detail) && nchar(api_detail) > 0) {
    fallback_note <- paste0(fallback_note, " ", api_detail)
  }

  fallback <- tibble(
    agency = c("Department of Transportation", "Department of Education", "HHS")[seq_len(nrow(top_types))],
    amount_usd = top_types$amount_usd,
    note = fallback_note
  )
  fallback$note <- ifelse(seq_len(nrow(fallback)) == 1, fallback$note, "")
  fallback
}
