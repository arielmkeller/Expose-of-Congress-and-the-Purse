library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

get_openfec_summary <- function(legislator, cycle = 2024, chamber = "House") {
  api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
  if (identical(api_key, "")) {
    return(tibble(
      source = c("Individuals", "PACs"),
      amount_usd = c(1200000, 550000),
      note = "Set OPENFEC_API_KEY to fetch live data."
    ))
  }

  office <- ifelse(tolower(chamber) == "senate", "S", "H")
  req <- request("https://api.open.fec.gov/v1/candidates/search/") |>
    req_url_query(
      q = legislator,
      api_key = api_key,
      office = office,
      election_year = cycle,
      per_page = 1
    )

  candidate_resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(candidate_resp)) {
    return(tibble(
      source = "API Error",
      amount_usd = NA_real_,
      note = "OpenFEC request failed."
    ))
  }

  candidate_json <- resp_body_json(candidate_resp, simplifyVector = TRUE)
  if (length(candidate_json$results) == 0) {
    return(tibble(
      source = "No Match",
      amount_usd = NA_real_,
      note = "No candidate found for that query."
    ))
  }

  candidate_id <- candidate_json$results$candidate_id[[1]]
  totals_req <- request("https://api.open.fec.gov/v1/candidate/totals/") |>
    req_url_query(
      candidate_id = candidate_id,
      cycle = cycle,
      api_key = api_key,
      per_page = 1
    )

  totals_resp <- tryCatch(req_perform(totals_req), error = function(e) NULL)
  if (is.null(totals_resp)) {
    return(tibble(
      source = "API Error",
      amount_usd = NA_real_,
      note = "OpenFEC totals request failed."
    ))
  }

  totals_json <- resp_body_json(totals_resp, simplifyVector = TRUE)
  if (length(totals_json$results) == 0) {
    return(tibble(
      source = "No Totals",
      amount_usd = NA_real_,
      note = "No totals returned for candidate."
    ))
  }

  res <- totals_json$results[1, ]
  tibble(
    source = c("Individuals", "PACs", "Total receipts"),
    amount_usd = c(
      as.numeric(res$receipts_from_individuals),
      as.numeric(res$receipts_from_pacs),
      as.numeric(res$receipts)
    ),
    note = paste("Candidate ID:", candidate_id)
  )
}
