library(httr2)
library(tibble)
library(dplyr)

get_congress_member_profile <- function(bioguide_id) {
  api_key <- Sys.getenv("CONGRESS_API_KEY", unset = "")
  if (is.null(bioguide_id) || is.na(bioguide_id) || bioguide_id == "") {
    return(list(
      ok = FALSE,
      note = "Bioguide ID unavailable for this member.",
      profile = tibble(field = character(), value = character())
    ))
  }
  if (identical(api_key, "")) {
    return(list(
      ok = FALSE,
      note = "Set CONGRESS_API_KEY to fetch member details.",
      profile = tibble(field = character(), value = character())
    ))
  }

  req <- request(paste0("https://api.congress.gov/v3/member/", bioguide_id)) |>
    req_url_query(api_key = api_key, format = "json") |>
    req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error") || resp_status(resp) >= 400) {
    return(list(
      ok = FALSE,
      note = "Congress API request failed.",
      profile = tibble(field = character(), value = character())
    ))
  }

  payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(payload)) {
    return(list(
      ok = FALSE,
      note = "Congress API response unavailable.",
      profile = tibble(field = character(), value = character())
    ))
  }

  member <- NULL
  if (!is.null(payload$member)) {
    member <- payload$member
  } else if (!is.null(payload$members) && length(payload$members) > 0) {
    member <- payload$members[[1]]
  }
  if (is.null(member)) {
    return(list(
      ok = FALSE,
      note = "Member details not returned by Congress API.",
      profile = tibble(field = character(), value = character())
    ))
  }

  birth_year <- suppressWarnings(as.integer(member$birthYear))
  age <- if (!is.na(birth_year)) as.integer(format(Sys.Date(), "%Y")) - birth_year else NA_integer_
  age_label <- if (!is.na(age)) as.character(age) else "Unavailable"

  terms <- member$terms
  terms_count <- NA_integer_
  if (!is.null(terms) && length(terms) > 0) {
    terms_count <- length(terms)
  }
  terms_label <- if (!is.na(terms_count)) as.character(terms_count) else "Unavailable"

  leadership <- member$leadership
  leadership_summary <- "None listed"
  if (!is.null(leadership) && length(leadership) > 0) {
    leadership_rows <- as_tibble(leadership)
    if ("title" %in% names(leadership_rows)) {
      leadership_summary <- paste(unique(leadership_rows$title), collapse = "; ")
    }
  }

  sponsored_count <- NA_integer_
  if (!is.null(member$sponsoredLegislation) && !is.null(member$sponsoredLegislation$count)) {
    sponsored_count <- suppressWarnings(as.integer(member$sponsoredLegislation$count))
  }
  sponsored_label <- if (!is.na(sponsored_count)) as.character(sponsored_count) else "Unavailable"


  profile <- tibble(
    field = c(
      "Age",
      "Terms in office (count)",
      "Sponsored bills (count)",
      "Party leadership roles"
    ),
    value = c(
      age_label,
      terms_label,
      sponsored_label,
      leadership_summary
    )
  )

  list(
    ok = TRUE,
    note = "",
    profile = profile
  )
}
