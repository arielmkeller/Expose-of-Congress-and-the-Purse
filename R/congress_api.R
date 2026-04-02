library(httr2)
library(tibble)
library(dplyr)
library(jsonlite)

fetch_unitedstates_committee_map <- function() {
  committees_url <- "https://unitedstates.github.io/congress-legislators/committees-current.json"
  req <- request(committees_url) |>
    req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error") || resp_status(resp) >= 400) {
    return(list(map = NULL, error = "Committee map request failed."))
  }
  payload <- tryCatch(resp_body_string(resp), error = function(e) NULL)
  if (is.null(payload)) {
    return(list(map = NULL, error = "Committee map response unavailable."))
  }
  committees <- tryCatch(fromJSON(payload, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(committees) || !is.list(committees)) {
    return(list(map = NULL, error = "Committee map not parsed."))
  }
  map <- list()
  for (row in committees) {
    if (!is.list(row)) next
    thomas_id <- if (!is.null(row$thomas_id)) as.character(row$thomas_id) else NA_character_
    id <- if (!is.null(row$id)) as.character(row$id) else NA_character_
    code <- if (!is.null(row$code)) as.character(row$code) else NA_character_
    name <- if (!is.null(row$name)) as.character(row$name) else NA_character_
    if (!is.na(name) && name != "") {
      if (!is.na(thomas_id) && thomas_id != "") map[[thomas_id]] <- name
      if (!is.na(id) && id != "") map[[id]] <- name
      if (!is.na(code) && code != "") map[[code]] <- name
    }
    subs <- row$subcommittees
    if (is.list(subs) && length(subs) > 0) {
      for (sub in subs) {
        if (!is.list(sub)) next
        sub_thomas_id <- if (!is.null(sub$thomas_id)) as.character(sub$thomas_id) else NA_character_
        sub_id <- if (!is.null(sub$id)) as.character(sub$id) else NA_character_
        sub_code <- if (!is.null(sub$code)) as.character(sub$code) else NA_character_
        sub_name <- if (!is.null(sub$name)) as.character(sub$name) else NA_character_
        if (!is.na(sub_name) && sub_name != "" && !is.na(name) && name != "") {
          label <- paste0(name, " — ", sub_name)
          if (!is.na(sub_thomas_id) && sub_thomas_id != "") map[[sub_thomas_id]] <- label
          if (!is.na(sub_id) && sub_id != "") map[[sub_id]] <- label
          if (!is.na(sub_code) && sub_code != "") map[[sub_code]] <- label
        }
      }
    }
  }
  list(map = map, error = NULL)
}

fetch_committee_assignments_unitedstates <- function(bioguide_id) {
  membership_url <- "https://unitedstates.github.io/congress-legislators/committee-membership-current.json"
  req <- request(membership_url) |>
    req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error") || resp_status(resp) >= 400) {
    return(list(committees = character(), error = "Committee membership request failed."))
  }
  payload <- tryCatch(resp_body_string(resp), error = function(e) NULL)
  if (is.null(payload)) {
    return(list(committees = character(), error = "Committee membership response unavailable."))
  }
  membership <- tryCatch(fromJSON(payload, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(membership) || !is.list(membership)) {
    return(list(committees = character(), error = "Committee membership not parsed."))
  }
  committee_ids <- c()
  for (cid in names(membership)) {
    members <- membership[[cid]]
    if (is.data.frame(members) && "bioguide" %in% names(members)) {
      if (any(as.character(members$bioguide) == bioguide_id, na.rm = TRUE)) {
        committee_ids <- c(committee_ids, cid)
      }
    }
  }
  committee_ids <- unique(committee_ids)
  if (length(committee_ids) == 0) {
    return(list(committees = character(), error = "No committee assignments found."))
  }
  map_res <- fetch_unitedstates_committee_map()
  if (identical(Sys.getenv("CONGRESS_COMMITTEE_DEBUG", unset = ""), "1")) {
    message("Committee IDs: ", paste(committee_ids, collapse = ", "))
    if (!is.null(map_res$map)) {
      missing <- committee_ids[!vapply(committee_ids, function(id) !is.null(map_res$map[[id]]), logical(1))]
      if (length(missing) > 0) message("Missing in map: ", paste(missing, collapse = ", "))
    } else {
      message("Committee map unavailable.")
    }
  }
  derive_fallback_label <- function(code, map) {
    if (is.null(code) || is.na(code) || code == "") return(code)
    # Common pattern: parent committee + subcommittee number (e.g., HSAG15)
    m <- regexec("^([A-Z]{4})([0-9]{2})$", code)
    parts <- regmatches(code, m)[[1]]
    if (length(parts) == 3) {
      parent <- parts[[2]]
      subnum <- parts[[3]]
      parent_name <- if (!is.null(map) && !is.null(map[[parent]])) map[[parent]] else parent
      return(paste0(parent_name, " — Subcommittee ", subnum))
    }
    code
  }
  if (!is.null(map_res$map)) {
    names_mapped <- vapply(committee_ids, function(id) {
      if (!is.null(map_res$map[[id]])) return(map_res$map[[id]])
      derive_fallback_label(id, map_res$map)
    }, character(1))
  } else {
    names_mapped <- vapply(committee_ids, function(id) derive_fallback_label(id, NULL), character(1))
  }
  list(committees = names_mapped, error = map_res$error)
}

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
  birth_date <- if (!is.null(member$birthDate)) as.character(member$birthDate) else NA_character_
  gender <- if (!is.null(member$gender)) as.character(member$gender) else NA_character_

  terms <- member$terms
  term_start <- NA_character_
  term_end <- NA_character_
  if (!is.null(terms) && length(terms) > 0) {
    terms_tbl <- tryCatch(as_tibble(terms), error = function(e) NULL)
    if (!is.null(terms_tbl)) {
      start_candidates <- intersect(c("startDate", "startYear"), names(terms_tbl))
      end_candidates <- intersect(c("endDate", "endYear"), names(terms_tbl))
      if (length(start_candidates) > 0) {
        start_vals <- terms_tbl[[start_candidates[[1]]]]
        start_vals <- start_vals[!is.na(start_vals)]
        if (length(start_vals) > 0) term_start <- as.character(sort(start_vals)[[1]])
      }
      if (length(end_candidates) > 0) {
        end_vals <- terms_tbl[[end_candidates[[1]]]]
        end_vals <- end_vals[!is.na(end_vals)]
        if (length(end_vals) > 0) term_end <- as.character(sort(end_vals, decreasing = TRUE)[[1]])
      }
    }
  }

  leadership <- member$leadership
  leadership_summary <- "None listed"
  if (!is.null(leadership) && length(leadership) > 0) {
    leadership_rows <- tryCatch(as_tibble(leadership), error = function(e) NULL)
    if (!is.null(leadership_rows) && "title" %in% names(leadership_rows)) {
      leadership_summary <- paste(unique(leadership_rows$title), collapse = "; ")
    }
  }

  committees_summary <- "Unavailable"
  # Prefer UnitedStates.io committee membership (more reliable, but 2 calls).
  committees_res <- fetch_committee_assignments_unitedstates(bioguide_id)
  if (length(committees_res$committees) > 0) {
    committees_summary <- paste(head(committees_res$committees, 5), collapse = "; ")
  } else {
    committees <- NULL
    if (!is.null(member$committees)) committees <- member$committees
    if (is.null(committees) && !is.null(member$committeeAssignments)) committees <- member$committeeAssignments
    if (!is.null(committees) && length(committees) > 0) {
      committees_tbl <- tryCatch(as_tibble(committees), error = function(e) NULL)
      if (!is.null(committees_tbl)) {
        name_col <- intersect(c("name", "committeeName", "committee"), names(committees_tbl))
        if (length(name_col) > 0) {
          names_vec <- unique(as.character(committees_tbl[[name_col[[1]]]]))
          names_vec <- names_vec[!is.na(names_vec) & names_vec != ""]
          if (length(names_vec) > 0) {
            committees_summary <- paste(head(names_vec, 5), collapse = "; ")
          }
        }
      }
    }
  }

  website <- NA_character_
  if (!is.null(member$officialWebsiteUrl)) website <- as.character(member$officialWebsiteUrl)
  if (is.null(website) || is.na(website) || website == "") {
    if (!is.null(member$website)) website <- as.character(member$website)
  }
  contact <- NA_character_
  if (!is.null(member$contact)) contact <- as.character(member$contact)
  if (is.null(contact) || is.na(contact) || contact == "") {
    if (!is.null(member$phone)) contact <- as.character(member$phone)
  }

  profile <- tibble(
    field = c(
      "Term start",
      "Term end",
      "Leadership role",
      "Committee assignments",
      "Website",
      "Contact",
      "Gender",
      "Birthday"
    ),
    value = c(
      ifelse(is.na(term_start) || term_start == "", "Unavailable", term_start),
      ifelse(is.na(term_end) || term_end == "", "Unavailable", term_end),
      leadership_summary,
      committees_summary,
      ifelse(is.na(website) || website == "", "Unavailable", website),
      ifelse(is.na(contact) || contact == "", "Unavailable", contact),
      ifelse(is.na(gender) || gender == "", "Unavailable", gender),
      ifelse(!is.na(birth_date) && birth_date != "", birth_date,
             ifelse(!is.na(birth_year), as.character(birth_year), "Unavailable"))
    )
  )

  list(
    ok = TRUE,
    note = "",
    profile = profile
  )
}
