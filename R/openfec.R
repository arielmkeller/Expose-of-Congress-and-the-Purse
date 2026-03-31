library(httr2)
library(dplyr)
library(tibble)

openfec_cache_env <- new.env(parent = emptyenv())
openfec_cache_path <- file.path("data", "openfec_cache.rds")

load_openfec_cache <- function() {
  if (file.exists(openfec_cache_path)) {
    obj <- tryCatch(readRDS(openfec_cache_path), error = function(e) NULL)
    if (is.list(obj)) {
      list2env(obj, envir = openfec_cache_env)
    }
  }
}

save_openfec_cache <- function() {
  keys <- ls(envir = openfec_cache_env, all.names = TRUE)
  if (length(keys) == 0) {
    return(invisible(NULL))
  }
  obj <- mget(keys, envir = openfec_cache_env, inherits = FALSE)
  dir.create(dirname(openfec_cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(obj, openfec_cache_path), error = function(e) NULL)
}

load_local_env <- function() {
  candidates <- c(".env", file.path("data", ".env"))
  for (path in candidates) {
    if (!file.exists(path)) {
      next
    }
    lines <- readLines(path, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      if (identical(line, "") || startsWith(line, "#")) {
        next
      }
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) < 2) {
        next
      }
      key <- trimws(parts[[1]])
      value <- trimws(paste(parts[-1], collapse = "="))
      if (!identical(key, "") && identical(Sys.getenv(key, unset = ""), "")) {
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
  }
}

normalize_person_name <- function(x) {
  y <- tolower(x)
  y <- gsub("[^a-z\\s]", " ", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

name_similarity_score <- function(candidate_name, query_name) {
  c_name <- normalize_person_name(candidate_name)
  q_name <- normalize_person_name(query_name)
  if (identical(c_name, "") || identical(q_name, "")) {
    return(0)
  }

  c_tokens <- unique(unlist(strsplit(c_name, " ")))
  q_tokens <- unique(unlist(strsplit(q_name, " ")))
  overlap <- length(intersect(c_tokens, q_tokens))
  last_name_bonus <- ifelse(tail(q_tokens, 1) %in% c_tokens, 1, 0)
  overlap + last_name_bonus
}

extract_last_name <- function(x) {
  tokens <- unlist(strsplit(normalize_person_name(x), " "))
  if (length(tokens) == 0) {
    return("")
  }
  tail(tokens, 1)
}

extract_first_last <- function(x) {
  tokens <- unlist(strsplit(normalize_person_name(x), " "))
  if (length(tokens) == 0) {
    return("")
  }
  if (length(tokens) == 1) {
    return(tokens[[1]])
  }
  paste(tokens[[1]], tokens[[length(tokens)]])
}

clean_honorifics <- function(x) {
  y <- gsub("\\b(senator|sen|representative|rep|hon|mr|mrs|ms)\\b\\.?","", x, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", y))
}

swap_last_first <- function(x) {
  if (!grepl(",", x)) {
    return(x)
  }
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(gsub(",", " ", x))
  }
  trimws(paste(trimws(parts[[2]]), trimws(parts[[1]])))
}

build_query_variants <- function(name) {
  raw <- trimws(name)
  if (identical(raw, "")) {
    return(character())
  }
  cleaned <- clean_honorifics(raw)
  swapped <- swap_last_first(cleaned)
  first_last <- extract_first_last(swapped)
  last_name <- extract_last_name(swapped)
  unique(c(raw, cleaned, swapped, first_last, last_name))
}

openfec_candidate_search <- function(query, api_key, office = NULL, election_year = NULL, state = NULL, per_page = 20) {
  if (!exists("cache_loaded", envir = openfec_cache_env, inherits = FALSE)) {
    load_openfec_cache()
    assign("cache_loaded", TRUE, envir = openfec_cache_env)
  }

  cache_key <- paste(
    "q", query,
    "office", ifelse(is.null(office), "NULL", office),
    "year", ifelse(is.null(election_year), "NULL", as.character(election_year)),
    "state", ifelse(is.null(state), "NULL", state),
    "per_page", per_page,
    sep = "|"
  )
  if (exists(cache_key, envir = openfec_cache_env, inherits = FALSE)) {
    return(get(cache_key, envir = openfec_cache_env, inherits = FALSE))
  }

  req <- request("https://api.open.fec.gov/v1/candidates/search/") |>
    req_url_query(
      q = query,
      api_key = api_key,
      per_page = per_page
    )
  if (!is.null(office) && nzchar(office)) {
    req <- req |>
      req_url_query(office = office)
  }
  if (!is.null(election_year) && !is.na(election_year)) {
    req <- req |>
      req_url_query(election_year = election_year)
  }
  if (!is.null(state) && !is.na(state) && nzchar(state)) {
    req <- req |>
      req_url_query(state = toupper(state))
  }

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (!is.null(resp) && resp_status(resp) == 429) {
    Sys.sleep(1.5)
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
  }
  if (is.null(resp)) {
    return(NULL)
  }
  if (resp_status(resp) == 429) {
    return(structure(list(), class = "openfec_rate_limited"))
  }
  if (resp_status(resp) >= 400) {
    body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
    if (nchar(body_text) > 200) {
      body_text <- paste0(substr(body_text, 1, 200), "…")
    }
    return(structure(list(status = resp_status(resp), message = body_text), class = "openfec_error"))
  }
  payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(payload$results) || length(payload$results) == 0) {
    assign(cache_key, NULL, envir = openfec_cache_env)
    save_openfec_cache()
    return(NULL)
  }
  res <- as_tibble(payload$results)
  assign(cache_key, res, envir = openfec_cache_env)
  save_openfec_cache()
  res
}

openfec_api_check <- function(api_key) {
  req <- request("https://api.open.fec.gov/v1/candidates/search/") |>
    req_url_query(q = "test", api_key = api_key, per_page = 1)
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(resp$message)
  }
  status <- resp_status(resp)
  if (status >= 400) {
    body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
    if (nchar(body_text) > 160) {
      body_text <- paste0(substr(body_text, 1, 160), "…")
    }
    return(paste0("HTTP ", status, ifelse(nchar(body_text) > 0, paste0(": ", body_text), "")))
  }
  NULL
}

get_openfec_summary <- function(legislator, cycle = 2024, chamber = NA_character_, state = NULL) {
  load_local_env()
  api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
  if (identical(api_key, "")) {
    return(tibble(
      source = c("Individuals", "PACs"),
      amount_usd = c(1200000, 550000),
      note = "Set OPENFEC_API_KEY to fetch live data."
    ))
  }

  api_check <- openfec_api_check(api_key)
  if (!is.null(api_check)) {
    return(tibble(
      source = "API Error",
      amount_usd = NA_real_,
      note = paste("OpenFEC request failed.", api_check)
    ))
  }

  office <- ifelse(tolower(chamber) == "senate", "S", ifelse(tolower(chamber) == "house", "H", NA_character_))
  queries <- build_query_variants(legislator)
  if (length(queries) == 0) {
    return(tibble(
      source = "No Match",
      amount_usd = NA_real_,
      note = "No candidate found for that query."
    ))
  }

  office_candidates <- unique(c(office, if (is.na(office)) "P" else NA_character_))
  office_candidates <- office_candidates[!is.na(office_candidates)]

  candidate_sets <- list()
  for (q in queries) {
    for (off in c(office_candidates, NA_character_)) {
      attempts <- list(
        list(q = q, off = off, year = cycle, st = state),
        list(q = q, off = off, year = cycle, st = NULL),
        list(q = q, off = off, year = NULL, st = state),
        list(q = q, off = off, year = NULL, st = NULL)
      )
      for (a in attempts) {
        res <- openfec_candidate_search(a$q, api_key, office = a$off, election_year = a$year, state = a$st)
        if (inherits(res, "openfec_rate_limited")) {
          return(tibble(
            source = "Rate Limited",
            amount_usd = NA_real_,
            note = "OpenFEC rate limit hit (HTTP 429). Wait and retry."
          ))
        }
        if (inherits(res, "openfec_error")) {
          return(tibble(
            source = "API Error",
            amount_usd = NA_real_,
            note = paste("OpenFEC request failed.", res$status, res$message)
          ))
        }
        if (!is.null(res) && nrow(res) > 0) {
          candidate_sets <- append(candidate_sets, list(res))
          break
        }
      }
    }
  }
  candidate_sets <- Filter(Negate(is.null), candidate_sets)
  if (length(candidate_sets) == 0) {
    return(tibble(
      source = "No Match",
      amount_usd = NA_real_,
      note = "No candidate found for that query."
    ))
  }

  candidates <- bind_rows(candidate_sets)
  if (!"candidate_id" %in% names(candidates)) {
    return(tibble(
      source = "No Match",
      amount_usd = NA_real_,
      note = "No candidate found for that query."
    ))
  }

  candidates <- candidates |>
    distinct(candidate_id, .keep_all = TRUE)

  if (nrow(candidates) == 0) {
    return(tibble(
      source = "No Match",
      amount_usd = NA_real_,
      note = "No candidate found for that query."
    ))
  }

  candidates <- candidates |>
    mutate(
      score_name = vapply(name, name_similarity_score, numeric(1), query_name = legislator),
      office_match = if ("office" %in% names(candidates)) toupper(.data$office) == toupper(office) else FALSE,
      state_match = if (!is.null(state) && "state" %in% names(candidates)) toupper(.data$state) == toupper(state) else FALSE,
      cycle_match = if ("election_years" %in% names(candidates)) {
        vapply(election_years, function(x) {
          if (is.null(x)) return(FALSE)
          any(as.integer(x) == as.integer(cycle), na.rm = TRUE)
        }, logical(1))
      } else {
        FALSE
      },
      incumbent_flag = ifelse(is.na(incumbent_challenge_full), "", incumbent_challenge_full),
      score = score_name +
        ifelse(office_match, 1, 0) +
        ifelse(state_match, 1, 0) +
        ifelse(cycle_match, 1, 0)
    ) |>
    arrange(desc(score), desc(incumbent_flag == "Incumbent"))

  candidate_id <- candidates$candidate_id[[1]]
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

get_openfec_committees_for_legislator <- function(legislator, cycle = 2024, chamber = NA_character_, state = NULL) {
  load_local_env()
  api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
  if (identical(api_key, "")) {
    return(tibble(note = "Set OPENFEC_API_KEY to fetch committee data."))
  }

  api_check <- openfec_api_check(api_key)
  if (!is.null(api_check)) {
    return(tibble(note = paste("OpenFEC request failed.", api_check)))
  }

  office <- ifelse(tolower(chamber) == "senate", "S", ifelse(tolower(chamber) == "house", "H", NA_character_))
  queries <- build_query_variants(legislator)
  if (length(queries) == 0) {
    return(tibble(note = "No candidate found for that query."))
  }

  office_candidates <- unique(c(office, if (is.na(office)) "P" else NA_character_))
  office_candidates <- office_candidates[!is.na(office_candidates)]

  candidate_sets <- list()
  for (q in queries) {
    for (off in c(office_candidates, NA_character_)) {
      attempts <- list(
        list(q = q, off = off, year = cycle, st = state),
        list(q = q, off = off, year = cycle, st = NULL),
        list(q = q, off = off, year = NULL, st = state),
        list(q = q, off = off, year = NULL, st = NULL)
      )
      for (a in attempts) {
        res <- openfec_candidate_search(a$q, api_key, office = a$off, election_year = a$year, state = a$st)
        if (inherits(res, "openfec_rate_limited")) {
          return(tibble(note = "OpenFEC rate limit hit (HTTP 429). Wait and retry."))
        }
        if (inherits(res, "openfec_error")) {
          return(tibble(note = paste("OpenFEC request failed.", res$status, res$message)))
        }
        if (!is.null(res) && nrow(res) > 0) {
          candidate_sets <- append(candidate_sets, list(res))
          break
        }
      }
    }
  }
  candidate_sets <- Filter(Negate(is.null), candidate_sets)
  if (length(candidate_sets) == 0) {
    return(tibble(note = "No candidate found for that query."))
  }

  candidates <- bind_rows(candidate_sets)
  if (!"candidate_id" %in% names(candidates)) {
    return(tibble(note = "No candidate found for that query."))
  }

  candidates <- candidates |>
    distinct(candidate_id, .keep_all = TRUE)

  if (nrow(candidates) == 0) {
    return(tibble(note = "No candidate found for that query."))
  }

  candidates <- candidates |>
    mutate(
      score_name = vapply(name, name_similarity_score, numeric(1), query_name = legislator),
      office_match = if ("office" %in% names(candidates)) toupper(.data$office) == toupper(office) else FALSE,
      state_match = if (!is.null(state) && "state" %in% names(candidates)) toupper(.data$state) == toupper(state) else FALSE,
      cycle_match = if ("election_years" %in% names(candidates)) {
        vapply(election_years, function(x) {
          if (is.null(x)) return(FALSE)
          any(as.integer(x) == as.integer(cycle), na.rm = TRUE)
        }, logical(1))
      } else {
        FALSE
      },
      incumbent_flag = ifelse(is.na(incumbent_challenge_full), "", incumbent_challenge_full),
      score = score_name +
        ifelse(office_match, 1, 0) +
        ifelse(state_match, 1, 0) +
        ifelse(cycle_match, 1, 0)
    ) |>
    arrange(desc(score), desc(incumbent_flag == "Incumbent"))

  candidate_id <- candidates$candidate_id[[1]]

  committees <- list()
  page <- 1
  repeat {
    req <- request(paste0("https://api.open.fec.gov/v1/candidate/", candidate_id, "/committees/")) |>
      req_url_query(
        api_key = api_key,
        cycle = cycle,
        per_page = 100,
        page = page
      )
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    if (is.null(resp)) {
      return(tibble(note = "OpenFEC committee request failed."))
    }
    if (resp_status(resp) == 429) {
      return(tibble(note = "OpenFEC rate limit hit (HTTP 429). Wait and retry."))
    }
    payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(payload$results) || length(payload$results) == 0) {
      break
    }
    committees <- append(committees, list(as_tibble(payload$results)))
    total_pages <- payload$pagination$pages
    if (is.null(total_pages) || page >= total_pages) {
      break
    }
    page <- page + 1
  }

  if (length(committees) == 0) {
    return(tibble(note = "No committees returned for candidate."))
  }

  res <- bind_rows(committees)
  if (!"committee_id" %in% names(res)) {
    return(tibble(note = "Committee IDs unavailable for candidate."))
  }

  res |>
    distinct(committee_id, .keep_all = TRUE) |>
    transmute(
      committee_id = committee_id,
      committee_name = if ("name" %in% names(res)) name else NA_character_,
      designation = if ("designation" %in% names(res)) designation else NA_character_,
      committee_type = if ("committee_type" %in% names(res)) committee_type else NA_character_
    )
}
