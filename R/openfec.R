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

sanitize_openfec_query <- function(x) {
  y <- trimws(x)
  if (identical(y, "")) {
    return(y)
  }
  y <- gsub("\\s*\\[[^\\]]+\\]\\s*$", "", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

normalize_openfec_query <- function(x) {
  if (is.null(x) || is.na(x)) {
    return("")
  }
  y <- tryCatch(iconv(x, to = "ASCII//TRANSLIT"), error = function(e) x)
  if (is.na(y) || is.null(y)) {
    y <- x
  }
  y <- gsub("[^A-Za-z0-9[:space:]'-]", "", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

sanitize_candidate_id <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  y <- toupper(trimws(as.character(x)))
  y <- gsub("[^A-Z0-9]", "", y)
  if (!grepl("^[HSP][A-Z0-9]{8}$", y)) {
    return(NA_character_)
  }
  y
}

pick_first_numeric <- function(res, fields) {
  for (f in fields) {
    if (!is.null(res[[f]])) {
      val <- suppressWarnings(as.numeric(res[[f]]))
      if (!is.na(val)) {
        return(val)
      }
    }
  }
  NA_real_
}

openfec_committee_totals <- function(committee_id, cycle, api_key) {
  if (is.null(committee_id) || is.na(committee_id) || !nzchar(committee_id)) {
    return(list(error = "Missing committee_id"))
  }
  req <- request(paste0("https://api.open.fec.gov/v1/committee/", committee_id, "/totals/")) |>
    req_error(is_error = function(resp) FALSE) |>
    req_url_query(
      cycle = cycle,
      api_key = api_key,
      per_page = 1
    )
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(error = resp$message))
  }
  if (resp_status(resp) >= 400) {
    body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
    if (nchar(body_text) > 160) {
      body_text <- paste0(substr(body_text, 1, 160), "…")
    }
    # Fallback to plural endpoint
    req2 <- request("https://api.open.fec.gov/v1/committee/totals/") |>
      req_error(is_error = function(resp) FALSE) |>
      req_url_query(
        committee_id = committee_id,
        cycle = cycle,
        api_key = api_key,
        per_page = 1
      )
    resp2 <- tryCatch(req_perform(req2), error = function(e) e)
    if (!inherits(resp2, "error") && resp_status(resp2) < 400) {
      json2 <- resp_body_json(resp2, simplifyVector = TRUE)
      return(list(json = json2))
    }
    return(list(error = paste0("HTTP ", resp_status(resp), ": ", body_text)))
  }
  json <- resp_body_json(resp, simplifyVector = TRUE)
  list(json = json)
}

format_district <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) {
    return(NA_character_)
  }
  val <- suppressWarnings(as.integer(x))
  if (is.na(val)) {
    return(NA_character_)
  }
  sprintf("%02d", val)
}

extract_principal_committee_ids <- function(candidate_row) {
  committees <- candidate_row$principal_committees
  if (is.null(committees) || length(committees) == 0) {
    return(character())
  }
  if (is.data.frame(committees)) {
    if ("committee_id" %in% names(committees)) {
      return(unique(as.character(committees$committee_id)))
    }
  }
  if (is.list(committees)) {
    ids <- unlist(lapply(committees, function(x) {
      if (is.null(x)) return(character())
      if (!is.null(x$committee_id)) return(as.character(x$committee_id))
      character()
    }), use.names = FALSE)
    return(unique(ids[ids != ""]))
  }
  character()
}

openfec_candidate_committees <- function(candidate_id, api_key, cycle = NULL) {
  req <- request(paste0("https://api.open.fec.gov/v1/candidate/", candidate_id, "/committees/")) |>
    req_error(is_error = function(resp) FALSE) |>
    req_url_query(api_key = api_key, per_page = 100, designation = "P")
  if (!is.null(cycle) && !is.na(cycle)) {
    req <- req |>
      req_url_query(cycle = cycle)
  }
  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error") || resp_status(resp) >= 400) {
    return(character())
  }
  payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(payload$results) || length(payload$results) == 0) {
    return(character())
  }
  committees <- as_tibble(payload$results)
  if (!"committee_id" %in% names(committees)) {
    return(character())
  }
  committees |>
    pull(committee_id) |>
    unique() |>
    as.character()
}

openfec_schedule_a_by_category <- function(endpoint, committee_ids, cycle, api_key, label_field) {
  if (length(committee_ids) == 0) {
    return(tibble(
      label = character(),
      amount_usd = numeric(),
      note = character()
    ))
  }

  results <- list()
  last_error <- NULL
  for (committee_id in committee_ids) {
    req <- request(endpoint) |>
      req_error(is_error = function(resp) FALSE) |>
      req_url_query(
        api_key = api_key,
        committee_id = committee_id,
        cycle = cycle,
        per_page = 10,
        sort = "-total"
      )
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      last_error <- resp$message
      next
    }
    if (resp_status(resp) >= 400) {
      body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
      if (nchar(body_text) > 160) {
        body_text <- paste0(substr(body_text, 1, 160), "…")
      }
      last_error <- paste0("HTTP ", resp_status(resp), ifelse(nchar(body_text) > 0, paste0(": ", body_text), ""))
      next
    }
    payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(payload$results) || length(payload$results) == 0) {
      next
    }
    res <- as_tibble(payload$results)
    if (!label_field %in% names(res)) {
      next
    }
    if (!"total" %in% names(res)) {
      next
    }
    res <- res |>
      transmute(
        label = as.character(.data[[label_field]]),
        amount_usd = as.numeric(.data$total)
      )
    results <- append(results, list(res))
  }

  if (length(results) == 0) {
    return(tibble(
      label = character(),
      amount_usd = numeric(),
      note = ifelse(is.null(last_error), "No Schedule A breakdown returned.", last_error)
    ))
  }

  bind_rows(results) |>
    filter(!is.na(label), label != "") |>
    group_by(label) |>
    summarize(amount_usd = sum(amount_usd, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(amount_usd)) |>
    head(10) |>
    mutate(note = "")
}

openfec_schedule_a_by_employer <- function(committee_ids, cycle, api_key) {
  openfec_schedule_a_by_category(
    "https://api.open.fec.gov/v1/schedules/schedule_a/by_employer/",
    committee_ids,
    cycle,
    api_key,
    "employer"
  ) |>
    rename(employer = label)
}

openfec_schedule_a_by_occupation <- function(committee_ids, cycle, api_key) {
  openfec_schedule_a_by_category(
    "https://api.open.fec.gov/v1/schedules/schedule_a/by_occupation/",
    committee_ids,
    cycle,
    api_key,
    "occupation"
  ) |>
    rename(occupation = label)
}

openfec_schedule_a_rows <- function(committee_ids, cycle, api_key, max_pages = 3, per_page = 100) {
  if (length(committee_ids) == 0 || is.null(cycle) || is.na(cycle)) {
    return(list(rows = NULL, error = NULL))
  }
  rows <- list()
  last_error <- NULL
  for (committee_id in committee_ids) {
    page <- 1
    repeat {
      req <- request("https://api.open.fec.gov/v1/schedules/schedule_a/") |>
        req_error(is_error = function(resp) FALSE) |>
        req_url_query(
          api_key = api_key,
          committee_id = committee_id,
          cycle = cycle,
          per_page = per_page,
          page = page,
          sort = "-contribution_receipt_amount"
        )
      resp <- tryCatch(req_perform(req), error = function(e) e)
      if (inherits(resp, "error")) {
        last_error <- resp$message
        break
      }
      if (resp_status(resp) >= 400) {
        body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
        if (nchar(body_text) > 160) {
          body_text <- paste0(substr(body_text, 1, 160), "…")
        }
        last_error <- paste0("HTTP ", resp_status(resp), ifelse(nchar(body_text) > 0, paste0(": ", body_text), ""))
        break
      }
      payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(payload$results) || length(payload$results) == 0) {
        break
      }
      rows <- append(rows, list(as_tibble(payload$results)))
      total_pages <- payload$pagination$pages
      if (is.null(total_pages) || page >= total_pages || page >= max_pages) {
        break
      }
      page <- page + 1
    }
  }
  if (length(rows) == 0) {
    return(list(rows = NULL, error = last_error))
  }
  list(rows = bind_rows(rows), error = last_error)
}

openfec_schedule_a_committee_contributors <- function(committee_ids, cycle, api_key, internal_only = FALSE, max_pages = 3, per_page = 100) {
  empty_result <- function(note) {
    tibble(pac = character(), amount_usd = numeric(), note = note)
  }

  if (length(committee_ids) == 0) {
    return(empty_result("No committee IDs available for committee breakdown."))
  }
  if (is.null(cycle) || is.na(cycle)) {
    return(empty_result("No cycle available for committee breakdown."))
  }

  fetched <- openfec_schedule_a_rows(committee_ids, cycle, api_key, max_pages = max_pages, per_page = per_page)
  if (is.null(fetched$rows)) {
    note <- ifelse(is.null(fetched$error), "No Schedule A rows returned for committee breakdown.", fetched$error)
    return(empty_result(note))
  }

  res <- fetched$rows
  amount_col <- if ("contribution_receipt_amount" %in% names(res)) {
    "contribution_receipt_amount"
  } else if ("total" %in% names(res)) {
    "total"
  } else {
    NA_character_
  }
  if (is.na(amount_col)) {
    return(empty_result("Schedule A rows missing contribution amounts."))
  }

  name_col <- intersect(c("contributor_name", "contributor", "name"), names(res))
  name_col <- if (length(name_col) > 0) name_col[[1]] else NA_character_
  id_col <- intersect(c("contributor_committee_id", "committee_id", "contributor_id"), names(res))
  id_col <- if (length(id_col) > 0) id_col[[1]] else NA_character_

  df <- res |>
    transmute(
      pac_id = if (!is.na(id_col)) as.character(.data[[id_col]]) else NA_character_,
      pac_name = if (!is.na(name_col)) as.character(.data[[name_col]]) else NA_character_,
      amount_usd = as.numeric(.data[[amount_col]])
    )

  df <- df |>
    mutate(
      pac_id = ifelse(is.na(pac_id) | pac_id == "", NA_character_, pac_id),
      pac_name = ifelse(is.na(pac_name) | pac_name == "", NA_character_, pac_name),
      is_committee = !is.na(pac_id),
      pac_key = ifelse(!is.na(pac_id), pac_id, pac_name),
      pac_label = ifelse(!is.na(pac_name), pac_name, pac_id),
      is_internal = pac_id %in% committee_ids
    ) |>
    filter(is_committee, !is.na(pac_key), !is.na(pac_label))

  if (internal_only) {
    df <- df |> filter(is_internal)
  } else {
    df <- df |> filter(!is_internal)
  }

  if (nrow(df) == 0) {
    return(empty_result("No committee contributors found in Schedule A rows."))
  }

  df |>
    group_by(pac_key, pac_label) |>
    summarize(amount_usd = sum(amount_usd, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(amount_usd)) |>
    head(10) |>
    transmute(pac = pac_label, amount_usd = amount_usd, note = "")
}

openfec_schedule_a_top_committees <- function(committee_ids, cycle, api_key, max_pages = 3, per_page = 100) {
  openfec_schedule_a_committee_contributors(
    committee_ids,
    cycle,
    api_key,
    internal_only = FALSE,
    max_pages = max_pages,
    per_page = per_page
  )
}

openfec_schedule_a_internal_transfers <- function(committee_ids, cycle, api_key, max_pages = 3, per_page = 100) {
  openfec_schedule_a_committee_contributors(
    committee_ids,
    cycle,
    api_key,
    internal_only = TRUE,
    max_pages = max_pages,
    per_page = per_page
  )
}

openfec_receipts_trend <- function(candidate_id, cycle, api_key) {
  if (is.null(cycle) || is.na(cycle)) {
    return(tibble(period = character(), amount_usd = numeric(), note = character()))
  }

  cycles <- sort(unique(c(cycle - 4, cycle - 2, cycle)))
  totals <- lapply(cycles, function(cyc) {
    req <- request("https://api.open.fec.gov/v1/candidate/totals/") |>
      req_error(is_error = function(resp) FALSE) |>
      req_url_query(
        candidate_id = candidate_id,
        cycle = cyc,
        api_key = api_key,
        per_page = 1
      )
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error") || resp_status(resp) >= 400) {
      return(NULL)
    }
    payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(payload$results) || length(payload$results) == 0) {
      return(NULL)
    }
    res <- payload$results[1, ]
    tibble(
      period = paste0("Cycle ", cyc),
      amount_usd = as.numeric(res$receipts)
    )
  })

  totals <- bind_rows(totals)
  if (nrow(totals) == 0) {
    return(tibble(period = character(), amount_usd = numeric(), note = character()))
  }
  totals |>
    mutate(note = "")
}

openfec_candidate_search <- function(query, api_key, office = NULL, election_year = NULL, state = NULL, district = NULL, per_page = 20) {
  query <- sanitize_openfec_query(query)
  office_val <- NULL
  if (!is.null(office) && !is.na(office)) {
    office_val <- toupper(trimws(as.character(office)))
    if (!(office_val %in% c("H", "S", "P"))) {
      office_val <- NULL
    }
  }
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
      req_error(is_error = function(resp) FALSE) |>
      req_url_query(
        q = query,
        api_key = api_key,
        per_page = per_page
      )
  if (!is.null(office_val)) {
    req <- req |>
      req_url_query(office = office_val)
  }
  if (!is.null(election_year) && !is.na(election_year)) {
    req <- req |>
      req_url_query(election_year = election_year)
  }
  if (!is.null(state) && !is.na(state) && nzchar(state)) {
    req <- req |>
      req_url_query(state = toupper(state))
  }
  if (!is.null(district) && !is.na(district) && nzchar(district) && toupper(office) == "H") {
    req <- req |>
      req_url_query(district = district)
  }

  resp <- tryCatch(req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(structure(list(message = resp$message), class = "openfec_request_error"))
  }
  if (!is.null(resp) && resp_status(resp) == 429) {
    Sys.sleep(1.5)
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      return(structure(list(message = resp$message), class = "openfec_request_error"))
    }
  }
  if (is.null(resp)) {
    return(NULL)
  }
  if (resp_status(resp) == 429) {
    return(structure(list(), class = "openfec_rate_limited"))
  }
  if (resp_status(resp) >= 400) {
    body_text <- tryCatch(resp_body_string(resp), error = function(e) "")
    if (nchar(body_text) > 160) {
      body_text <- paste0(substr(body_text, 1, 160), "…")
    }
    return(structure(
      list(status = resp_status(resp), body = body_text),
      class = "openfec_http_error"
    ))
  }
  payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(payload$results) || length(payload$results) == 0) {
    return(NULL)
  }
  res <- as_tibble(payload$results)
  assign(cache_key, res, envir = openfec_cache_env)
  save_openfec_cache()
  res
}

openfec_api_check <- function(api_key) {
  req <- request("https://api.open.fec.gov/v1/candidates/search/") |>
    req_error(is_error = function(resp) FALSE) |>
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

get_openfec_summary <- function(legislator, cycle = 2024, chamber = NA_character_, state = NULL, district = NULL) {
  load_local_env()
  api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
  if (identical(api_key, "")) {
    return(list(
      diagnostics = list(
        api_key_present = FALSE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character()
      ),
      summary = tibble(
        source = c("Individuals", "PACs", "Total receipts"),
        amount_usd = c(1200000, 550000, 1750000),
        note = "Set OPENFEC_API_KEY to fetch live data."
      ),
      donor_types = tibble(
        donor_type = c("Individuals", "PACs"),
        amount_usd = c(1200000, 550000),
        note = "Sample data."
      ),
      industries = tibble(
        industry = c("Finance", "Health", "Energy"),
        amount_usd = c(420000, 310000, 220000),
        note = "Sample data."
      ),
      occupations = tibble(
        occupation = c("Healthcare", "Attorney", "Retired"),
        amount_usd = c(180000, 160000, 90000),
        note = "Sample data."
      ),
      pacs_all = tibble(
        pac = c("Citizens for Example PAC", "Committee for Good Govt"),
        amount_usd = c(150000, 120000),
        note = "Sample data."
      ),
      pacs_internal = tibble(
        pac = c("Example Authorized Committee"),
        amount_usd = c(400000),
        note = "Sample data."
      ),
      trend = tibble(
        period = c("Past", "Recent", "Now"),
        amount_usd = c(300000, 500000, 700000),
        note = "Sample data."
      )
    ))
  }

  api_check <- openfec_api_check(api_key)
  if (!is.null(api_check)) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        error = api_check
      ),
      summary = tibble(
        source = "API Error",
        amount_usd = NA_real_,
        note = paste("OpenFEC request failed.", api_check)
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  territory_states <- c("PR", "VI", "GU", "AS", "MP")
  if (!is.null(state) && !is.na(state) && toupper(state) %in% territory_states) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        state_used = toupper(state),
        error = paste0("OpenFEC unavailable for territory ", toupper(state))
      ),
      summary = tibble(
        source = "Unavailable",
        amount_usd = NA_real_,
        note = paste0("OpenFEC data not available for territory ", toupper(state), ".")
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  office <- ifelse(tolower(chamber) == "senate", "S", ifelse(tolower(chamber) == "house", "H", NA_character_))
  district <- format_district(district)
  if (!is.null(district) && !is.na(district) && district == "00") {
    district <- NA_character_
  }
  queries <- build_query_variants(legislator)
  if (length(queries) > 0) {
    normalized <- vapply(queries, normalize_openfec_query, character(1))
    queries <- unique(c(queries, normalized))
    queries <- queries[nzchar(queries)]
    queries <- queries[nchar(gsub("[^A-Za-z0-9]", "", queries)) >= 3]
  }
  if (length(queries) == 0) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        error = "No search query"
      ),
      summary = tibble(
        source = "No Match",
        amount_usd = NA_real_,
        note = "No candidate found for that query."
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  office_candidates <- unique(c(office, if (is.na(office)) "P" else NA_character_))
  office_candidates <- office_candidates[!is.na(office_candidates)]

  candidate_sets_state <- list()
  candidate_sets_any <- list()
  last_http_error <- NULL
  last_request_error <- NULL
  for (q in queries) {
    for (off in c(office_candidates, NA_character_)) {
      attempts <- list(
        list(q = q, off = off, year = cycle, st = state),
        list(q = q, off = off, year = cycle, st = NULL),
        list(q = q, off = off, year = NULL, st = state),
        list(q = q, off = off, year = NULL, st = NULL)
      )
      for (a in attempts) {
        res <- openfec_candidate_search(
          a$q,
          api_key,
          office = a$off,
          election_year = a$year,
          state = a$st,
          district = ifelse(toupper(a$off) == "H", district, NA_character_)
        )
        if (inherits(res, "openfec_rate_limited")) {
          return(list(
            diagnostics = list(
              api_key_present = TRUE,
              candidate_id = NA_character_,
              cycle_used = NA_integer_,
              principal_committees = character(),
              error = "Rate limited"
            ),
            summary = tibble(
              source = "Rate Limited",
              amount_usd = NA_real_,
              note = "OpenFEC rate limit hit (HTTP 429). Wait and retry."
            ),
            donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
            industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
            occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
            trend = tibble(period = character(), amount_usd = numeric(), note = "")
          ))
        }
        if (inherits(res, "openfec_request_error")) {
          last_request_error <- res$message
          next
        }
        if (inherits(res, "openfec_http_error")) {
          last_http_error <- list(status = res$status, body = res$body)
          if (res$status == 422) {
            next
          }
          detail <- ifelse(nchar(res$body) > 0, paste0(" ", res$body), "")
          return(list(
            diagnostics = list(
              api_key_present = TRUE,
              candidate_id = NA_character_,
              cycle_used = NA_integer_,
              principal_committees = character(),
              error = paste0("HTTP ", res$status, ": ", res$body)
            ),
            summary = tibble(
              source = "API Error",
              amount_usd = NA_real_,
              note = paste0("OpenFEC request failed. HTTP ", res$status, ".", detail)
            ),
            donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
            industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
            occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
            trend = tibble(period = character(), amount_usd = numeric(), note = "")
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
          if (!is.null(a$st) && !is.na(a$st) && nzchar(a$st)) {
            candidate_sets_state <- append(candidate_sets_state, list(res))
          } else {
            candidate_sets_any <- append(candidate_sets_any, list(res))
          }
          break
        }
      }
    }
  }
  candidate_sets_state <- Filter(Negate(is.null), candidate_sets_state)
  candidate_sets_any <- Filter(Negate(is.null), candidate_sets_any)
  if (!is.null(state) && !is.na(state) && nzchar(state) && length(candidate_sets_state) == 0) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        state_used = toupper(state),
        error = paste0("No match in state ", toupper(state))
      ),
      summary = tibble(
        source = "No Match",
        amount_usd = NA_real_,
        note = paste0("No candidate found for state ", toupper(state), ".")
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }
  candidate_sets <- if (length(candidate_sets_state) > 0) candidate_sets_state else candidate_sets_any
  if (length(candidate_sets) == 0) {
    if (!is.null(last_request_error)) {
      return(list(
        diagnostics = list(
          api_key_present = TRUE,
          candidate_id = NA_character_,
          cycle_used = NA_integer_,
          principal_committees = character(),
          error = last_request_error
        ),
        summary = tibble(
          source = "API Error",
          amount_usd = NA_real_,
          note = paste("OpenFEC request failed.", last_request_error)
        ),
        donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
        industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
        occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
        trend = tibble(period = character(), amount_usd = numeric(), note = "")
      ))
    }
    if (!is.null(last_http_error)) {
      detail <- ifelse(nchar(last_http_error$body) > 0, paste0(" ", last_http_error$body), "")
      return(list(
        diagnostics = list(
          api_key_present = TRUE,
          candidate_id = NA_character_,
          cycle_used = NA_integer_,
          principal_committees = character(),
          error = paste0("HTTP ", last_http_error$status, ": ", last_http_error$body)
        ),
        summary = tibble(
          source = "API Error",
          amount_usd = NA_real_,
          note = paste0("OpenFEC request failed. HTTP ", last_http_error$status, ".", detail)
        ),
        donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
        industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
        occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
        trend = tibble(period = character(), amount_usd = numeric(), note = "")
      ))
    }
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        error = "No match"
      ),
      summary = tibble(
        source = "No Match",
        amount_usd = NA_real_,
        note = "No candidate found for that query."
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  candidates <- bind_rows(candidate_sets)
  candidates_debug <- character()
  if (nrow(candidates) > 0) {
    debug_cols <- intersect(c("candidate_id", "name", "state", "office", "party", "election_years"), names(candidates))
    debug_rows <- candidates |>
      select(all_of(debug_cols)) |>
      distinct()
    candidates_debug <- apply(debug_rows, 1, function(row) {
      parts <- c()
      if ("candidate_id" %in% names(row)) parts <- c(parts, paste0("id=", row[["candidate_id"]]))
      if ("name" %in% names(row)) parts <- c(parts, paste0("name=", row[["name"]]))
      if ("state" %in% names(row)) parts <- c(parts, paste0("state=", row[["state"]]))
      if ("office" %in% names(row)) parts <- c(parts, paste0("office=", row[["office"]]))
      if ("party" %in% names(row)) parts <- c(parts, paste0("party=", row[["party"]]))
      if ("election_years" %in% names(row)) parts <- c(parts, paste0("years=", paste(unlist(row[["election_years"]]), collapse = ",")))
      paste(parts, collapse = "; ")
    })
  }
  if (!"candidate_id" %in% names(candidates)) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        candidate_options = candidates_debug,
        error = "Missing candidate_id"
      ),
      summary = tibble(
        source = "No Match",
        amount_usd = NA_real_,
        note = "No candidate found for that query."
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  candidates <- candidates |>
    distinct(candidate_id, .keep_all = TRUE)

  if (!is.null(state) && !is.na(state) && nzchar(state) && "state" %in% names(candidates)) {
    candidates <- candidates |>
      filter(toupper(.data$state) == toupper(state))
  }

  if (nrow(candidates) == 0) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        candidate_options = candidates_debug,
        error = ifelse(!is.null(state) && !is.na(state) && nzchar(state),
          paste0("No match in state ", toupper(state)),
          "No match"
        )
      ),
      summary = tibble(
        source = "No Match",
        amount_usd = NA_real_,
        note = "No candidate found for that query."
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  candidates <- candidates |>
    mutate(
      score_name = vapply(name, name_similarity_score, numeric(1), query_name = legislator),
      office_match = if ("office" %in% names(candidates)) toupper(.data$office) == toupper(office) else FALSE,
      state_match = if (!is.null(state) && "state" %in% names(candidates)) toupper(.data$state) == toupper(state) else FALSE,
      district_match = if (!is.null(district) && "district" %in% names(candidates)) {
        suppressWarnings(as.integer(.data$district) == as.integer(district))
      } else {
        FALSE
      },
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
        ifelse(district_match, 1, 0) +
        ifelse(cycle_match, 1, 0)
    )

  filtered_candidates <- candidates
  if (!is.null(cycle) && !is.na(cycle)) {
    filtered_candidates <- candidates |>
      filter(cycle_match)
  }

  if (nrow(filtered_candidates) == 0) {
    filtered_candidates <- candidates
  }

  candidates <- filtered_candidates |>
    arrange(desc(score), desc(incumbent_flag == "Incumbent"))

  if (nrow(candidates) == 0) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        error = paste0("No candidate for cycle ", cycle)
      ),
      summary = tibble(
        source = "No Match",
        amount_usd = NA_real_,
        note = paste0("No candidate found for cycle ", cycle, ".")
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  requested_cycle <- cycle
  candidate_id <- NA_character_
  principal_committees <- character()
  try_totals <- function(cycle_value) {
    if (is.null(cycle_value) || is.na(cycle_value)) {
      return(NULL)
    }
    totals_req <- request(paste0("https://api.open.fec.gov/v1/candidate/", candidate_id, "/totals/")) |>
      req_error(is_error = function(resp) FALSE) |>
      req_url_query(
        cycle = cycle_value,
        api_key = api_key,
        per_page = 1
      )
    totals_resp <- tryCatch(req_perform(totals_req), error = function(e) e)
    if (inherits(totals_resp, "error")) {
      return(list(error = totals_resp$message))
    }
    if (resp_status(totals_resp) >= 400) {
      body_text <- tryCatch(resp_body_string(totals_resp), error = function(e) "")
      if (nchar(body_text) > 160) {
        body_text <- paste0(substr(body_text, 1, 160), "…")
      }
      # Fallback to plural endpoint if needed
      totals_req2 <- request("https://api.open.fec.gov/v1/candidates/totals/") |>
        req_error(is_error = function(resp) FALSE) |>
        req_url_query(
          candidate_id = candidate_id,
          cycle = cycle_value,
          api_key = api_key,
          per_page = 1
        )
      totals_resp2 <- tryCatch(req_perform(totals_req2), error = function(e) e)
      if (!inherits(totals_resp2, "error") && resp_status(totals_resp2) < 400) {
        totals_json2 <- resp_body_json(totals_resp2, simplifyVector = TRUE)
        return(list(json = totals_json2))
      }
      body_text2 <- if (!inherits(totals_resp2, "error")) {
        tryCatch(resp_body_string(totals_resp2), error = function(e) "")
      } else {
        totals_resp2$message
      }
      if (nchar(body_text2) > 160) {
        body_text2 <- paste0(substr(body_text2, 1, 160), "…")
      }
      return(list(error = paste0("HTTP ", resp_status(totals_resp), ": ", body_text)))
    }
    totals_json <- resp_body_json(totals_resp, simplifyVector = TRUE)
    list(json = totals_json)
  }

  totals_json <- NULL
  totals_error <- NULL
  cycle_used <- NA_integer_
  using_committee_totals <- FALSE
  committee_id_used <- NA_character_
  for (i in seq_len(nrow(candidates))) {
    candidate_id_try <- sanitize_candidate_id(candidates$candidate_id[[i]])
    if (is.na(candidate_id_try)) {
      totals_error <- "Invalid candidate_id returned from search"
      next
    }
    years <- integer()
    if (!is.null(candidates$election_years[[i]]) && length(candidates$election_years[[i]]) > 0) {
      years <- suppressWarnings(as.integer(candidates$election_years[[i]]))
      years <- years[!is.na(years)]
    }
    candidate_cycle <- requested_cycle
    if (is.null(candidate_cycle) || is.na(candidate_cycle)) {
      if (length(years) > 0) {
        candidate_cycle <- max(years)
      }
    }
    committees_try <- extract_principal_committee_ids(candidates[i, ])
    if (length(committees_try) == 0) {
      committees_try <- openfec_candidate_committees(candidate_id_try, api_key, cycle = candidate_cycle)
    }
    cycles_to_try <- unique(na.omit(c(
      requested_cycle,
      if (length(years) > 0) sort(years, decreasing = TRUE) else NULL,
      if (!is.null(requested_cycle) && !is.na(requested_cycle)) c(requested_cycle - 2, requested_cycle - 4) else NULL
    )))
    candidate_id <- candidate_id_try
    principal_committees <- committees_try
    for (cyc in cycles_to_try) {
      attempt <- try_totals(cyc)
      if (is.null(attempt)) {
        next
      }
      if (!is.null(attempt$json)) {
        totals_json <- attempt$json
        cycle_used <- cyc
        break
      }
      totals_error <- attempt$error
    }
    if (!is.null(totals_json)) {
      break
    }
    if (!is.null(totals_error) && grepl("Invalid candidate_id", totals_error, fixed = TRUE)) {
      # Try committee totals as a fallback when candidate totals are rejected
      if (length(principal_committees) > 0) {
        committee_id_try <- principal_committees[[1]]
        for (cyc in cycles_to_try) {
          comm_attempt <- openfec_committee_totals(committee_id_try, cyc, api_key)
          if (!is.null(comm_attempt$json)) {
            totals_json <- comm_attempt$json
            cycle_used <- cyc
            using_committee_totals <- TRUE
            committee_id_used <- committee_id_try
            break
          }
          totals_error <- comm_attempt$error
        }
      }
    }
  }

  if (is.null(totals_json)) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = candidate_id,
        cycle_used = ifelse(is.na(cycle_used), requested_cycle, cycle_used),
        principal_committees = principal_committees,
        candidate_options = candidates_debug,
        error = totals_error
      ),
      summary = tibble(
        source = "API Error",
        amount_usd = NA_real_,
        note = paste("OpenFEC totals request failed.", totals_error)
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }
  if (length(totals_json$results) == 0) {
    return(list(
      diagnostics = list(
        api_key_present = TRUE,
        candidate_id = candidate_id,
        cycle_used = cycle_used,
        principal_committees = principal_committees,
        candidate_options = candidates_debug,
        error = "No totals results"
      ),
      summary = tibble(
        source = "No Totals",
        amount_usd = NA_real_,
        note = "No totals returned for candidate."
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    ))
  }

  res <- totals_json$results[1, ]
  cycle_note <- if (using_committee_totals) {
    if (!is.null(cycle_used) && !is.na(cycle_used)) {
      paste("Committee ID:", committee_id_used, "(cycle used:", cycle_used, ")")
    } else {
      paste("Committee ID:", committee_id_used)
    }
  } else {
    if (!is.null(cycle_used) && !is.na(cycle_used)) {
      paste("Candidate ID:", candidate_id, "(cycle used:", cycle_used, ")")
    } else {
      paste("Candidate ID:", candidate_id)
    }
  }

  receipts_individuals <- pick_first_numeric(res, c(
    "receipts_from_individuals",
    "individual_contributions",
    "individual_itemized_contributions",
    "individual_unitemized_contributions"
  ))
  receipts_pacs <- pick_first_numeric(res, c(
    "receipts_from_pacs",
    "other_political_committee_contributions",
    "contributions_from_other_political_committees",
    "party_committee_contributions"
  ))
  total_receipts <- pick_first_numeric(res, c(
    "receipts",
    "total_receipts",
    "adjusted_receipts"
  ))

  summary <- tibble(
    source = c("Individuals", "PACs", "Total receipts"),
    amount_usd = c(
      receipts_individuals,
      receipts_pacs,
      total_receipts
    ),
    note = cycle_note
  )

  donor_types <- summary |>
    filter(source %in% c("Individuals", "PACs")) |>
    transmute(donor_type = source, amount_usd = amount_usd, note = "")

  industries <- tibble(
    industry = character(),
    amount_usd = numeric(),
    note = character()
  )

  occupations <- tibble(
    occupation = character(),
    amount_usd = numeric(),
    note = character()
  )

  if (length(principal_committees) > 0) {
    by_employer <- openfec_schedule_a_by_employer(principal_committees, cycle_used, api_key)
    if (!is.null(by_employer) && nrow(by_employer) > 0) {
      industries <- by_employer |>
        transmute(industry = employer, amount_usd = amount_usd, note = note)
    }
    by_occupation <- openfec_schedule_a_by_occupation(principal_committees, cycle_used, api_key)
    if (!is.null(by_occupation) && nrow(by_occupation) > 0) {
      occupations <- by_occupation |>
        transmute(occupation = occupation, amount_usd = amount_usd, note = note)
    }
    pacs_all <- openfec_schedule_a_top_committees(principal_committees, cycle_used, api_key)
    pacs_internal <- openfec_schedule_a_internal_transfers(principal_committees, cycle_used, api_key)
  } else {
    industries <- tibble(
      industry = "No principal committee found for candidate.",
      amount_usd = NA_real_,
      note = ""
    )
    occupations <- tibble(
      occupation = "No principal committee found for candidate.",
      amount_usd = NA_real_,
      note = ""
    )
    pacs_all <- tibble(
      pac = character(),
      amount_usd = numeric(),
      note = "No principal committee found for candidate."
    )
    pacs_internal <- tibble(
      pac = character(),
      amount_usd = numeric(),
      note = "No principal committee found for candidate."
    )
  }

  if (nrow(pacs_all) > 0 && "amount_usd" %in% names(pacs_all)) {
    identifiable_total <- sum(pacs_all$amount_usd, na.rm = TRUE)
  } else {
    identifiable_total <- 0
  }
  if (!is.na(receipts_pacs) && receipts_pacs > identifiable_total) {
    pacs_all <- bind_rows(
      pacs_all,
      tibble(
        pac = "Other (unitemized/unknown)",
        amount_usd = receipts_pacs - identifiable_total,
        note = ""
      )
    )
  }

  trend <- openfec_receipts_trend(candidate_id, cycle_used, api_key)
  if (nrow(trend) == 0) {
    trend <- tibble(
      period = "No receipt history returned.",
      amount_usd = NA_real_,
      note = ""
    )
  }

  list(
    diagnostics = list(
      api_key_present = TRUE,
      candidate_id = candidate_id,
      cycle_used = cycle_used,
      principal_committees = principal_committees
    ),
    summary = summary,
    donor_types = donor_types,
    industries = industries,
    occupations = occupations,
    pacs_all = pacs_all,
    pacs_internal = pacs_internal,
    trend = trend
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
