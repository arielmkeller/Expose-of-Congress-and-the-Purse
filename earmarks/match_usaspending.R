#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(httr2)
  library(jsonlite)
  library(stringdist)
  library(tibble)
})

clean_key <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\\p{Cc}\\p{Cf}]", "")
  x <- tolower(x)
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_squish(x)
  x
}

fy_to_range <- function(fy) {
  # Fiscal year runs Oct 1 (prior year) to Sep 30 (FY)
  start <- as.Date(sprintf("%d-10-01", fy - 1))
  end <- as.Date(sprintf("%d-09-30", fy))
  list(start_date = format(start, "%Y-%m-%d"), end_date = format(end, "%Y-%m-%d"))
}

score_matches <- function(df, earmark_row) {
  if (nrow(df) == 0) return(df)
  recip_clean <- clean_key(earmark_row$recipient_name)
  state <- toupper(trimws(earmark_row$state))
  fy <- earmark_row$fiscal_year

  df <- df %>%
    mutate(
      recipient_clean = clean_key(recipient_name),
      recip_sim = ifelse(is.na(recipient_clean) | recip_clean == "", 0,
                         stringsim(recip_clean, recipient_clean, method = "jw")),
      state_match = ifelse(!is.na(state) & state != "" & (recipient_state == state | place_of_performance_state == state), 1, 0),
      year_diff = ifelse(!is.na(award_year), abs(award_year - fy), NA_real_),
      year_score = ifelse(is.na(year_diff), 0, pmax(0, 1 - (year_diff / 2)))
    ) %>%
    mutate(match_score = 0.7 * recip_sim + 0.2 * state_match + 0.1 * year_score)

  df
}

request_awards <- function(recipient, state, fy, limit = 3, pause = 0.2) {
  if (is.na(recipient) || recipient == "") {
    return(list(status = "missing_recipient", results = tibble(), error = NA_character_))
  }

  fy1 <- fy_to_range(fy)
  fy2 <- fy_to_range(fy + 1)

  base_filters <- list(
    recipient_search_text = list(recipient),
    time_period = list(
      modifyList(fy1, list(date_type = "action_date")),
      modifyList(fy2, list(date_type = "action_date"))
    )
  )

  fields_primary <- c(
    "Award ID",
    "Recipient Name",
    "Recipient State Code",
    "Place of Performance State Code",
    "Start Date",
    "Award Amount",
    "Awarding Agency"
  )

  body_with_state <- NULL
  if (!is.na(state) && state != "") {
    body_with_state <- list(
      filters = modifyList(base_filters, list(
        place_of_performance_scope = "domestic",
        place_of_performance_locations = list(list(country = "USA", state = state)),
        recipient_locations = list(list(country = "USA", state = state))
      )),
      fields = fields_primary,
      limit = limit
    )
  }

  body_no_state <- list(
    filters = base_filters,
    fields = fields_primary,
    limit = limit
  )

  attempt <- function(body) {
    req <- request("https://api.usaspending.gov/api/v2/search/spending_by_award/") %>%
      req_method("POST") %>%
      req_user_agent("Expose-of-Congress-and-the-Purse/1.0") %>%
      req_headers(`Content-Type` = "application/json") %>%
      req_body_json(body, auto_unbox = TRUE)

    resp <- tryCatch(req_perform(req), error = function(e) e)
    Sys.sleep(pause)

    if (inherits(resp, "error")) {
      return(list(ok = FALSE, error = resp$message, payload = NULL, status = NA_integer_))
    }
    status <- resp_status(resp)
    if (status >= 400) {
      return(list(ok = FALSE, error = resp_body_string(resp), payload = NULL, status = status))
    }

    payload <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
    list(ok = TRUE, error = NA_character_, payload = payload, status = status)
  }

  # Try with state filter first, then without
  resp1 <- if (!is.null(body_with_state)) attempt(body_with_state) else list(ok = FALSE)
  if (!resp1$ok) {
    resp2 <- attempt(body_no_state)
    if (!resp2$ok) {
      return(list(status = "api_error", results = tibble(), error = resp2$error))
    }
    payload <- resp2$payload
    state_used <- FALSE
  } else {
    payload <- resp1$payload
    state_used <- TRUE
  }

  results <- payload$results
  if (is.null(results) || length(results) == 0) {
    return(list(status = ifelse(state_used, "no_results_with_state", "no_results"), results = tibble(), error = NA_character_))
  }

  df <- as_tibble(results)

  # Normalize column names from API field labels
  rename_map <- c(
    "Award ID" = "award_id",
    "Recipient Name" = "recipient_name",
    "Recipient State Code" = "recipient_state",
    "Place of Performance State Code" = "place_of_performance_state",
    "Start Date" = "start_date",
    "Award Amount" = "award_amount",
    "Awarding Agency" = "awarding_agency"
  )

  for (nm in names(rename_map)) {
    if (nm %in% names(df)) {
      names(df)[names(df) == nm] <- rename_map[[nm]]
    }
  }

  df <- df %>%
    mutate(
      award_year = ifelse(!is.na(start_date) & start_date != "", as.integer(substr(start_date, 1, 4)), NA_integer_),
      award_amount = as.numeric(award_amount)
    )

  list(status = "ok", results = df, error = NA_character_, state_used = state_used)
}

main <- function() {
  input_path <- "earmarks/earmarks_master_ready.csv"
  output_path <- "earmarks/earmarks_with_awards.csv"
  log_path <- "earmarks/usaspending_match_log.csv"
  if (!file.exists(input_path)) stop("Missing: ", input_path)

  earmarks <- read_csv(input_path, show_col_types = FALSE)

  if (!"project_id" %in% names(earmarks) || all(is.na(earmarks$project_id) | earmarks$project_id == "")) {
    earmarks <- earmarks %>%
      mutate(project_id = sprintf("earmark_%06d", row_number()))
  }

  earmarks <- earmarks %>%
    mutate(
      recipient_name = as.character(recipient_name),
      state = as.character(state),
      fiscal_year = as.integer(fiscal_year)
    )

  max_rows <- as.integer(Sys.getenv("MAX_ROWS", "0"))
  run_full <- tolower(Sys.getenv("RUN_FULL", "0")) %in% c("1", "true", "yes")
  if (is.na(max_rows)) max_rows <- 0
  workset <- if (max_rows > 0) earmarks %>% slice(1:max_rows) else earmarks

  # Test on first 20 rows
  test_rows <- earmarks %>% filter(!is.na(recipient_name) & recipient_name != "") %>% slice(1:20)
  message("Testing first 20 rows...")
  test_results <- map_dfr(seq_len(nrow(test_rows)), function(i) {
    row <- test_rows[i, ]
    resp <- request_awards(row$recipient_name, row$state, row$fiscal_year)
    if (nrow(resp$results) == 0) {
      return(tibble(
        project_id = row$project_id,
        match_status = resp$status,
        match_score = NA_real_
      ))
    }
    scored <- score_matches(resp$results, row)
    top <- scored %>% arrange(desc(match_score)) %>% slice(1)
    tibble(
      project_id = row$project_id,
      match_status = resp$status,
      match_score = top$match_score[[1]]
    )
  })
  message("Test complete. Matched: ", sum(test_results$match_status == "ok"))

  if (!run_full) {
    message("RUN_FULL not set. Skipping full dataset run.")
    return(invisible(NULL))
  }

  # Full run
  message("Running full dataset...")
  log_rows <- list()
  full_results <- map_dfr(seq_len(nrow(workset)), function(i) {
    row <- workset[i, ]
    resp <- request_awards(row$recipient_name, row$state, row$fiscal_year)

    if (nrow(resp$results) == 0) {
      log_rows[[length(log_rows) + 1]] <<- tibble(
        project_id = row$project_id,
        recipient_name = row$recipient_name,
        state = row$state,
        fiscal_year = row$fiscal_year,
        status = resp$status,
        error = resp$error
      )
      return(tibble(
        project_id = row$project_id,
        award_id = NA_character_,
        award_amount = NA_real_,
        award_year = NA_integer_,
        awarding_agency = NA_character_,
        match_score = NA_real_,
        match_status = resp$status
      ))
    }

    scored <- score_matches(resp$results, row)
    top <- scored %>% arrange(desc(match_score)) %>% slice(1)

    log_rows[[length(log_rows) + 1]] <<- tibble(
      project_id = row$project_id,
      recipient_name = row$recipient_name,
      state = row$state,
      fiscal_year = row$fiscal_year,
      status = "matched",
      error = NA_character_
    )

    tibble(
      project_id = row$project_id,
      award_id = top$award_id[[1]],
      award_amount = top$award_amount[[1]],
      award_year = top$award_year[[1]],
      awarding_agency = top$awarding_agency[[1]],
      match_score = top$match_score[[1]],
      match_status = "matched"
    )
  })

  out <- workset %>%
    left_join(full_results, by = "project_id") %>%
    select(
      project_id,
      fiscal_year,
      member_name,
      state,
      district,
      recipient_name,
      project_description,
      amount_requested,
      award_id,
      award_amount,
      award_year,
      awarding_agency,
      match_score,
      match_status
    )

  write_csv(out, output_path, na = "")
  if (length(log_rows) > 0) {
    write_csv(bind_rows(log_rows), log_path, na = "")
  }
  message("Wrote ", output_path)
}

main()
