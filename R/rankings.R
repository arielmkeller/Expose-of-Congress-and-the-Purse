library(dplyr)
library(tibble)

rank_cache_env <- new.env(parent = emptyenv())

rank_cache_path <- function(filename) {
  file.path("data", filename)
}

load_rank_cache <- function(filename, env_key) {
  path <- rank_cache_path(filename)
  if (exists(env_key, envir = rank_cache_env, inherits = FALSE)) {
    return(get(env_key, envir = rank_cache_env, inherits = FALSE))
  }
  if (!file.exists(path)) {
    return(NULL)
  }
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.null(obj)) {
    assign(env_key, obj, envir = rank_cache_env)
  }
  obj
}

save_rank_cache <- function(filename, env_key, value) {
  dir.create(dirname(rank_cache_path(filename)), showWarnings = FALSE, recursive = TRUE)
  assign(env_key, value, envir = rank_cache_env)
  tryCatch(saveRDS(value, rank_cache_path(filename)), error = function(e) NULL)
}

format_rank <- function(rank_val, total_val) {
  if (is.na(rank_val) || is.na(total_val) || total_val == 0) {
    return("Unavailable")
  }
  paste0(rank_val, " of ", total_val)
}

format_money <- function(val) {
  if (is.na(val)) {
    return("Unavailable")
  }
  paste0("$", formatC(val, format = "f", digits = 0, big.mark = ","))
}

earmark_totals_cache_key <- "earmark_totals"

get_earmark_totals_by_legislator <- function(legislators_ref) {
  path <- "fy2_All_Earmarks.xlsx"
  if (exists("get_project_root", mode = "function")) {
    root_path <- file.path(get_project_root(), "fy2_All_Earmarks.xlsx")
    if (file.exists(root_path)) {
      path <- root_path
    }
  }
  if (file.exists(path)) {
    data_mtime <- file.info(path)$mtime
  } else {
    data_mtime <- NA
  }
  cached <- load_rank_cache("earmark_totals_cache.rds", earmark_totals_cache_key)
  if (!is.null(cached) && !is.null(cached$mtime) && identical(cached$mtime, data_mtime)) {
    return(cached$data)
  }

  if (!file.exists(path)) {
    return(tibble())
  }

  by_lastname <- get_earmark_totals_by_lastname()
  if (nrow(by_lastname) == 0) {
    return(tibble())
  }

  last_names <- vapply(legislators_ref$legislator, function(name) {
    cands <- build_name_candidates(name)
    if (length(cands) == 0) return(NA_character_)
    cands[[1]]
  }, character(1))

  totals <- legislators_ref |>
    mutate(last_name = last_names) |>
    left_join(by_lastname, by = c("last_name" = "last_name")) |>
    transmute(
      legislator = legislator,
      state = state,
      chamber = chamber,
      total_earmarks = total_amount
    )

  payload <- list(
    mtime = data_mtime,
    data = totals
  )
  save_rank_cache("earmark_totals_cache.rds", earmark_totals_cache_key, payload)
  totals
}

get_earmark_ranks_for_member <- function(member, legislators_ref) {
  totals <- get_earmark_totals_by_legislator(legislators_ref)
  if (nrow(totals) == 0) {
    return(list(total = NA_real_, state_rank = NA_integer_, state_total = NA_integer_, national_rank = NA_integer_, national_total = NA_integer_))
  }
  name <- member$legislator[[1]]
  member_state <- member$state[[1]]
  total_val <- totals$total_earmarks[totals$legislator == name]
  total_val <- if (length(total_val) == 0) NA_real_ else total_val[[1]]

  state_totals <- totals |>
    filter(state == member_state, !is.na(total_earmarks))
  national_totals <- totals |>
    filter(!is.na(total_earmarks))

  state_rank <- NA_integer_
  national_rank <- NA_integer_
  if (!is.na(total_val) && nrow(state_totals) > 0) {
    state_rank <- rank(-state_totals$total_earmarks, ties.method = "min")[state_totals$legislator == name][[1]]
  }
  if (!is.na(total_val) && nrow(national_totals) > 0) {
    national_rank <- rank(-national_totals$total_earmarks, ties.method = "min")[national_totals$legislator == name][[1]]
  }

  list(
    total = total_val,
    state_rank = state_rank,
    state_total = nrow(state_totals),
    national_rank = national_rank,
    national_total = nrow(national_totals)
  )
}

campaign_cache_key <- "campaign_receipts"

load_campaign_receipts_cache <- function() {
  cached <- load_rank_cache("campaign_receipts_cache.rds", campaign_cache_key)
  if (is.null(cached)) {
    cached <- list(data = tibble())
  }
  cached
}

save_campaign_receipts_cache <- function(data) {
  payload <- list(
    updated = Sys.time(),
    data = data
  )
  save_rank_cache("campaign_receipts_cache.rds", campaign_cache_key, payload)
}

extract_total_receipts <- function(finance) {
  if (is.null(finance) || is.null(finance$summary) || nrow(finance$summary) == 0) {
    return(NA_real_)
  }
  total <- finance$summary |>
    filter(source == "Total receipts") |>
    summarize(total = sum(amount_usd, na.rm = TRUE)) |>
    pull(total)
  if (length(total) == 0 || is.na(total)) {
    total <- sum(finance$summary$amount_usd, na.rm = TRUE)
  }
  as.numeric(total)
}

compute_campaign_receipts_totals <- function(legislators_ref, cycle = 2024) {
  cached <- load_campaign_receipts_cache()
  existing <- cached$data
  if (is.null(existing) || nrow(existing) == 0) {
    existing <- tibble()
  }

  rows <- lapply(seq_len(nrow(legislators_ref)), function(i) {
    row <- legislators_ref[i, ]
    key <- paste(row$legislator, cycle, row$chamber, row$state, row$district, sep = "|")
    if (nrow(existing) > 0 && key %in% existing$key) {
      return(existing[existing$key == key, ] |> slice(1))
    }
    finance <- get_openfec_summary(row$legislator, cycle = cycle, chamber = row$chamber, state = row$state, district = row$district)
    tibble(
      key = key,
      legislator = row$legislator,
      state = row$state,
      chamber = row$chamber,
      cycle = cycle,
      total_receipts = extract_total_receipts(finance)
    )
  })

  out <- bind_rows(rows)
  save_campaign_receipts_cache(out)
  out
}

get_campaign_ranks_for_member <- function(member, legislators_ref, cycle = 2024, totals = NULL, min_national = 50, min_state = 2) {
  if (is.null(totals)) {
    totals <- load_campaign_receipts_cache()$data
  }
  if (is.null(totals) || nrow(totals) == 0) {
    return(list(total = NA_real_, state_rank = NA_integer_, state_total = NA_integer_, national_rank = NA_integer_, national_total = NA_integer_))
  }
  name <- member$legislator[[1]]
  member_state <- member$state[[1]]
  total_val <- totals$total_receipts[totals$legislator == name]
  total_val <- if (length(total_val) == 0) NA_real_ else total_val[[1]]

  state_totals <- totals |>
    filter(state == member_state, !is.na(total_receipts))
  national_totals <- totals |>
    filter(!is.na(total_receipts))

  state_rank <- NA_integer_
  national_rank <- NA_integer_
  if (!is.na(total_val) && nrow(state_totals) >= min_state) {
    state_rank <- rank(-state_totals$total_receipts, ties.method = "min")[state_totals$legislator == name][[1]]
  }
  if (!is.na(total_val) && nrow(national_totals) >= min_national) {
    national_rank <- rank(-national_totals$total_receipts, ties.method = "min")[national_totals$legislator == name][[1]]
  }

  list(
    total = total_val,
    state_rank = state_rank,
    state_total = nrow(state_totals),
    national_rank = national_rank,
    national_total = nrow(national_totals)
  )
}

state_spending_cache_key <- "state_spending_totals"

get_state_spending_totals <- function() {
  cached <- load_rank_cache("state_spending_totals_cache.rds", state_spending_cache_key)
  if (!is.null(cached) && !is.null(cached$data)) {
    return(cached$data)
  }

  rows <- lapply(state.abb, function(st) {
    res <- get_usaspending_context(state = st, district = NULL)
    tibble(
      state = st,
      total_spending = res$total_amount
    )
  })
  out <- bind_rows(rows)
  payload <- list(
    updated = Sys.time(),
    data = out
  )
  save_rank_cache("state_spending_totals_cache.rds", state_spending_cache_key, payload)
  out
}

get_state_spending_rank <- function(state_abbrev) {
  totals <- get_state_spending_totals()
  if (nrow(totals) == 0) {
    return(list(total = NA_real_, rank = NA_integer_, total_states = NA_integer_))
  }
  totals <- totals |>
    filter(!is.na(total_spending))
  total_val <- totals$total_spending[totals$state == state_abbrev]
  total_val <- if (length(total_val) == 0) NA_real_ else total_val[[1]]
  rank_val <- NA_integer_
  if (!is.na(total_val) && nrow(totals) > 0) {
    rank_val <- rank(-totals$total_spending, ties.method = "min")[totals$state == state_abbrev][[1]]
  }
  list(total = total_val, rank = rank_val, total_states = nrow(totals))
}
