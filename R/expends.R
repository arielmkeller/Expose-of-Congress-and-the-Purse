library(readr)
library(dplyr)
library(tibble)

expends_cache_env <- new.env(parent = emptyenv())
expends_cache_path <- file.path("data", "expends22_cache.rds")

load_expends_cache <- function() {
  if (file.exists(expends_cache_path)) {
    obj <- tryCatch(readRDS(expends_cache_path), error = function(e) NULL)
    if (is.list(obj)) {
      list2env(obj, envir = expends_cache_env)
    }
  }
}

save_expends_cache <- function() {
  keys <- ls(envir = expends_cache_env, all.names = TRUE)
  if (length(keys) == 0) {
    return(invisible(NULL))
  }
  obj <- mget(keys, envir = expends_cache_env, inherits = FALSE)
  dir.create(dirname(expends_cache_path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(obj, expends_cache_path), error = function(e) NULL)
}

expends_make_cache_key <- function(committee_ids, path) {
  mtime <- if (file.exists(path)) file.info(path)$mtime else NA
  paste0("expends22|", as.character(mtime), "|", paste(sort(unique(committee_ids)), collapse = "|"))
}

expends_filter_lines <- function(committee_ids, path) {
  patterns <- paste0("|", committee_ids, "|")
  tmp <- tempfile(fileext = ".txt")

  rg <- Sys.which("rg")
  if (nzchar(rg)) {
    args <- c("-F")
    for (p in patterns) {
      args <- c(args, "-e", p)
    }
    args <- c(args, path)
    cmd <- paste(shQuote(rg), paste(vapply(args, shQuote, character(1))), ">", shQuote(tmp))
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(tmp)
  }

  args <- c("-F")
  for (p in patterns) {
    args <- c(args, "-e", p)
  }
  args <- c(args, path)
  cmd <- paste("grep", paste(vapply(args, shQuote, character(1))), ">", shQuote(tmp))
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  tmp
}

summarize_expends_by_purpose <- function(committee_ids, path) {
  if (!exists("expends_cache_loaded", envir = expends_cache_env, inherits = FALSE)) {
    load_expends_cache()
    assign("expends_cache_loaded", TRUE, envir = expends_cache_env)
  }

  cache_key <- expends_make_cache_key(committee_ids, path)
  if (exists(cache_key, envir = expends_cache_env, inherits = FALSE)) {
    return(get(cache_key, envir = expends_cache_env, inherits = FALSE))
  }

  filtered_path <- expends_filter_lines(committee_ids, path)
  on.exit(unlink(filtered_path), add = TRUE)

  col_names <- c(
    "cycle", "record_id", "image_num", "committee_id", "committee_type",
    "committee_name", "payee", "disbursement_code", "amount", "disbursement_date",
    "city", "state", "zip", "unk1", "unk2", "purpose", "unk3", "unk4",
    "unk5", "entity_type", "source"
  )

  col_types <- cols_only(
    committee_id = col_character(),
    payee = col_character(),
    amount = col_double(),
    disbursement_date = col_character(),
    purpose = col_character()
  )

  raw <- suppressMessages(
    read_delim(
      filtered_path,
      delim = ",",
      quote = "|",
      col_names = col_names,
      col_types = col_types,
      trim_ws = TRUE
    )
  )

  if (nrow(raw) == 0) {
    res <- tibble(message = "No matching Expend22 records found for those committees.")
    assign(cache_key, res, envir = expends_cache_env)
    save_expends_cache()
    return(res)
  }

  res <- raw |>
    mutate(
      purpose = ifelse(is.na(purpose) | trimws(purpose) == "", "Unspecified", purpose)
    ) |>
    group_by(purpose) |>
    summarise(
      transactions = n(),
      total_amount = sum(amount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(total_amount))

  assign(cache_key, res, envir = expends_cache_env)
  save_expends_cache()
  res
}

get_expends_for_legislator <- function(
  legislator,
  cycle = 2024,
  chamber = NA_character_,
  state = NULL,
  expends_path = "/Users/ariel/Desktop/Expose-of-Congress-and-the-Purse/527/expends527.txt"
) {
  if (!file.exists(expends_path)) {
    return(tibble(message = paste("Expend22 file not found at", expends_path)))
  }

  committees <- get_openfec_committees_for_legislator(
    legislator,
    cycle = cycle,
    chamber = chamber,
    state = state
  )

  if (!"committee_id" %in% names(committees) || nrow(committees) == 0) {
    note <- if ("note" %in% names(committees) && length(committees$note) > 0) {
      committees$note[[1]]
    } else {
      "No committees found for that legislator."
    }
    return(tibble(message = note))
  }

  committee_ids <- unique(committees$committee_id)
  summarize_expends_by_purpose(committee_ids, expends_path)
}
