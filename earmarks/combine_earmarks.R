#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(readr)
  library(purrr)
  library(tibble)
})

get_script_dir <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  fileArg <- sub("^--file=", "", cmdArgs[grepl("^--file=", cmdArgs)])
  if (length(fileArg) > 0) {
    return(dirname(normalizePath(fileArg)))
  }
  getwd()
}

folder <- get_script_dir()
files <- sort(list.files(folder, pattern = "\\.xls[x]?$", full.names = TRUE))

if (length(files) == 0) {
  stop("No Excel files found in ", folder)
}

find_cols <- function(nms, patterns) {
  hits <- nms[Reduce(`|`, lapply(patterns, function(p) str_detect(nms, regex(p, ignore_case = TRUE))))]
  hits
}

pick_col <- function(nms, patterns) {
  for (p in patterns) {
    hits <- nms[str_detect(nms, regex(p, ignore_case = TRUE))]
    if (length(hits) > 0) {
      return(list(col = hits[1], hits = hits))
    }
  }
  return(list(col = NA_character_, hits = character(0)))
}

extract_fiscal_year <- function(filename, colnames_lower) {
  m <- str_match(filename, regex("fy\\s*([0-9]{2,4})", ignore_case = TRUE))
  if (!is.na(m[1,2])) {
    yr <- m[1,2]
    if (nchar(yr) == 2) return(list(year = 2000 + as.integer(yr), source = "filename"))
    return(list(year = as.integer(yr), source = "filename"))
  }
  if (any(str_detect(colnames_lower, "fy22"))) return(list(year = 2022L, source = "column_name"))
  return(list(year = NA_integer_, source = "unknown"))
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\\p{Cc}\\p{Cf}]", "")
  x <- str_squish(x)
  x
}

clean_key <- function(x) {
  x <- clean_text(x)
  x <- tolower(x)
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_squish(x)
  x
}

records <- list()
notes <- list(files = list(), mappings = list(), ambiguities = list(), unmatched = list(), rows_dropped = list(), year_notes = list(), amount_parse = list(), year_source = list())

for (f in files) {
  sheet <- excel_sheets(f)[1]
  df_raw <- read_excel(f, sheet = sheet, col_types = "text")
  orig_names <- names(df_raw)
  nms <- tolower(orig_names)

  # Identify columns
  member_first_cols <- find_cols(orig_names, c("member first"))
  member_last_cols  <- find_cols(orig_names, c("member last"))
  member_cols       <- find_cols(orig_names, c("^member$"))
  district_cols     <- find_cols(orig_names, c("district"))
  state_cols        <- find_cols(orig_names, c("state"))
  subcomm_cols      <- find_cols(orig_names, c("subcommittee"))
  recipient_pick    <- pick_col(orig_names, c("^recipient$"))
  proj_pick         <- pick_col(orig_names, c("^project purpose$", "^program/language/project title$", "^request$", "^explanation$"))
  amount_cols       <- find_cols(orig_names, c("amount requested"))

  # Ambiguities
  amb <- list()
  if (length(member_cols) > 1) amb$member <- member_cols
  if (length(member_first_cols) > 1) amb$member_first <- member_first_cols
  if (length(member_last_cols) > 1) amb$member_last <- member_last_cols
  if (length(district_cols) > 1) amb$district <- district_cols
  if (length(state_cols) > 1) amb$state <- state_cols
  if (length(subcomm_cols) > 1) amb$subcommittee <- subcomm_cols
  if (length(recipient_pick$hits) > 1) amb$recipient <- recipient_pick$hits
  if (length(proj_pick$hits) > 1) amb$project <- proj_pick$hits
  if (length(amount_cols) > 1) amb$amount <- amount_cols
  if (length(amb) > 0) notes$ambiguities[[basename(f)]] <- amb

  # Choose first match if multiple
  member_first <- if (length(member_first_cols) > 0) member_first_cols[1] else NA_character_
  member_last  <- if (length(member_last_cols)  > 0) member_last_cols[1]  else NA_character_
  member_col   <- if (length(member_cols)       > 0) member_cols[1]       else NA_character_
  district_col <- if (length(district_cols)     > 0) district_cols[1]     else NA_character_
  state_col    <- if (length(state_cols)        > 0) state_cols[1]        else NA_character_
  subcomm_col  <- if (length(subcomm_cols)      > 0) subcomm_cols[1]      else NA_character_
  recipient_col<- recipient_pick$col
  proj_col     <- proj_pick$col
  amount_col   <- if (length(amount_cols)       > 0) amount_cols[1]       else NA_character_

  # Build standardized fields
  member_name <- NULL
  if (!is.na(member_first) && !is.na(member_last)) {
    member_name <- str_c(df_raw[[member_first]], df_raw[[member_last]], sep = " ")
  } else if (!is.na(member_col)) {
    member_name <- df_raw[[member_col]]
  } else {
    member_name <- NA_character_
  }

  district <- if (!is.na(district_col)) df_raw[[district_col]] else NA_character_
  state <- if (!is.na(state_col)) df_raw[[state_col]] else NA_character_
  subcommittee <- if (!is.na(subcomm_col)) df_raw[[subcomm_col]] else NA_character_
  recipient_name_raw <- if (!is.na(recipient_col)) df_raw[[recipient_col]] else NA_character_
  project_description <- if (!is.na(proj_col)) df_raw[[proj_col]] else NA_character_
  amount_requested_raw <- if (!is.na(amount_col)) df_raw[[amount_col]] else NA_character_

  # Clean
  member_name <- clean_text(member_name)
  district <- clean_text(district)
  state <- clean_text(state)
  subcommittee <- clean_text(subcommittee)
  project_description <- clean_text(project_description)
  recipient_name_raw <- clean_text(recipient_name_raw)
  recipient_name <- clean_text(recipient_name_raw)
  amount_requested <- parse_number(amount_requested_raw)
  parse_fail <- sum(!is.na(amount_requested_raw) & amount_requested_raw != "" & is.na(amount_requested))
  if (parse_fail > 0) {
    bad_vals <- unique(amount_requested_raw[!is.na(amount_requested_raw) & amount_requested_raw != "" & is.na(amount_requested)])
    notes$amount_parse[[basename(f)]] <- list(count = parse_fail, examples = head(bad_vals, 5))
  } else {
    notes$amount_parse[[basename(f)]] <- list(count = 0, examples = character(0))
  }

  colnames_lower <- tolower(orig_names)
  fy <- extract_fiscal_year(basename(f), colnames_lower)
  fiscal_year <- fy$year
  notes$year_source[[basename(f)]] <- fy$source

  # Note year resolution
  if (is.na(fiscal_year)) {
    notes$year_notes[[basename(f)]] <- "Fiscal year not found in filename; set to NA"
  }

  df_std <- tibble(
    member_name = member_name,
    state = state,
    district = district,
    subcommittee = subcommittee,
    recipient_name = recipient_name,
    recipient_name_raw = recipient_name_raw,
    project_description = project_description,
    amount_requested = amount_requested,
    fiscal_year = fiscal_year
  )

  # Drop rows where all core fields are empty
  core_cols <- c("member_name", "recipient_name", "project_description", "amount_requested")
  is_all_empty <- df_std %>%
    mutate(across(all_of(core_cols), ~ is.na(.) | . == "")) %>%
    transmute(all_empty = if_all(all_of(core_cols), identity)) %>%
    pull(all_empty)
  dropped <- sum(is_all_empty, na.rm = TRUE)
  df_std <- df_std[!is_all_empty, , drop = FALSE]

  # Store notes
  notes$files[[basename(f)]] <- sheet
  notes$rows_dropped[[basename(f)]] <- dropped

  mapping <- list(
    member_name = list(from = c(member_first = member_first, member_last = member_last, member = member_col)),
    state = state_col,
    district = district_col,
    subcommittee = subcomm_col,
    recipient_name = recipient_col,
    project_description = proj_col,
    amount_requested = amount_col
  )
  notes$mappings[[basename(f)]] <- mapping

  matched_cols <- unique(na.omit(c(member_first, member_last, member_col, state_col, district_col, subcomm_col, recipient_col, proj_col, amount_col)))
  unmatched <- setdiff(orig_names, matched_cols)
  notes$unmatched[[basename(f)]] <- unmatched

  records[[basename(f)]] <- df_std
}

combined <- bind_rows(records)

# Add join-ready fields
combined <- combined %>%
  mutate(
    project_id = NA_character_,
    recipient_clean = clean_key(recipient_name),
    project_clean = clean_key(project_description),
    member_clean = clean_key(member_name)
  )

# Write outputs
out_csv <- file.path(folder, "earmarks_master.csv")
write_csv(combined, out_csv, na = "")

out_ready <- file.path(folder, "earmarks_master_ready.csv")
write_csv(combined, out_ready, na = "")

# Write notes
notes_path <- file.path(folder, "data_notes.md")
con <- file(notes_path, open = "wt")
writeLines("# Earmark Data Notes", con)
writeLines("", con)
writeLines("## Files Read", con)
for (nm in names(notes$files)) {
  writeLines(paste0("- ", nm, " | sheet: ", notes$files[[nm]]), con)
}
writeLines("", con)
writeLines("## Column Mapping", con)
for (nm in names(notes$mappings)) {
  writeLines(paste0("- ", nm), con)
  m <- notes$mappings[[nm]]
  writeLines(paste0("  member_name: ", paste(na.omit(unlist(m$member_name$from)), collapse = ", ")), con)
  writeLines(paste0("  state: ", m$state), con)
  writeLines(paste0("  district: ", m$district), con)
  writeLines(paste0("  subcommittee: ", m$subcommittee), con)
  writeLines(paste0("  recipient_name: ", m$recipient_name), con)
  writeLines(paste0("  project_description: ", m$project_description), con)
  writeLines(paste0("  amount_requested: ", m$amount_requested), con)
}
writeLines("", con)
writeLines("## Unmatched Columns", con)
for (nm in names(notes$unmatched)) {
  um <- notes$unmatched[[nm]]
  if (length(um) == 0) {
    writeLines(paste0("- ", nm, ": none"), con)
  } else {
    writeLines(paste0("- ", nm, ": ", paste(um, collapse = "; ")), con)
  }
}
writeLines("", con)
writeLines("## Ambiguities", con)
if (length(notes$ambiguities) == 0) {
  writeLines("- none", con)
} else {
  for (nm in names(notes$ambiguities)) {
    writeLines(paste0("- ", nm), con)
    amb <- notes$ambiguities[[nm]]
    for (k in names(amb)) {
      writeLines(paste0("  ", k, ": ", paste(amb[[k]], collapse = ", ")), con)
    }
  }
}
writeLines("", con)
writeLines("## Rows Dropped", con)
for (nm in names(notes$rows_dropped)) {
  writeLines(paste0("- ", nm, ": ", notes$rows_dropped[[nm]]), con)
}
writeLines("", con)
writeLines("## Fiscal Year", con)
for (nm in names(notes$files)) {
  src <- notes$year_source[[nm]]
  if (!is.null(notes$year_notes[[nm]])) {
    writeLines(paste0("- ", nm, ": ", notes$year_notes[[nm]]), con)
  } else {
    writeLines(paste0("- ", nm, ": fiscal_year from ", src), con)
  }
}
writeLines("", con)
writeLines("## Amount Parse Issues", con)
for (nm in names(notes$amount_parse)) {
  ap <- notes$amount_parse[[nm]]
  if (ap$count == 0) {
    writeLines(paste0("- ", nm, ": none"), con)
  } else {
    writeLines(paste0("- ", nm, ": ", ap$count, " non-numeric values. Examples: ", paste(ap$examples, collapse = ", ")), con)
  }
}
close(con)

# Write join notes
join_notes_path <- file.path(folder, "join_notes.md")
con2 <- file(join_notes_path, open = "wt")
writeLines("# Join Columns", con2)
writeLines("", con2)
writeLines("Use these columns for fuzzy or exact joins to USAspending data:", con2)
writeLines("- `recipient_clean`: normalized recipient name", con2)
writeLines("- `project_clean`: normalized project description or purpose", con2)
writeLines("- `member_clean`: normalized member name", con2)
writeLines("- `amount_requested`: numeric amount for range or exact matching", con2)
writeLines("- `fiscal_year`: fiscal year alignment", con2)
writeLines("", con2)
writeLines("`project_id` is reserved for a future deterministic ID once a stable scheme is chosen.", con2)
close(con2)
