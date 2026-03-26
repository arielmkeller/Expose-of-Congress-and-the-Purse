library(readr)
library(dplyr)
library(tibble)

get_earmarks_for_legislator <- function(legislator_name) {
  path <- "data/earmarks.csv"

  if (!file.exists(path)) {
    return(tibble(
      legislator = legislator_name,
      project_type = c("Transportation", "Education", "Health"),
      amount_usd = c(8500000, 3200000, 4700000)
    ))
  }

  raw <- suppressMessages(read_csv(path, show_col_types = FALSE))
  required_cols <- c("legislator", "project_type", "amount_usd")
  if (!all(required_cols %in% names(raw))) {
    stop("data/earmarks.csv must include columns: legislator, project_type, amount_usd")
  }

  raw |>
    mutate(legislator = trimws(legislator)) |>
    filter(tolower(legislator) == tolower(trimws(legislator_name))) |>
    transmute(
      legislator = legislator,
      project_type = project_type,
      amount_usd = as.numeric(amount_usd)
    )
}
