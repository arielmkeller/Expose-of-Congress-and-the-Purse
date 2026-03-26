library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

get_usaspending_context <- function(earmarks_df) {
  if (nrow(earmarks_df) == 0) {
    return(tibble(
      agency = character(),
      amount_usd = numeric(),
      note = character()
    ))
  }

  # Placeholder context: MVP keeps this simple and deterministic.
  top_types <- earmarks_df |>
    group_by(project_type) |>
    summarize(amount_usd = sum(amount_usd, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(amount_usd)) |>
    head(3)

  tibble(
    agency = c("Department of Transportation", "Department of Education", "HHS")[seq_len(nrow(top_types))],
    amount_usd = top_types$amount_usd,
    note = "Replace with live USAspending API mapping in next iteration."
  )
}
