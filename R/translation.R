library(readr)
library(dplyr)
library(tibble)

translate_dollars <- function(total_usd, benchmarks_path = "data/benchmarks.csv") {
  total_usd <- suppressWarnings(as.numeric(total_usd))
  if (length(total_usd) == 0) {
    total_usd <- NA_real_
  } else {
    total_usd <- total_usd[[1]]
  }
  if (!file.exists(benchmarks_path)) {
    return(tibble(
      benchmark = "Benchmark data missing",
      unit_cost_usd = NA_real_,
      estimated_units = NA_real_,
      interpretation = NA_character_,
      note = paste("Expected", benchmarks_path)
    ))
  }

  benchmarks <- suppressMessages(read_csv(benchmarks_path, show_col_types = FALSE)) |>
    mutate(unit_cost_usd = suppressWarnings(as.numeric(unit_cost_usd)))

  benchmarks |>
    mutate(
      total_usd = total_usd,
      estimated_units = round(total_usd / unit_cost_usd, 1),
      interpretation = paste(format(round(estimated_units, 1), big.mark = ","), unit_label)
    ) |>
    transmute(
      benchmark = benchmark_name,
      unit_cost_usd = unit_cost_usd,
      estimated_units = estimated_units,
      interpretation = interpretation,
      note = "Illustrative estimate."
    )
}

translate_inflow <- function(total_usd) {
  translate_dollars(total_usd, "data/benchmarks_in.csv")
}

translate_outflow <- function(total_usd) {
  translate_dollars(total_usd, "data/benchmarks_out.csv")
}
