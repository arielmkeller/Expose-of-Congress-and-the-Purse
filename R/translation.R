library(readr)
library(dplyr)
library(tibble)

translate_dollars <- function(total_usd) {
  benchmarks <- suppressMessages(read_csv("data/benchmarks.csv", show_col_types = FALSE))

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
