# Expose of Congress and the Purse

Interactive Shiny tool with two modules:
- Money into politics (FEC): who funds a member, donor mix, totals, and trends
- Money out into communities (USAspending): agencies and recipients by state/district

## Quick start

1. Install packages in R:

```r
install.packages(c(
  "shiny", "dplyr", "ggplot2", "DT",
  "httr2", "readr", "readxl", "tibble",
  "stringr", "jsonlite"
))
```

2. Add OpenFEC API key (choose one):

Option A: `.env` file in the project root (recommended)

```text
OPENFEC_API_KEY=your_openfec_key_here
```

Option B: shell profile or `.Renviron`

```bash
export OPENFEC_API_KEY="your_openfec_key_here"
```

3. Run app:

```r
shiny::runApp()
```

## Data and usage notes

- Earmarks data file: `fy2_All_Earmarks.xlsx` is required for earmark tables and totals. If it is missing, the app will show "No earmark requests found" and earmark ranks will be blank.
- Benchmarks data files: `data/benchmarks_in.csv` and `data/benchmarks_out.csv` are used to translate FEC and USAspending values into comparable estimates.
- Live vs. fallback data:
  - OpenFEC calls require `OPENFEC_API_KEY`. Without it, the app uses sample outputs and disables campaign funding ranks.
  - USAspending calls are public and do not require a key, but if the API is unreachable the app falls back to context-only messaging.

## Current MVP status

- Legislator input: dropdown of current members loaded from GovTrack public API, with fallback sample list if unavailable.
- Member metadata: political affiliation, state, and chamber shown in the app.
- OpenFEC: live call if `OPENFEC_API_KEY` is set, fallback sample output otherwise.
- USAspending: live keyword-based call to `spending_by_category` with fallback context when unavailable.
- Translation layer: benchmark-based estimates from `data/benchmarks_in.csv` (inflow) and `data/benchmarks_out.csv` (outflow).
- Legislator matching: normalized/fuzzy matching for earmark records and improved candidate ranking for OpenFEC search.
