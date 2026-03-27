# Expose of Congress and the Purse

Interactive Shiny tool to connect a legislator with:
- earmarked federal funding
- campaign finance activity
- plain-language dollar translations

## Quick start

1. Install packages in R:

```r
install.packages(c("shiny", "dplyr", "ggplot2", "DT", "httr2", "readr", "tibble"))
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

3. Optional: use local earmark data.
- Copy `data/earmarks_sample.csv` to `data/earmarks.csv`
- Replace with your Appropriations dataset using columns:
  - `legislator`
  - `project_type`
  - `amount_usd`

4. Run app:

```r
shiny::runApp()
```

## Current MVP status

- Legislator input: dropdown of current members loaded from GovTrack public API, with fallback sample list if unavailable.
- Member metadata: political affiliation, state, and chamber shown in the app.
- OpenFEC: live call if `OPENFEC_API_KEY` is set, fallback sample output otherwise.
- USAspending: live keyword-based call to `spending_by_category` with fallback context when unavailable.
- Translation layer: benchmark-based estimates from `data/benchmarks.csv`.
- Legislator matching: normalized/fuzzy matching for earmark records and improved candidate ranking for OpenFEC search.
