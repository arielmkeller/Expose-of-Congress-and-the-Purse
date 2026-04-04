library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

format_dollar_short <- function(x) {
  vapply(
    x,
    function(val) {
      if (is.na(val)) {
        return(NA_character_)
      }

      absval <- abs(val)
      suffix <- ""
      scale <- 1

      if (absval >= 1e12) {
        suffix <- "T"
        scale <- 1e12
      } else if (absval >= 1e9) {
        suffix <- "B"
        scale <- 1e9
      } else if (absval >= 1e6) {
        suffix <- "M"
        scale <- 1e6
      } else if (absval >= 1e3) {
        suffix <- "K"
        scale <- 1e3
      }

      digits <- if (suffix == "") 0 else 1
      out <- formatC(val / scale, format = "f", digits = digits, big.mark = ",")
      out <- sub("\\.0$", "", out)
      paste0("$", out, suffix)
    },
    character(1)
  )
}

source("R/openfec.R")
source("R/usaspending.R")
source("R/translation.R")
source("R/legislators.R")
source("R/congress_api.R")
source("R/earmarks.R")
source("R/rankings.R")

state_lookup <- tibble(
  state_name = c(state.name, "District of Columbia", "Puerto Rico", "U.S. Virgin Islands", "Guam", "American Samoa", "Northern Mariana Islands"),
  state_abbrev = c(state.abb, "DC", "PR", "VI", "GU", "AS", "MP")
)

clean_legislator_name <- function(x) {
  one <- function(val) {
    y <- as.character(val)
    y <- tryCatch(iconv(y, to = "ASCII//TRANSLIT"), error = function(e) y)
    y <- sub("\\s*\\[.*\\]\\s*$", "", y)
    y <- gsub("\\([^\\)]*\\)", "", y)
    y <- gsub("\\b(rep\\.?|representative|sen\\.?|senator|delegate|del\\.?|hon\\.?|mr\\.?|mrs\\.?|ms\\.?|commish\\.?|commissioner)\\b", "", y, ignore.case = TRUE)
    y <- gsub("[^A-Za-z\\s'-]", " ", y)
    y <- gsub("\\s+", " ", y)
    y <- trimws(y)
    y <- sub("\\s+[A-Z]{1,2}-[A-Z]{2}\\d*$", "", y)
    if (identical(y, "")) {
      return(y)
    }
    tokens <- unlist(strsplit(y, " "))
    tokens <- tokens[tokens != ""]
    suffixes <- c("jr", "sr", "ii", "iii", "iv", "v")
    tokens <- tokens[!tolower(tokens) %in% suffixes]
    if (length(tokens) == 1) {
      return(tokens[[1]])
    }
    paste(tokens[[1]], tokens[[length(tokens)]])
  }
  if (length(x) == 1) {
    return(one(x))
  }
  vapply(x, one, character(1))
}

get_member_photo_url <- function(bioguide_id) {
  if (is.null(bioguide_id) || is.na(bioguide_id) || bioguide_id == "") {
    return(NA_character_)
  }
  bioguide_id <- as.character(bioguide_id)
  paste0("https://bioguide.congress.gov/bioguide/photo/", substr(bioguide_id, 1, 1), "/", bioguide_id, ".jpg")
}

safe_scalar <- function(x, fallback = "Unavailable") {
  if (length(x) == 0) {
    return(fallback)
  }
  val <- as.character(x[[1]])
  if (is.na(val) || val == "") {
    return(fallback)
  }
  val
}

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@300;400;600;700&family=Source+Serif+4:wght@500;700&display=swap"
    ),
    tags$style(HTML("
      :root {
        --ink: #111827;
        --muted: #6b7280;
        --accent: #00887c;
        --accent-soft: #e6f4f7;
        --panel: #ffffff;
        --panel-border: #e5e7eb;
        --bg: #f8fafc;
      }

      body, .container-fluid,
      .shiny-input-container, .form-control, .selectize-input,
      table.dataTable, table.dataTable th, table.dataTable td,
      .dataTables_wrapper, .dataTables_filter, .dataTables_length, .dataTables_info,
      .dataTables_paginate, .dataTables_wrapper input, .dataTables_wrapper select {
        font-family: 'Source Sans 3', system-ui, -apple-system, sans-serif;
        background: var(--bg);
        color: var(--ink);
      }

      h1, h2, h3 {
        font-family: 'Source Serif 4', serif;
        letter-spacing: -0.01em;
      }

      h1 {
        font-size: 2.1rem;
        margin-bottom: 0.35rem;
      }

      h2 {
        font-size: 1.5rem;
        margin-top: 1.5rem;
        margin-bottom: 0.6rem;
      }

      h3 {
        font-size: 1.25rem;
        margin-top: 1.2rem;
        margin-bottom: 0.5rem;
      }

      .title-panel {
        padding: 1.25rem 1.5rem 1rem 1.5rem;
        border-bottom: 1px solid var(--panel-border);
        margin-bottom: 1.2rem;
        background: var(--panel);
      }

      .title-panel .subtitle {
        color: var(--muted);
        font-size: 1rem;
        margin-top: 0.2rem;
      }

      .title-panel .overview-text {
        color: var(--ink);
        font-size: 1.2rem;
        line-height: 1.5;
        margin-top: 0.5rem;
        max-width: 520px;
      }

      .tab-description {
        color: var(--ink);
        font-size: 1.15rem;
        line-height: 1.5;
        margin: 0.2rem 0 1rem 0;
        max-width: 720px;
      }

      .plot-caption {
        color: var(--ink);
        font-size: 0.95rem;
        line-height: 1.4;
        margin-top: 0.35rem;
      }

      .context-heading,
      .context-subtext {
        color: var(--ink);
        font-size: 1.22rem;
        line-height: 1.55;
      }

      .context-heading {
        margin: 0.2rem 0 0.2rem 0;
        font-weight: 600;
      }

      .context-subtext {
        margin: 0 0 0.8rem 0;
      }

      .sidebar {
        background: var(--panel);
        border: 1px solid var(--panel-border);
        border-radius: 12px;
        padding: 1rem 1rem 0.8rem 1rem;
      }

      .map-panel {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        gap: 0.35rem;
        padding: 0.8rem 0 0.4rem 0;
      }

      .map-note {
        color: var(--muted);
        font-size: 0.85rem;
      }

      .member-snapshot {
        display: flex;
        align-items: flex-start;
        justify-content: flex-start;
        gap: 0.9rem;
        padding: 0.85rem 0.9rem;
        border: 1px solid var(--panel-border);
        border-radius: 10px;
        background: #fbfdff;
      }

      .member-snapshot-main {
        flex: 1;
        min-width: 0;
      }

      .member-name {
        font-size: 1.2rem;
        font-weight: 700;
        margin-bottom: 0.35rem;
      }

      .member-meta {
        display: flex;
        flex-direction: column;
        gap: 0.45rem;
        color: var(--muted);
        font-size: 0.92rem;
      }

      .member-pill {
        background: transparent;
        color: var(--ink);
        border-radius: 4px;
        border: 1px solid #d7dee6;
        padding: 0.38rem 0.7rem;
        font-size: 0.92rem;
        font-weight: 700;
        width: fit-content;
      }

      .member-photo {
        width: 72px;
        height: 92px;
        border-radius: 6px;
        overflow: hidden;
        border: 1px solid var(--panel-border);
        background: #f1f5f9;
        flex: 0 0 auto;
      }

      .member-photo img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        display: block;
      }

      .member-snapshot-stats {
        display: flex;
        flex-direction: column;
        gap: 0.5rem;
        margin-left: auto;
        min-width: 180px;
      }

      .member-stat-card {
        border: 1px solid var(--panel-border);
        border-radius: 8px;
        padding: 0.5rem 0.65rem;
        background: #ffffff;
      }

      .member-stat-title {
        font-size: 0.78rem;
        font-weight: 700;
        color: var(--muted);
        text-transform: uppercase;
        letter-spacing: 0.04em;
        margin-bottom: 0.2rem;
      }

      .main-wide {
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 1.2rem 2rem 1.2rem;
      }

      .section-card {
        background: var(--panel);
        border: 1px solid var(--panel-border);
        border-radius: 14px;
        padding: 1.2rem 1.4rem;
        margin-bottom: 1.2rem;
        box-shadow: 0 1px 2px rgba(15, 23, 42, 0.05);
      }

      .sub-card {
        border: 1px solid var(--panel-border);
        border-radius: 12px;
        padding: 0.9rem 1rem;
        margin-bottom: 0.9rem;
        background: #fbfcfd;
      }

      .sub-card-title {
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: var(--muted);
        margin-bottom: 0.4rem;
      }

      .stat-label {
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: var(--muted);
        margin-top: 0.2rem;
      }

      .stat-value {
        font-size: 1.8rem;
        font-weight: 700;
        color: var(--ink);
        margin-top: 0.3rem;
      }

      .custom-benchmark {
        margin-top: 0.7rem;
        padding-top: 0.6rem;
        border-top: 1px dashed var(--panel-border);
      }

      .custom-title {
        font-size: 0.85rem;
        color: var(--muted);
        margin-bottom: 0.4rem;
      }


      .section-label {
        font-size: 0.85rem;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: var(--muted);
        margin-bottom: 0.35rem;
      }

      .accent-bar {
        height: 4px;
        width: 60px;
        background: var(--accent);
        border-radius: 999px;
        margin-bottom: 0.9rem;
      }

      .helper-text {
        color: var(--muted);
        font-size: 0.95rem;
        margin: 0.2rem 0 1rem 0;
      }

      .dataTables_wrapper .dataTables_filter input {
        border-radius: 8px;
        border: 1px solid var(--panel-border);
        font-size: 0.8rem;
        padding: 0.2rem 0.4rem;
      }

      .dataTables_wrapper .dataTables_length label,
      .dataTables_wrapper .dataTables_filter label {
        font-size: 0.8rem;
        color: var(--muted);
      }

      .dataTables_wrapper .dataTables_length select {
        font-size: 0.8rem;
        padding: 0.2rem 0.3rem;
      }

      .nav-tabs > li > a,
      .nav-tabs > li > a:focus,
      .nav-tabs > li > a:hover {
        color: var(--ink);
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #00887c;
        font-weight: 600;
      }

      .nav-tabs {
        border-bottom: 1px solid var(--panel-border);
        margin-bottom: 1rem;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        border: none;
        border-bottom: 2px solid #00887c;
        background: transparent;
      }

      .nav-tabs > li > a {
        border: none;
        padding: 8px 12px;
      }

      .tab-content {
        padding-top: 0.5rem;
      }

      table.dataTable thead th {
        font-size: 0.85rem;
        color: var(--muted);
        white-space: nowrap;
      }

      table.dataTable tbody td {
        font-size: 0.95rem;
      }

      .shiny-output-error-validation {
        color: var(--muted);
      }
    "))
  ),
  tags$div(
    class = "title-panel",
    fluidRow(
      column(
        4,
        tags$h1("Congress, Cash, & Constituents"),
        tags$div(
          class = "overview-text",
          "This app is for policymakers and curious citizens alike to explore how money moves through Congress: campaign cash in, federal dollars out, and how it ties back to a member’s district and state. Use the tabs to explore funding totals, trends, rankings, and where awards land geographically."
        )
      ),
      column(
        4,
        tags$div(
          class = "map-panel",
          plotOutput("member_district_map", height = 220),
          uiOutput("member_district_note")
        )
      ),
      column(
        4,
        tags$div(
          class = "sidebar",
          selectizeInput("state", "State", choices = NULL, multiple = FALSE),
          uiOutput("legislator_ui"),
          numericInput("cycle", "Election cycle", value = 2024, min = 2000, step = 2),
          actionButton("run", "Analyze")
        )
      )
    )
  ),
  tabsetPanel(
    tabPanel(
      "Overview",
      tags$div(
        class = "main-wide",
        tags$div(
          class = "section-card",
          tags$div(class = "section-label", "Overview"),
          tags$div(class = "accent-bar"),
          h2("Snapshot: Campaign Funding and Federal Awards"),
          tags$div(class = "tab-description", "Shows the key totals for the selected member and state, so you can see campaign cash in and federal dollars out at a glance."),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Member Profile"),
            uiOutput("member_profile_overview_ui"),
            tableOutput("member_details_overview"),
            uiOutput("member_details_note")
          ),
          NULL
        )
      )
    ),
    tabPanel(
      "Earmarks",
      tags$div(
        class = "main-wide",
        tags$div(
          class = "section-card",
          tags$div(class = "section-label", "Earmarks"),
          tags$div(class = "accent-bar"),
          h2("Earmark Requests"),
          tags$div(class = "tab-description", "Lists the member’s earmark requests, including project names and requested amounts."),
          tags$div(
            class = "sub-card",
            tags$div(class = "stat-label", "Total Earmark Spending"),
            tags$div(class = "stat-value", textOutput("earmarks_total")),
            tableOutput("earmarks_table")
          )
        )
      )
    ),
    tabPanel(
      "Rankings",
      tags$div(
        class = "main-wide",
        tags$div(
          class = "section-card",
          tags$div(class = "section-label", "Rankings"),
          tags$div(class = "accent-bar"),
          h2("Member + State Rankings"),
          tags$div(class = "tab-description", "Compares the selected member and state against others on fundraising and federal spending totals."),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Ranking Snapshot"),
            tags$div(
              style = "display:flex;gap:0.5rem;flex-wrap:wrap;margin-bottom:0.6rem;",
              actionButton("build_campaign_ranks", "Compute Campaign Funding Ranks (State)"),
              actionButton("build_campaign_ranks_national", "Compute Campaign Funding Ranks (National, long run)"),
              actionButton("build_state_spending_ranks", "Compute State Spending Rank")
            ),
            tableOutput("rankings_table"),
            uiOutput("rankings_note")
          )
        )
      )
    ),
    tabPanel(
      "Cash In",
      tags$div(
        class = "main-wide",
      tags$div(
        class = "section-card",
        tags$div(class = "section-label", "Cash In"),
        tags$div(class = "accent-bar"),
        h2("Cash In — Campaign Money by Member"),
        tags$div(class = "tab-description", "Breaks down campaign fundraising by source, donor type, and internal transfers, with trends and context."),
        tags$div(
          class = "sub-card",
          tags$div(class = "sub-card-title", "Campaign Receipts"),
            tags$div(class = "stat-label", "Total Receipts"),
            tags$div(class = "stat-value", textOutput("totals_in_cash_in"))
          ),
        tags$div(
          class = "sub-card",
            tags$div(class = "sub-card-title", "Receipts Trend"),
            tabsetPanel(
              tabPanel(
                "New PAC + Individual Receipts (Cycle)",
                plotOutput("receipts_trend_donor_types_overview", height = 260),
                tags$div(
                  class = "plot-caption",
                  "Shows new receipts by donor type for the selected cycle. Internal PAC/committee transfers are shown separately."
                )
              ),
              tabPanel("Donor Employers", plotOutput("receipts_trend_employers_overview", height = 260)),
              tabPanel("Donor Occupations", plotOutput("receipts_trend_occupations_overview", height = 260)),
              tabPanel("Internal PAC/Committee Transfers", plotOutput("receipts_trend_internal_overview", height = 260))
            )
          ),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Context"),
            tags$div(class = "context-heading", "Putting Campaign Cash in Context"),
            tags$div(class = "context-subtext", "Translate campaign totals into everyday costs."),
            tableOutput("translation_in"),
            tags$div(
              class = "custom-benchmark",
              tags$div(class = "custom-title", "Add a custom benchmark"),
              fluidRow(
                column(
                  6,
                  textInput("custom_benchmark_label", "Benchmark name", value = "")
                ),
                column(
                  6,
                  numericInput("custom_benchmark_cost", "Cost per unit ($)", value = NA_real_, min = 0)
                )
              )
            ),
            uiOutput("translation_in_note")
          )
        )
      )
    ),
    tabPanel(
      "Cash Out",
      tags$div(
        class = "main-wide",
      tags$div(
        class = "section-card",
        tags$div(class = "section-label", "Cash Out"),
        tags$div(class = "accent-bar"),
        h2("Cash Out — Where Federal Funds Are Awarded"),
        tags$div(class = "tab-description", "Maps and summarizes where federal awards go, by district and county, with totals and context."),
        tags$div(
          class = "sub-card",
          tags$div(class = "sub-card-title", "Federal Awards"),
            tags$div(class = "stat-label", "Total Outflow"),
            tags$div(class = "stat-value", textOutput("totals_out_cash_out"))
          ),
        tags$div(
          class = "sub-card",
          tags$div(class = "sub-card-title", "Geography"),
            h3("Federal Awards by Recipient Location"),
            tabsetPanel(
              id = "geo_view_overview",
              tabPanel("District"),
              tabPanel("County")
            ),
            plotOutput("spending_geo_map_overview", height = 320),
            uiOutput("geo_note_overview")
          ),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Context"),
            tags$div(class = "context-heading", "Putting State-Level Spending in Context"),
            tags$div(class = "context-subtext", "Translate federal award totals into everyday costs."),
            tableOutput("translation_out"),
            tags$div(
              class = "custom-benchmark",
              tags$div(class = "custom-title", "Add a custom benchmark"),
              fluidRow(
                column(
                  6,
                  textInput("custom_benchmark_out_label", "Benchmark name", value = "")
                ),
                column(
                  6,
                  numericInput("custom_benchmark_out_cost", "Cost per unit ($)", value = NA_real_, min = 0)
                )
              )
            ),
            uiOutput("translation_out_note")
          )
        )
      )
    ),
    tabPanel(
      "Dig Deeper",
      tags$div(
        class = "main-wide",
        tags$div(
          class = "section-card",
          tags$div(class = "section-label", "Cash In"),
          tags$div(class = "accent-bar"),
          h2("Cash In — Campaign Money by Member"),
          tags$div(class = "tab-description", "Provides detailed tables behind the charts, including donors, committees, agencies, and recipients."),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Receipts"),
            h3("Campaign Receipts"),
            fluidRow(
              column(
                7,
                DTOutput("donor_mix"),
                uiOutput("donor_mix_note")
              ),
              column(
                5,
                tags$div(class = "stat-label", "Total Receipts"),
                tags$div(class = "stat-value", textOutput("totals_in_dig"))
              )
            )
          ),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Breakdowns"),
            h3("Top 10 Donor Employers"),
            DTOutput("donor_employers"),
            uiOutput("donor_employers_note"),
            h3("Top 10 Donor Occupations"),
            DTOutput("donor_occupations"),
            uiOutput("donor_occupations_note")
          ),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Committees"),
            h3("Top Internal Committee Transfers"),
            DTOutput("donor_pacs_internal")
          )
        ),
        tags$div(
          class = "section-card",
          tags$div(class = "section-label", "Cash Out"),
          tags$div(class = "accent-bar"),
          h2("Cash Out — Where Federal Funds Are Awarded"),
          tags$div(class = "tab-description", "Provides detailed tables behind the charts, including donors, committees, agencies, and recipients."),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Recipients"),
            h3("State Recipients"),
            DTOutput("spending_recipients"),
            uiOutput("recipients_note")
          ),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Agencies"),
            h3("Federal Funding by Awarding Agency"),
            DTOutput("spending_agencies"),
            uiOutput("agencies_note")
          ),
          tags$div(
            class = "sub-card",
            tags$div(class = "sub-card-title", "Geography"),
            h3("Federal Awards by Recipient Location"),
            tabsetPanel(
              id = "geo_view",
              tabPanel("District"),
              tabPanel("County")
            ),
            DTOutput("spending_geo_table"),
            uiOutput("geo_table_note")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  get_single <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    as.character(x[[1]])
  }

  normalize_state_abbrev <- function(x) {
    s <- trimws(get_single(x))
    if (is.na(s) || s == "") {
      return(NA_character_)
    }
    if (nchar(s) == 2) {
      return(toupper(s))
    }
    ab <- state_lookup$state_abbrev[state_lookup$state_name == s]
    if (length(ab) == 0) {
      return(NA_character_)
    }
    toupper(ab[[1]])
  }

  cache_path <- "/Users/ariel/Desktop/Expose-of-Congress-and-the-Purse/data/legislators_cache.csv"
  if (file.exists(cache_path)) {
    cached <- tryCatch(read.csv(cache_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(cached) && nrow(cached) > 0) {
      legislators_ref <- as_tibble(cached)
      if (!"bioguide_id" %in% names(legislators_ref)) {
        legislators_ref <- get_legislators_reference()
      }
    } else {
      legislators_ref <- get_legislators_reference()
    }
  } else {
    legislators_ref <- get_legislators_reference()
  }
  legislators_ref <- legislators_ref |>
    mutate(
      state = toupper(state),
      legislator_clean = clean_legislator_name(legislator)
    ) |>
    filter(!is.na(legislator_clean), legislator_clean != "")
  if (!"bioguide_id" %in% names(legislators_ref)) {
    legislators_ref$bioguide_id <- NA_character_
  }
  message("Loaded legislators: ", nrow(legislators_ref), " from cache: ", cache_path)
  state_choices <- setNames(state_lookup$state_abbrev, state_lookup$state_name)
  updateSelectizeInput(session, "state", choices = state_choices, selected = state_lookup$state_abbrev[[1]], server = TRUE)

  output$legislator_ui <- renderUI({
    st <- normalize_state_abbrev(input$state)
    if (nrow(legislators_ref) == 0) {
      choices <- c("")
    } else if (is.null(st) || is.na(st) || st == "") {
      choices <- c("", sort(unique(legislators_ref$legislator_clean)))
    } else {
      filtered <- legislators_ref |>
        filter(state == toupper(st)) |>
        pull(legislator_clean) |>
        unique() |>
        sort()
      choices <- c("", filtered)
    }
    selectizeInput(
      "legislator",
      "Member of Congress",
      choices = choices,
      multiple = FALSE,
      options = list(create = FALSE, placeholder = "Select a member", maxOptions = 5000)
    )
  })

  selected_member <- reactive({
    legis <- get_single(input$legislator)
    if (is.null(legis) || is.na(legis) || legis == "") {
      return(tibble())
    }
    st <- normalize_state_abbrev(input$state)
    out <- legislators_ref |>
      filter(legislator_clean == legis)
    if (!is.na(st) && nrow(out) > 0) {
      out_state <- out |> filter(state == st)
      if (nrow(out_state) > 0) {
        return(out_state |> slice(1))
      }
    }
    out |> slice(1)
  })

  build_openfec_placeholder <- function(state, note) {
    list(
      diagnostics = list(
        api_key_present = !identical(Sys.getenv("OPENFEC_API_KEY", unset = ""), ""),
        candidate_id = NA_character_,
        cycle_used = NA_integer_,
        principal_committees = character(),
        state_used = state,
        error = note
      ),
      summary = tibble(
        source = "OpenFEC unavailable",
        amount_usd = NA_real_,
        note = note
      ),
      donor_types = tibble(donor_type = character(), amount_usd = numeric(), note = ""),
      industries = tibble(industry = character(), amount_usd = numeric(), note = ""),
      occupations = tibble(occupation = character(), amount_usd = numeric(), note = ""),
      pacs_all = tibble(pac = character(), amount_usd = numeric(), note = ""),
      pacs_internal = tibble(pac = character(), amount_usd = numeric(), note = ""),
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    )
  }

  district_shape_cache <- new.env(parent = emptyenv())
  county_map_cache <- new.env(parent = emptyenv())
  county_map_all <- NULL

  get_district_shape_cached <- function(state_abbrev) {
    if (is.null(state_abbrev) || is.na(state_abbrev) || state_abbrev == "") {
      return(NULL)
    }
    year_try <- as.integer(format(Sys.Date(), "%Y"))
    key <- paste(state_abbrev, year_try, sep = ":")
    if (exists(key, envir = district_shape_cache, inherits = FALSE)) {
      return(get(key, envir = district_shape_cache))
    }
    shape <- tryCatch(
      tigris::congressional_districts(state = state_abbrev, year = year_try, cb = TRUE, class = "sf"),
      error = function(e) NULL
    )
    if (is.null(shape)) {
      shape <- tryCatch(
        tigris::congressional_districts(state = state_abbrev, cb = TRUE, class = "sf"),
        error = function(e) NULL
      )
    }
    assign(key, shape, envir = district_shape_cache)
    shape
  }

  get_county_map_cached <- function(state_name) {
    if (is.null(state_name) || is.na(state_name) || state_name == "") {
      return(NULL)
    }
    state_key <- tolower(state_name)
    if (exists(state_key, envir = county_map_cache, inherits = FALSE)) {
      return(get(state_key, envir = county_map_cache))
    }
    if (is.null(county_map_all)) {
      county_map_all <<- ggplot2::map_data("county")
    }
    map_df <- county_map_all[county_map_all$region == state_key, , drop = FALSE]
    assign(state_key, map_df, envir = county_map_cache)
    map_df
  }

  results <- eventReactive(input$run, {
    withProgress(message = "Analyzing…", value = 0, {
      state_selected <- normalize_state_abbrev(input$state)
      state_name_selected <- state_lookup$state_name[state_lookup$state_abbrev == state_selected]
      if (length(state_name_selected) == 0) {
        state_name_selected <- NA_character_
      }
      shiny::validate(shiny::need(!is.na(state_selected) && nchar(state_selected) > 0, "Select a state."))

      legislator <- trimws(get_single(input$legislator))
      member <- selected_member()
      chamber <- ifelse(nrow(member) > 0, member$chamber[[1]], NA_character_)
      member_state <- ifelse(nrow(member) > 0, member$state[[1]], state_selected)
      bioguide_id <- ifelse(nrow(member) > 0 && "bioguide_id" %in% names(member), member$bioguide_id[[1]], NA_character_)

      incProgress(0.2, detail = "Loading campaign finance")
      if (!is.null(legislator) && legislator != "") {
        finance <- get_openfec_summary(
          legislator,
          cycle = input$cycle,
          chamber = ifelse(nrow(member) > 0, chamber, NA_character_),
          state = member_state,
          district = ifelse(nrow(member) > 0 && "district" %in% names(member), member$district[[1]], NA_character_)
        )
      } else {
        finance <- build_openfec_placeholder(member_state, "Select a legislator for OpenFEC campaign finance data.")
      }

      incProgress(0.5, detail = "Loading federal awards")
      district_value <- ifelse(nrow(member) > 0 && "district" %in% names(member), member$district[[1]], NA_character_)
      spending <- get_usaspending_context(state = member_state, district = district_value)
      receipts_total <- finance$summary |>
        filter(source == "Total receipts") |>
        summarize(total = sum(amount_usd, na.rm = TRUE)) |>
        pull(total)
      if (length(receipts_total) == 0 || is.na(receipts_total)) {
        receipts_total <- sum(finance$summary$amount_usd, na.rm = TRUE)
      }

      incProgress(0.7, detail = "Building context")
      translation_in <- translate_inflow(suppressWarnings(as.numeric(receipts_total)))
      translation_out <- translate_outflow(spending$total_amount)

      incProgress(0.85, detail = "Loading member profile")
      congress_profile <- get_congress_member_profile(bioguide_id)

      incProgress(1)
      list(
        legislator = ifelse(is.null(legislator) || legislator == "", paste0("State: ", state_name_selected), legislator),
        state_selected = state_selected,
        member = member,
        congress_profile = congress_profile,
        finance = finance,
        spending = spending,
        translation_in = translation_in,
        translation_out = translation_out
      )
    })
  })

  output$member_profile <- renderTable({
    member <- selected_member()
    if (nrow(member) > 0) {
      district_val <- if ("district" %in% names(member)) as.character(member$district) else NA_character_
      district_val <- ifelse(is.na(district_val) | district_val == "" | district_val == "0" | district_val == "00",
                             ifelse(tolower(member$chamber[[1]]) == "house", "At-Large", NA_character_),
                             district_val)
      member |>
        transmute(
          Name = legislator,
          Party = party,
          State = state,
          Chamber = chamber,
          District = district_val
        )
    } else {
      state_selected <- normalize_state_abbrev(input$state)
      state_name_selected <- state_lookup$state_name[state_lookup$state_abbrev == state_selected]
      if (length(state_name_selected) == 0) {
        state_name_selected <- NA_character_
      }
      data.frame(
        Name = "State-level view",
        Party = NA_character_,
        State = state_name_selected,
        Chamber = NA_character_,
        District = NA_character_
      )
    }
  }, striped = TRUE, bordered = TRUE, width = "100%")

  campaign_rank_totals <- reactiveVal(NULL)
  state_spending_totals_cache <- reactiveVal(NULL)

  observeEvent(input$build_campaign_ranks, {
    api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
    if (identical(api_key, "")) {
      showNotification("Set OPENFEC_API_KEY to compute campaign funding ranks.", type = "error")
      return()
    }
    member <- selected_member()
    member_state <- if (nrow(member) > 0) member$state[[1]] else normalize_state_abbrev(input$state)
    member_name <- if (nrow(member) > 0) member$legislator[[1]] else NA_character_
    withProgress(message = "Building campaign funding ranks...", value = 0, {
      incProgress(0.1, detail = "Fetching OpenFEC totals for selected state")
      totals <- compute_campaign_receipts_totals(
        legislators_ref,
        cycle = input$cycle,
        limit_states = member_state,
        priority_names = member_name
      )
      campaign_rank_totals(totals)
      incProgress(0.9)
    })
  })

  observeEvent(input$build_campaign_ranks_national, {
    api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
    if (identical(api_key, "")) {
      showNotification("Set OPENFEC_API_KEY to compute campaign funding ranks.", type = "error")
      return()
    }
    withProgress(message = "Building national campaign funding ranks (this can take a few minutes)...", value = 0, {
      incProgress(0.05, detail = "Fetching OpenFEC totals for all legislators")
      totals <- compute_campaign_receipts_totals(legislators_ref, cycle = input$cycle)
      campaign_rank_totals(totals)
      incProgress(0.95)
    })
  })

  observeEvent(input$build_state_spending_ranks, {
    withProgress(message = "Building state spending ranks...", value = 0, {
      incProgress(0.1, detail = "Fetching USAspending totals for 50 states")
      totals <- get_state_spending_totals()
      state_spending_totals_cache(totals)
      incProgress(0.9)
    })
  })

  output$member_profile_overview_ui <- renderUI({
    member <- selected_member()
    if (nrow(member) > 0) {
      district_val <- if ("district" %in% names(member)) as.character(member$district) else NA_character_
      district_val <- ifelse(is.na(district_val) | district_val == "" | district_val == "0" | district_val == "00",
                             ifelse(tolower(member$chamber[[1]]) == "house", "At-Large", NA_character_),
                             district_val)
      photo_url <- get_member_photo_url(member$bioguide_id[[1]])
      name_label <- safe_scalar(member$legislator, fallback = "Member")
      party_label <- safe_scalar(member$party, fallback = "Unknown")
      state_label <- safe_scalar(member$state, fallback = "Unknown")
      chamber_label <- safe_scalar(member$chamber, fallback = "Unknown")
      district_label <- if (!is.na(district_val) && district_val != "") as.character(district_val) else ""
      tags$div(
        class = "member-snapshot",
        tags$div(
          class = "member-photo",
          if (!is.na(photo_url)) tags$img(src = photo_url, alt = paste(name_label, "photo"))
        ),
        tags$div(
          class = "member-snapshot-main",
          tags$div(class = "member-name", name_label),
          tags$div(
            class = "member-meta",
            tags$span(class = "member-pill", paste0("Party: ", party_label)),
            tags$span(class = "member-pill", paste0("State: ", state_label)),
            tags$span(class = "member-pill", paste0("Chamber: ", chamber_label)),
            if (district_label != "") {
              tags$span(class = "member-pill", paste0("District: ", district_label))
            }
          )
        ),
        tags$div(
          class = "member-snapshot-stats",
          tags$div(
            class = "member-stat-card",
            tags$div(class = "member-stat-title", "Campaign Receipts"),
            tags$div(class = "stat-label", "Total Receipts"),
            tags$div(class = "stat-value", textOutput("totals_in_overview"))
          ),
          tags$div(
            class = "member-stat-card",
            tags$div(class = "member-stat-title", "Federal Awards"),
            tags$div(class = "stat-label", "Total Outflow"),
            tags$div(class = "stat-value", textOutput("totals_out_overview"))
          )
        )
      )
    } else {
      state_selected <- normalize_state_abbrev(input$state)
      state_name_selected <- state_lookup$state_name[state_lookup$state_abbrev == state_selected]
      state_name_selected <- safe_scalar(state_name_selected, fallback = "Unavailable")
      tags$div(
        class = "member-snapshot",
        tags$div(class = "member-photo"),
        tags$div(
          class = "member-snapshot-main",
          tags$div(class = "member-name", "State-level view"),
          tags$div(
            class = "member-meta",
            tags$span(class = "member-pill", paste0("State: ", state_name_selected))
          )
        ),
        tags$div(
          class = "member-snapshot-stats",
          tags$div(
            class = "member-stat-card",
            tags$div(class = "member-stat-title", "Campaign Receipts"),
            tags$div(class = "stat-label", "Total Receipts"),
            tags$div(class = "stat-value", textOutput("totals_in_overview"))
          ),
          tags$div(
            class = "member-stat-card",
            tags$div(class = "member-stat-title", "Federal Awards"),
            tags$div(class = "stat-label", "Total Outflow"),
            tags$div(class = "stat-value", textOutput("totals_out_overview"))
          )
        )
      )
    }
  })

  output$member_details_overview <- renderTable({
    details <- results()$congress_profile
    if (is.null(details) || is.null(details$profile) || nrow(details$profile) == 0) {
      data.frame(
        Field = "Member details",
        Value = "Unavailable"
      )
    } else {
      details$profile |>
        rename(Field = field, Value = value)
    }
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$member_details_note <- renderUI({
    details <- results()$congress_profile
    note <- if (!is.null(details) && !is.null(details$note)) details$note else ""
    if (is.null(note) || note == "") {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", note)
    }
  })

  output$earmarks_table <- renderTable({
    member <- selected_member()
    if (nrow(member) == 0) {
      return(data.frame(Message = "Select a member to view earmark requests."))
    }

    earmarks <- get_earmarks_for_legislator(member$legislator[[1]], member$chamber[[1]])
    if (nrow(earmarks) == 0) {
      return(data.frame(Message = "No earmark requests found for this member in the dataset."))
    }

    earmarks |>
      mutate(
        `Total Amount` = ifelse(
          is.na(.data$Total_Amount),
          "Unavailable",
          paste0("$", formatC(.data$Total_Amount, format = "f", digits = 0, big.mark = ","))
        )
      ) |>
      transmute(
        Chamber = .data$Chamber,
        `Requesting Members` = .data$Requesting_Members,
        Project = .data$Project,
        Recipient = .data$Recipient,
        Location = .data$Location,
        Agency = .data$Agency,
        Account = .data$Account,
        Bill = .data$Bill,
        `Total Amount` = .data$`Total Amount`
      )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$earmarks_total <- renderText({
    member <- selected_member()
    if (nrow(member) == 0) {
      return("Unavailable")
    }
    earmarks <- get_earmarks_for_legislator(member$legislator[[1]], member$chamber[[1]])
    if (nrow(earmarks) == 0) {
      return("Unavailable")
    }
    total <- sum(earmarks$Total_Amount, na.rm = TRUE)
    if (is.na(total) || total == 0) {
      return("Unavailable")
    }
    paste0("$", formatC(total, format = "f", digits = 0, big.mark = ","))
  })

  output$rankings_table <- renderTable({
    tryCatch({
      member <- selected_member()
      if (nrow(member) == 0) {
        return(data.frame(Message = "Select a member to view rankings."))
      }

      earmark_ranks <- get_earmark_ranks_for_member(member, legislators_ref)
      earmark_value <- format_money(earmark_ranks$total)

      campaign_totals <- campaign_rank_totals()
      if (is.null(campaign_totals)) {
        campaign_totals <- load_campaign_receipts_cache()$data
      }
      campaign_ranks <- get_campaign_ranks_for_member(member, legislators_ref, cycle = input$cycle, totals = campaign_totals)
      res <- tryCatch(results(), error = function(e) NULL)
      campaign_value <- if (is.null(res)) "Unavailable" else format_money(extract_total_receipts(res$finance))

      state_abbrev <- member$state[[1]]
      state_spending_totals <- state_spending_totals_cache()
      if (is.null(state_spending_totals)) {
        cached <- load_rank_cache("state_spending_totals_cache.rds", state_spending_cache_key)
        if (!is.null(cached) && !is.null(cached$data)) {
          state_spending_totals <- cached$data
        }
      }
      state_spending_rank <- list(total = NA_real_, rank = NA_integer_, total_states = NA_integer_)
      if (!is.null(state_spending_totals)) {
        totals <- state_spending_totals |>
          filter(!is.na(total_spending))
        total_val <- totals$total_spending[totals$state == state_abbrev]
        total_val <- if (length(total_val) == 0) NA_real_ else total_val[[1]]
        rank_val <- if (!is.na(total_val) && nrow(totals) > 0) {
          rank(-totals$total_spending, ties.method = "min")[totals$state == state_abbrev][[1]]
        } else {
          NA_integer_
        }
        state_spending_rank <- list(
          total = total_val,
          rank = rank_val,
          total_states = nrow(totals)
        )
      }

      data.frame(
        Metric = c("Campaign funding (cycle)", "Earmark spending", "Federal spending to state"),
        Value = c(
          campaign_value,
          earmark_value,
          format_money(state_spending_rank$total)
        ),
        `Rank in State` = c(
          format_rank(campaign_ranks$state_rank, campaign_ranks$state_total),
          format_rank(earmark_ranks$state_rank, earmark_ranks$state_total),
          "N/A"
        ),
        `Rank National` = c(
          format_rank(campaign_ranks$national_rank, campaign_ranks$national_total),
          format_rank(earmark_ranks$national_rank, earmark_ranks$national_total),
          format_rank(state_spending_rank$rank, state_spending_rank$total_states)
        ),
        check.names = FALSE
      )
    }, error = function(e) {
      data.frame(Message = paste("Ranking data unavailable:", e$message))
    })
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$rankings_note <- renderUI({
    notes <- c()
    api_key <- Sys.getenv("OPENFEC_API_KEY", unset = "")
    if (identical(api_key, "")) {
      notes <- c(notes, "Campaign funding ranks require OPENFEC_API_KEY.")
    }
    campaign_totals <- campaign_rank_totals()
    if (is.null(campaign_totals)) {
      campaign_totals <- load_campaign_receipts_cache()$data
    }
    if (!is.null(campaign_totals) && nrow(campaign_totals) > 0) {
      available <- sum(!is.na(campaign_totals$total_receipts))
      if (available > 0 && available < 50) {
        notes <- c(notes, paste0("Campaign funding ranks are incomplete (", available, " members with totals)."))
      }
    }
    cached_state_spending <- load_rank_cache("state_spending_totals_cache.rds", state_spending_cache_key)
    if (is.null(state_spending_totals_cache()) && is.null(cached_state_spending)) {
      notes <- c(notes, "Click “Compute State Spending Rank” to rank the state against all 50 states.")
    }
    cached_campaign <- load_campaign_receipts_cache()$data
    if (is.null(campaign_rank_totals()) && (is.null(cached_campaign) || nrow(cached_campaign) == 0)) {
      notes <- c(notes, "Click “Compute Campaign Funding Ranks” for state + national comparisons.")
    }
    if (length(notes) == 0) {
      return(NULL)
    }
    tags$div(style = "color:#666;font-size:0.85em;", paste(notes, collapse = " "))
  })

  output$member_district_map <- renderPlot({
    member <- selected_member()
    if (nrow(member) == 0) {
      plot.new()
      text(0.5, 0.5, "Select a member to view district map.", cex = 0.9)
      return()
    }
    if (!requireNamespace("tigris", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
      plot.new()
      text(0.5, 0.5, "Packages 'tigris' and 'sf' required for district map.", cex = 0.9)
      return()
    }

    state_abbrev <- member$state[[1]]
    chamber <- tolower(member$chamber[[1]])
    district <- if ("district" %in% names(member)) as.character(member$district[[1]]) else NA_character_
    if (is.na(state_abbrev) || state_abbrev == "") {
      plot.new()
      text(0.5, 0.5, "State unavailable for district map.", cex = 0.9)
      return()
    }
    if (chamber == "senate" || is.na(district) || district == "" || district == "0" || district == "00") {
      state_shape <- tryCatch(
        tigris::states(cb = TRUE, class = "sf"),
        error = function(e) NULL
      )
      if (is.null(state_shape)) {
        plot.new()
        text(0.5, 0.5, "State map unavailable.", cex = 0.9)
        return()
      }
      state_shape <- state_shape |>
        filter(.data$STUSPS == state_abbrev)
      if (nrow(state_shape) == 0) {
        plot.new()
        text(0.5, 0.5, "State map unavailable.", cex = 0.9)
        return()
      }
      return(
        ggplot() +
          geom_sf(data = state_shape, fill = "#E7EEF7", color = "#2B4C7E", linewidth = 0.6) +
          theme_void()
      )
    }

    district_num <- suppressWarnings(as.integer(district))
    if (is.na(district_num)) {
      plot.new()
      text(0.5, 0.5, "District unavailable for map.", cex = 0.9)
      return()
    }

    year_try <- as.integer(format(Sys.Date(), "%Y"))
    shape <- tryCatch(
      tigris::congressional_districts(state = state_abbrev, year = year_try, cb = TRUE, class = "sf"),
      error = function(e) NULL
    )
    if (is.null(shape)) {
      shape <- tryCatch(
        tigris::congressional_districts(state = state_abbrev, cb = TRUE, class = "sf"),
        error = function(e) NULL
      )
    }
    if (is.null(shape)) {
      plot.new()
      text(0.5, 0.5, "District map unavailable.", cex = 0.9)
      return()
    }

    cd_cols <- names(shape)[grepl("^CD\\d+FP$", names(shape))]
    cd_col <- if (length(cd_cols) > 0) cd_cols[[1]] else if ("CD" %in% names(shape)) "CD" else NA_character_
    if (is.na(cd_col)) {
      plot.new()
      text(0.5, 0.5, "District map unavailable.", cex = 0.9)
      return()
    }

    district_num_value <- district_num
    shape <- shape |>
      mutate(district_num = suppressWarnings(as.integer(.data[[cd_col]])))
    highlight <- shape |>
      filter(district_num == district_num_value)

    ggplot() +
      geom_sf(data = shape, fill = "#e7f5f2", color = "white", size = 0.2) +
      geom_sf(data = highlight, fill = "#00887c", color = "white", size = 0.3) +
      theme_void(base_size = 11)
  })

  output$member_district_note <- renderUI({
    member <- selected_member()
    if (nrow(member) == 0) {
      return(NULL)
    }
    chamber <- tolower(member$chamber[[1]])
    district <- if ("district" %in% names(member)) as.character(member$district[[1]]) else NA_character_
    is_at_large <- is.na(district) || district == "" || district == "0" || district == "00"
    note <- NULL
    if (chamber == "senate") {
      note <- "Senators represent the whole state."
    } else if (is_at_large) {
      note <- "At-large district."
    }
    if (is.null(note)) {
      return(NULL)
    }
    tags$div(class = "map-note", note)
  })

  formatted_receipts_total <- function() {
    x <- results()
    if (is.null(x)) {
      return("Click Analyze")
    }
    legis_sel <- get_single(input$legislator)
    if (is.null(legis_sel) || is.na(legis_sel) || legis_sel == "") {
      return("Select a Member of Congress")
    }
    receipts_total <- x$finance$summary |>
      filter(source == "Total receipts") |>
      summarize(total = sum(amount_usd, na.rm = TRUE)) |>
      pull(total)
    if (length(receipts_total) == 0 || is.na(receipts_total)) {
      receipts_total <- sum(x$finance$summary$amount_usd, na.rm = TRUE)
    }
    if (is.na(receipts_total) || receipts_total == 0) {
      "Unavailable"
    } else {
      paste0("$", format(round(receipts_total, 2), big.mark = ","))
    }
  }

  output$totals_in_overview <- renderText({
    formatted_receipts_total()
  })

  output$totals_in_cash_in <- renderText({
    formatted_receipts_total()
  })

  output$totals_in_dig <- renderText({
    formatted_receipts_total()
  })


  formatted_outflow_total <- function() {
    x <- results()
    if (is.null(x)) {
      return("Click Analyze")
    }
    total_amount <- x$spending$total_amount
    if (is.null(total_amount) || is.na(total_amount) || total_amount == 0) {
      "Unavailable"
    } else {
      paste0("$", format(round(total_amount, 2), big.mark = ","))
    }
  }

  output$totals_out_overview <- renderText({
    formatted_outflow_total()
  })

  output$totals_out_cash_out <- renderText({
    formatted_outflow_total()
  })


  output$translation_in <- renderTable({
    base <- results()$translation_in

    custom_label <- trimws(input$custom_benchmark_label)
    custom_cost <- suppressWarnings(as.numeric(input$custom_benchmark_cost))
    if (!is.null(custom_label) && nzchar(custom_label) && !is.na(custom_cost) && custom_cost > 0) {
      receipts_total <- results()$finance$summary |>
        filter(source == "Total receipts") |>
        summarize(total = sum(amount_usd, na.rm = TRUE)) |>
        pull(total)
      if (length(receipts_total) == 0 || is.na(receipts_total)) {
        receipts_total <- sum(results()$finance$summary$amount_usd, na.rm = TRUE)
      }
      if (!is.na(receipts_total) && receipts_total > 0) {
        units <- floor(receipts_total / custom_cost)
        interpretation <- paste0(
          "At $", format(round(custom_cost, 2), big.mark = ","),
          " each, this equals about ", format(units, big.mark = ","),
          " units."
        )
        base <- bind_rows(
          base,
          tibble(
            benchmark = custom_label,
            unit_cost_usd = custom_cost,
            estimated_units = units,
            interpretation = interpretation,
            note = "Custom benchmark."
          )
        )
      }
    }

    base |>
      select(-note, -estimated_units) |>
      mutate(
        unit_cost_usd = ifelse(
          is.na(unit_cost_usd),
          NA_character_,
          paste0(
            "$",
            trimws(formatC(round(as.numeric(unit_cost_usd), 0), format = "f", digits = 0, big.mark = ","))
          )
        )
      ) |>
      rename(
        Benchmark = benchmark,
        `Cost per Unit` = unit_cost_usd,
        `What That Means` = interpretation
      )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$translation_in_note <- renderUI({
    notes <- results()$translation_in$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$translation_out <- renderTable({
    base <- results()$translation_out

    custom_label <- trimws(input$custom_benchmark_out_label)
    custom_cost <- suppressWarnings(as.numeric(input$custom_benchmark_out_cost))
    if (!is.null(custom_label) && nzchar(custom_label) && !is.na(custom_cost) && custom_cost > 0) {
      total_outflow <- results()$spending$total_amount
      if (!is.na(total_outflow) && total_outflow > 0) {
        units <- floor(total_outflow / custom_cost)
        interpretation <- paste0(
          "At $", format(round(custom_cost, 2), big.mark = ","),
          " each, this equals about ", format(units, big.mark = ","),
          " units."
        )
        base <- bind_rows(
          base,
          tibble(
            benchmark = custom_label,
            unit_cost_usd = custom_cost,
            estimated_units = units,
            interpretation = interpretation,
            note = "Custom benchmark."
          )
        )
      }
    }

    base |>
      select(-note, -estimated_units) |>
      mutate(
        unit_cost_usd = ifelse(
          is.na(unit_cost_usd),
          NA_character_,
          paste0(
            "$",
            trimws(formatC(round(as.numeric(unit_cost_usd), 0), format = "f", digits = 0, big.mark = ","))
          )
        )
      ) |>
      rename(
        Benchmark = benchmark,
        `Cost per Unit` = unit_cost_usd,
        `What That Means` = interpretation
      )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$donor_mix <- renderDT({
    df <- results()$finance$donor_types
    if (nrow(df) == 0) {
      fallback <- results()$finance$summary |>
        filter(source %in% c("Individuals", "PACs")) |>
        transmute(donor_type = source, amount_usd = amount_usd, note = note)
      if (nrow(fallback) > 0) {
        df <- fallback
      } else {
        df <- data.frame(
          donor_type = "No data",
          amount_usd = NA_real_,
          note = "Check OpenFEC API key or candidate match."
        )
      }
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    if ("donor_type" %in% names(df)) {
      names(df)[names(df) == "donor_type"] <- "Donor Type"
    }
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
  })

  output$donor_mix_note <- renderUI({
    notes <- results()$finance$donor_types$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      fallback_notes <- results()$finance$summary |>
        filter(source %in% c("Individuals", "PACs")) |>
        pull(note)
      fallback_notes <- fallback_notes[!is.na(fallback_notes) & nzchar(fallback_notes)]
      notes <- fallback_notes
    }
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$donor_employers <- renderDT({
    df <- results()$finance$industries
    if (nrow(df) == 0) {
      df <- data.frame(
        industry = "No data",
        amount_usd = NA_real_,
        note = "Schedule A by employer returned no rows."
      )
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    if ("industry" %in% names(df)) {
      names(df)[names(df) == "industry"] <- "Industry"
    }
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
  })

  output$donor_employers_note <- renderUI({
    notes <- results()$finance$industries$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$donor_occupations <- renderDT({
    df <- results()$finance$occupations
    if (nrow(df) == 0) {
      df <- data.frame(
        occupation = "No data",
        amount_usd = NA_real_,
        note = "Schedule A by occupation returned no rows."
      )
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    if ("occupation" %in% names(df)) {
      names(df)[names(df) == "occupation"] <- "Occupation"
    }
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
  })

  output$donor_occupations_note <- renderUI({
    notes <- results()$finance$occupations$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$donor_pacs_all <- renderDT({
    df <- results()$finance$pacs_all
    if (nrow(df) == 0) {
      df <- data.frame(
        pac = "No data",
        amount_usd = NA_real_,
        note = "Schedule A PAC breakdown returned no rows."
      )
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    if ("pac" %in% names(df)) {
      names(df)[names(df) == "pac"] <- "PAC"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
  })

  output$donor_pacs_internal <- renderDT({
    df <- results()$finance$pacs_internal
    if (nrow(df) == 0) {
      df <- data.frame(
        pac = "No data",
        amount_usd = NA_real_,
        note = "No internal committee transfers found."
      )
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    if ("pac" %in% names(df)) {
      names(df)[names(df) == "pac"] <- "Committee"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
  })

  output$spending_agencies <- renderDT({
    df <- results()$spending$agencies
    if (nrow(df) == 0) {
      df <- data.frame(
        agency = "No data",
        amount_usd = NA_real_,
        note = "USAspending returned no rows."
      )
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    if ("agency" %in% names(df)) {
      names(df)[names(df) == "agency"] <- "Agency"
    }
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if ("Amount" %in% names(df)) {
      tbl <- tbl %>% formatCurrency("Amount", currency = "$", digits = 0)
    }
    tbl
  })

  output$spending_recipients <- renderDT({
    df <- results()$spending$recipients
    if (nrow(df) == 0) {
      df <- data.frame(
        recipient = "No data",
        amount_usd = NA_real_,
        note = "USAspending returned no rows."
      )
    }
    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    if ("recipient" %in% names(df)) {
      names(df)[names(df) == "recipient"] <- "Recipient"
    }
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if ("Amount" %in% names(df)) {
      tbl <- tbl %>% formatCurrency("Amount", currency = "$", digits = 0)
    }
    tbl
  })

  output$spending_geo_table <- renderDT({
    view <- input$geo_view
    if (is.null(view) || view == "") {
      view <- "County"
    }
    df <- if (identical(view, "District")) results()$spending$districts else results()$spending$counties
    label_col <- if (identical(view, "District")) "district" else "county"
    label_name <- if (identical(view, "District")) "District" else "County"

    if (nrow(df) == 0) {
      df <- data.frame(
        label = "No data",
        amount_usd = NA_real_,
        note = "USAspending returned no rows."
      )
    } else {
      df <- df |> rename(label = all_of(label_col))
    }

    if ("note" %in% names(df)) {
      df <- df %>% select(-note)
    }
    if ("label" %in% names(df)) {
      names(df)[names(df) == "label"] <- label_name
    }
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
    }
    tbl <- datatable(
      df,
      options = list(pageLength = 10, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE),
      rownames = FALSE
    )
    if ("Amount" %in% names(df)) {
      tbl <- tbl %>% formatCurrency("Amount", currency = "$", digits = 0)
    }
    tbl
  })

  output$spending_geo_map <- renderPlot({
    view <- input$geo_view
    if (is.null(view) || view == "") {
      view <- "County"
    }

    res <- results()
    if (identical(view, "District")) {
      df <- res$spending$districts
      if (nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5, "No district data available.", cex = 0.9)
        return()
      }
      if (!requireNamespace("tigris", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
        plot.new()
        text(0.5, 0.5, "Packages 'tigris' and 'sf' required for district map.", cex = 0.9)
        return()
      }

      df_use <- df
      if ("note" %in% names(df_use)) {
        df_use <- df_use %>% select(-note)
      }
      df_use <- df_use |>
        mutate(
          district_num = suppressWarnings(as.integer(gsub("^.*-0*", "", district)))
        ) |>
        filter(!is.na(district_num))

      state_abbrev <- res$state_selected
      if (is.null(state_abbrev) || is.na(state_abbrev) || state_abbrev == "") {
        plot.new()
        text(0.5, 0.5, "State not available for district map.", cex = 0.9)
        return()
      }

      shape <- get_district_shape_cached(state_abbrev)
      if (is.null(shape)) {
        plot.new()
        text(0.5, 0.5, "District map unavailable for state.", cex = 0.9)
        return()
      }

      cd_cols <- names(shape)[grepl("^CD\\d+FP$", names(shape))]
      cd_col <- if (length(cd_cols) > 0) cd_cols[[1]] else if ("CD" %in% names(shape)) "CD" else NA_character_
      if (is.na(cd_col)) {
        plot.new()
        text(0.5, 0.5, "District map unavailable.", cex = 0.9)
        return()
      }

      shape <- shape |>
        mutate(district_num = suppressWarnings(as.integer(.data[[cd_col]]))) |>
        left_join(df_use, by = "district_num")

      ggplot(shape) +
        geom_sf(aes(fill = amount_usd), color = "white", size = 0.15) +
        scale_fill_gradient(
          low = "#e7f5f2",
          high = "#00887c",
          labels = scales::label_dollar(accuracy = 1),
          na.value = "#f0f0f0"
        ) +
        labs(fill = "Amount") +
        theme_void(base_size = 11) +
        theme(legend.position = "right")
    } else {
      df <- res$spending$counties
      if (nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5, "No county data available.", cex = 0.9)
        return()
      }
      if (!requireNamespace("maps", quietly = TRUE)) {
        plot.new()
        text(0.5, 0.5, "Package 'maps' required for county map.", cex = 0.9)
        return()
      }

      df_use <- df
      if ("note" %in% names(df_use)) {
        df_use <- df_use %>% select(-note)
      }
      df_use <- df_use |>
        mutate(
          county_raw = tolower(county),
          county_raw = gsub("\\s+county.*$", "", county_raw),
          county_name = trimws(county_raw)
        )

      state_abbrev <- res$state_selected
      state_name <- state_lookup$state_name[state_lookup$state_abbrev == state_abbrev]
      if (length(state_name) == 0 || is.na(state_name[[1]])) {
        plot.new()
        text(0.5, 0.5, "State not available for county map.", cex = 0.9)
        return()
      }
      state_name <- tolower(state_name[[1]])

      map_df <- get_county_map_cached(state_name)
      if (nrow(map_df) == 0) {
        plot.new()
        text(0.5, 0.5, "County map unavailable for state.", cex = 0.9)
        return()
      }

      map_df <- map_df |>
        left_join(df_use, by = c("subregion" = "county_name"))

      ggplot(map_df, aes(long, lat, group = group, fill = amount_usd)) +
        geom_polygon(color = "white", size = 0.15) +
        coord_fixed(1.3) +
        scale_fill_gradient(
          low = "#e7f5f2",
          high = "#00887c",
          labels = scales::label_dollar(accuracy = 1),
          na.value = "#f0f0f0"
        ) +
        labs(fill = "Amount") +
        theme_void(base_size = 11) +
        theme(legend.position = "right")
    }
  })

  output$spending_geo_map_overview <- renderPlot({
    view <- input$geo_view_overview
    if (is.null(view) || view == "") {
      view <- "District"
    }

    res <- results()
    if (identical(view, "District")) {
      df <- res$spending$districts
      if (nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5, "No district data available.", cex = 0.9)
        return()
      }
      if (!requireNamespace("tigris", quietly = TRUE) || !requireNamespace("sf", quietly = TRUE)) {
        plot.new()
        text(0.5, 0.5, "Packages 'tigris' and 'sf' required for district map.", cex = 0.9)
        return()
      }

      df_use <- df
      if ("note" %in% names(df_use)) {
        df_use <- df_use %>% select(-note)
      }
      df_use <- df_use |>
        mutate(
          district_num = suppressWarnings(as.integer(gsub("^.*-0*", "", district)))
        ) |>
        filter(!is.na(district_num))

      state_abbrev <- res$state_selected
      if (is.null(state_abbrev) || is.na(state_abbrev) || state_abbrev == "") {
        plot.new()
        text(0.5, 0.5, "State not available for district map.", cex = 0.9)
        return()
      }

      shape <- get_district_shape_cached(state_abbrev)
      if (is.null(shape)) {
        plot.new()
        text(0.5, 0.5, "District map unavailable for state.", cex = 0.9)
        return()
      }

      cd_cols <- names(shape)[grepl("^CD\\d+FP$", names(shape))]
      cd_col <- if (length(cd_cols) > 0) cd_cols[[1]] else if ("CD" %in% names(shape)) "CD" else NA_character_
      if (is.na(cd_col)) {
        plot.new()
        text(0.5, 0.5, "District map unavailable.", cex = 0.9)
        return()
      }

      shape <- shape |>
        mutate(district_num = suppressWarnings(as.integer(.data[[cd_col]]))) |>
        left_join(df_use, by = "district_num")

      ggplot(shape) +
        geom_sf(aes(fill = amount_usd), color = "white", size = 0.15) +
        scale_fill_gradient(
          low = "#e7f5f2",
          high = "#00887c",
          labels = scales::label_dollar(accuracy = 1),
          na.value = "#f0f0f0"
        ) +
        labs(fill = "Amount") +
        theme_void(base_size = 11) +
        theme(legend.position = "right")
    } else {
      df <- res$spending$counties
      if (nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5, "No county data available.", cex = 0.9)
        return()
      }
      if (!requireNamespace("maps", quietly = TRUE)) {
        plot.new()
        text(0.5, 0.5, "Package 'maps' required for county map.", cex = 0.9)
        return()
      }

      df_use <- df
      if ("note" %in% names(df_use)) {
        df_use <- df_use %>% select(-note)
      }
      df_use <- df_use |>
        mutate(
          county_raw = tolower(county),
          county_raw = gsub("\\s+county.*$", "", county_raw),
          county_name = trimws(county_raw)
        )

      state_abbrev <- res$state_selected
      state_name <- state_lookup$state_name[state_lookup$state_abbrev == state_abbrev]
      if (length(state_name) == 0 || is.na(state_name[[1]])) {
        plot.new()
        text(0.5, 0.5, "State not available for county map.", cex = 0.9)
        return()
      }
      state_name <- tolower(state_name[[1]])

      map_df <- get_county_map_cached(state_name)
      if (nrow(map_df) == 0) {
        plot.new()
        text(0.5, 0.5, "County map unavailable for state.", cex = 0.9)
        return()
      }

      map_df <- map_df |>
        left_join(df_use, by = c("subregion" = "county_name"))

      ggplot(map_df, aes(long, lat, group = group, fill = amount_usd)) +
        geom_polygon(color = "white", size = 0.15) +
        coord_fixed(1.3) +
        scale_fill_gradient(
          low = "#e7f5f2",
          high = "#00887c",
          labels = scales::label_dollar(accuracy = 1),
          na.value = "#f0f0f0"
        ) +
        labs(fill = "Amount") +
        theme_void(base_size = 11) +
        theme(legend.position = "right")
    }
  })

  output$recipients_note <- renderUI({
    notes <- results()$spending$recipients$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$agencies_note <- renderUI({
    notes <- results()$spending$agencies$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$geo_table_note <- renderUI({
    view <- input$geo_view
    notes <- if (identical(view, "District")) {
      results()$spending$districts$note
    } else {
      results()$spending$counties$note
    }
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$geo_note <- renderUI({
    view <- input$geo_view
    notes <- if (identical(view, "District")) {
      results()$spending$districts$note
    } else {
      results()$spending$counties$note
    }
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$geo_note_overview <- renderUI({
    view <- input$geo_view_overview
    notes <- if (identical(view, "District")) {
      results()$spending$districts$note
    } else {
      results()$spending$counties$note
    }
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  output$translation_out_note <- renderUI({
    notes <- results()$translation_out$note
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes) == 0) {
      NULL
    } else {
      tags$div(style = "color:#666;font-size:0.85em;", notes[[1]])
    }
  })

  make_barh_plot <- function(df, label_col, amount_col = "amount_usd", title = NULL, color = "#00887c") {
    if (is.null(df) || nrow(df) == 0 || !(label_col %in% names(df)) || !(amount_col %in% names(df))) {
      plot.new()
      text(0.5, 0.5, "No data available.", cex = 0.9)
      return(invisible(NULL))
    }
    df <- df |>
      filter(!is.na(.data[[label_col]]), .data[[label_col]] != "", !is.na(.data[[amount_col]])) |>
      arrange(.data[[amount_col]]) |>
      mutate(label = factor(.data[[label_col]], levels = .data[[label_col]]))

    ggplot(df, aes(x = label, y = .data[[amount_col]])) +
      geom_col(fill = color, width = 0.6) +
      coord_flip() +
      scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
      labs(x = NULL, y = NULL, title = title) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#e5e7eb", linewidth = 0.3)
      )
  }

  output$receipts_trend_donor_types <- renderPlot({
    df <- results()$finance$donor_types
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No campaign finance records available.", cex = 0.9)
      return()
    }
    make_barh_plot(df, "donor_type")
  })

  output$receipts_trend_donor_types_overview <- renderPlot({
    df <- results()$finance$donor_types
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No campaign finance records available.", cex = 0.9)
      return()
    }
    make_barh_plot(df, "donor_type")
  })

  output$receipts_trend_employers <- renderPlot({
    make_barh_plot(results()$finance$industries, "industry")
  })

  output$receipts_trend_employers_overview <- renderPlot({
    make_barh_plot(results()$finance$industries, "industry")
  })

  output$receipts_trend_occupations <- renderPlot({
    make_barh_plot(results()$finance$occupations, "occupation")
  })

  output$receipts_trend_occupations_overview <- renderPlot({
    make_barh_plot(results()$finance$occupations, "occupation")
  })

  output$receipts_trend_internal <- renderPlot({
    make_barh_plot(results()$finance$pacs_internal, "pac")
  })

  output$receipts_trend_internal_overview <- renderPlot({
    make_barh_plot(results()$finance$pacs_internal, "pac")
  })

  output$member_debug <- renderText({
    st <- normalize_state_abbrev(input$state)
    st_name <- state_lookup$state_name[state_lookup$state_abbrev == st]
    if (length(st_name) == 0) {
      st_name <- NA_character_
    }
    total_members <- nrow(legislators_ref)
    state_members <- if (!is.na(st)) {
      sum(legislators_ref$state == toupper(st), na.rm = TRUE)
    } else {
      NA_integer_
    }
    sample_names <- legislators_ref |>
      filter(state == toupper(st)) |>
      pull(legislator_clean) |>
      unique() |>
      sort()
    sample_names <- head(sample_names, 10)
    paste0(
      "Total cached members: ", total_members, "\n",
      "Selected state: ", st_name, " (", st, ")\n",
      "Members in state: ", state_members, "\n",
      "Sample names: ", paste(sample_names, collapse = ", ")
    )
  })

}

shinyApp(ui, server)
