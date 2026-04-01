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

ui <- fluidPage(
  titlePanel("Congress, Cash, & Constituents: Where the Money Goes"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("state", "State", choices = NULL, multiple = FALSE),
      uiOutput("legislator_ui"),
      numericInput("cycle", "Election cycle", value = 2024, min = 2000, step = 2),
      actionButton("run", "Analyze")
    ),
    mainPanel(
      h3("Member of Congress Profile"),
      tableOutput("member_profile"),
      h2("Cash In — Campaign Money by Member"),
      h3("Campaign Receipts"),
      textOutput("totals_in"),
      h3("Funding Breakdown"),
      DTOutput("donor_mix"),
      h3("Top Donor Employers"),
      DTOutput("donor_employers"),
      h3("Top Donor Occupations"),
      DTOutput("donor_occupations"),
      h3("Receipts trend"),
      plotOutput("receipts_trend", height = 260),
      h3("Putting Campaign Cash in Context"),
      tableOutput("translation_in"),
      hr(),
      h2("Module 2: Money out into communities"),
      h3("Outflow totals"),
      tableOutput("totals_out"),
      h3("Outflow translation"),
      tableOutput("translation_out"),
      h3("Top recipients"),
      DTOutput("spending_recipients"),
      h3("Agency flow"),
      DTOutput("spending_agencies"),
      hr(),
      h3("Member list diagnostics"),
      verbatimTextOutput("member_debug")
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
      trend = tibble(period = character(), amount_usd = numeric(), note = "")
    )
  }

  results <- eventReactive(input$run, {
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

    district_value <- ifelse(nrow(member) > 0 && "district" %in% names(member), member$district[[1]], NA_character_)
    spending <- get_usaspending_context(state = member_state, district = district_value)
    receipts_total <- finance$summary |>
      filter(source == "Total receipts") |>
      summarize(total = sum(amount_usd, na.rm = TRUE)) |>
      pull(total)
    if (length(receipts_total) == 0 || is.na(receipts_total)) {
      receipts_total <- sum(finance$summary$amount_usd, na.rm = TRUE)
    }

    translation_in <- translate_inflow(suppressWarnings(as.numeric(receipts_total)))
    translation_out <- translate_outflow(spending$total_amount)

    list(
      legislator = ifelse(is.null(legislator) || legislator == "", paste0("State: ", state_name_selected), legislator),
      state_selected = state_selected,
      member = member,
      finance = finance,
      spending = spending,
      translation_in = translation_in,
      translation_out = translation_out
    )
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

  output$totals_in <- renderText({
    x <- results()
    legis_sel <- get_single(input$legislator)
    if (is.null(legis_sel) || is.na(legis_sel) || legis_sel == "") {
      "Select a Member of Congress"
    } else {
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
  })

  output$totals_out <- renderTable({
    x <- results()
    data.frame(
      metric = c("Member of Congress", "Total outflow in scope (USD)"),
      value = c(
        x$legislator,
        format(round(x$spending$total_amount, 2), big.mark = ",")
      )
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$translation_in <- renderTable({
    results()$translation_in |>
      select(-note) |>
      rename(
        Benchmark = benchmark,
        `Cost per Unit ($)` = unit_cost_usd,
        `How Many Units?` = estimated_units,
        `What That Means` = interpretation
      )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$translation_out <- renderTable({
    results()$translation_out
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
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
      rownames = FALSE
    )
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
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
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    tbl <- datatable(df, options = list(pageLength = 5), rownames = FALSE)
    if (!is.null(amount_col)) {
      tbl <- tbl %>% formatCurrency(amount_col, currency = "$", digits = 0)
    }
    tbl
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
    amount_col <- NULL
    if ("amount_usd" %in% names(df)) {
      names(df)[names(df) == "amount_usd"] <- "Amount"
      amount_col <- "Amount"
    }
    tbl <- datatable(df, options = list(pageLength = 5), rownames = FALSE)
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
    datatable(df, options = list(pageLength = 5))
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
    datatable(df, options = list(pageLength = 5))
  })

  output$receipts_trend <- renderPlot({
    x <- results()$finance$trend
    shiny::validate(shiny::need(nrow(x) > 0, "No campaign finance records available."))

    ggplot(x, aes(x = factor(period, levels = unique(period)), y = amount_usd)) +
      geom_col(fill = "#0b7285") +
      labs(x = NULL, y = "Amount (USD)") +
      theme_minimal(base_size = 12)
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
