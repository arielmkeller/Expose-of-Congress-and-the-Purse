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

ui <- fluidPage(
  titlePanel("Expose of Congress and the Purse"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("legislator", "Legislator", choices = NULL, multiple = FALSE),
      numericInput("cycle", "Election cycle", value = 2024, min = 2000, step = 2),
      actionButton("run", "Analyze")
    ),
    mainPanel(
      h2("Module 1: Money into politics"),
      h3("Member profile"),
      tableOutput("member_profile"),
      h3("Key totals"),
      tableOutput("totals_in"),
      h3("Funding translation"),
      tableOutput("translation_in"),
      h3("Donor mix"),
      DTOutput("donor_mix"),
      h3("Top donor employers (proxy for industry)"),
      DTOutput("donor_employers"),
      h3("Top donor occupations"),
      DTOutput("donor_occupations"),
      h3("Receipts trend"),
      plotOutput("receipts_trend", height = 260),
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
      h3("Diagnostics"),
      verbatimTextOutput("diagnostics")
    )
  )
)

server <- function(input, output, session) {
  legislators_ref <- get_legislators_reference()
  updateSelectizeInput(
    session,
    "legislator",
    choices = legislators_ref$legislator,
    selected = legislators_ref$legislator[[1]],
    server = TRUE
  )

  selected_member <- reactive({
    req(input$legislator)
    legislators_ref |>
      filter(legislator == input$legislator) |>
      slice(1)
  })

  results <- eventReactive(input$run, {
    legislator <- trimws(input$legislator)
    shiny::validate(shiny::need(nchar(legislator) > 0, "Enter a legislator name."))
    member <- selected_member()
    chamber <- ifelse(nrow(member) > 0, member$chamber[[1]], NA_character_)
    member_state <- ifelse(nrow(member) > 0, member$state[[1]], NA_character_)

    finance <- get_openfec_summary(
      legislator,
      cycle = input$cycle,
      chamber = chamber,
      state = member_state,
      district = ifelse(nrow(member) > 0 && "district" %in% names(member), member$district[[1]], NA_character_)
    )
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
      legislator = legislator,
      member = member,
      finance = finance,
      spending = spending,
      translation_in = translation_in,
      translation_out = translation_out
    )
  })

  output$member_profile <- renderTable({
    member <- selected_member()
    shiny::validate(shiny::need(nrow(member) > 0, "Member profile unavailable."))
    member |>
      transmute(
        legislator = legislator,
        political_affiliation = party,
        state = state,
        chamber = chamber,
        district = if ("district" %in% names(member)) district else NA_character_
      )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$totals_in <- renderTable({
    x <- results()
    data.frame(
      metric = c("Legislator", "Campaign receipts (USD)"),
      value = c(
        x$legislator,
        format(round({
          receipts_total <- x$finance$summary |>
            filter(source == "Total receipts") |>
            summarize(total = sum(amount_usd, na.rm = TRUE)) |>
            pull(total)
          if (length(receipts_total) == 0 || is.na(receipts_total)) {
            receipts_total <- sum(x$finance$summary$amount_usd, na.rm = TRUE)
          }
          receipts_total
        }, 2), big.mark = ",")
      )
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$totals_out <- renderTable({
    x <- results()
    data.frame(
      metric = c("Legislator", "Total outflow in scope (USD)"),
      value = c(
        x$legislator,
        format(round(x$spending$total_amount, 2), big.mark = ",")
      )
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$translation_in <- renderTable({
    results()$translation_in
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
    datatable(df, options = list(pageLength = 5))
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
    datatable(df, options = list(pageLength = 5))
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
    datatable(df, options = list(pageLength = 5))
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

  output$diagnostics <- renderText({
    x <- results()
    fec_diag <- x$finance$diagnostics
    spend_diag <- x$spending$diagnostics
    lines <- c(
      "OpenFEC diagnostics:",
      paste0("api_key_present: ", fec_diag$api_key_present),
      paste0("candidate_id: ", fec_diag$candidate_id),
      paste0("cycle_used: ", fec_diag$cycle_used),
      paste0("principal_committees: ", paste(fec_diag$principal_committees, collapse = ", ")),
      paste0("state_used: ", ifelse(is.null(fec_diag$state_used), "", fec_diag$state_used)),
      paste0("error: ", ifelse(is.null(fec_diag$error), "", fec_diag$error)),
      if (!is.null(fec_diag$candidate_options) && length(fec_diag$candidate_options) > 0) {
        c(
          "candidate_options:",
          paste0("  - ", fec_diag$candidate_options)
        )
      } else {
        NULL
      },
      "",
      "USAspending diagnostics:",
      paste0("state: ", spend_diag$state),
      paste0("district: ", spend_diag$district),
      paste0("status_agencies: ", spend_diag$status_agencies),
      paste0("status_recipients: ", spend_diag$status_recipients),
      paste0("geo_failed_agencies: ", spend_diag$geo_failed_agencies),
      paste0("geo_failed_recipients: ", spend_diag$geo_failed_recipients)
    )
    paste(lines, collapse = "\n")
  })
}

shinyApp(ui, server)
