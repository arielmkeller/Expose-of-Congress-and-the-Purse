library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

source("R/openfec.R")
source("R/usaspending.R")
source("R/earmarks.R")
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
      h3("Member profile"),
      tableOutput("member_profile"),
      h3("Key totals"),
      tableOutput("totals"),
      h3("Funding translation"),
      tableOutput("translation"),
      h3("Earmark categories"),
      plotOutput("earmark_plot", height = 300),
      h3("Campaign finance summary"),
      DTOutput("finance_table"),
      h3("USAspending context"),
      DTOutput("spending_table")
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
    chamber <- ifelse(nrow(member) > 0, member$chamber[[1]], "House")

    earmarks <- get_earmarks_for_legislator(legislator)
    finance <- get_openfec_summary(legislator, cycle = input$cycle, chamber = chamber)
    spending <- get_usaspending_context(earmarks)

    total_earmarks <- sum(earmarks$amount_usd, na.rm = TRUE)
    translation <- translate_dollars(total_earmarks)

    list(
      legislator = legislator,
      member = member,
      earmarks = earmarks,
      finance = finance,
      spending = spending,
      total_earmarks = total_earmarks,
      translation = translation
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
        chamber = chamber
      )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$totals <- renderTable({
    x <- results()
    data.frame(
      metric = c("Legislator", "Total earmarked funding (USD)", "Campaign receipts (USD)"),
      value = c(
        x$legislator,
        format(round(x$total_earmarks, 2), big.mark = ","),
        format(round(sum(x$finance$amount_usd, na.rm = TRUE), 2), big.mark = ",")
      )
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$translation <- renderTable({
    results()$translation
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$earmark_plot <- renderPlot({
    x <- results()$earmarks
    shiny::validate(shiny::need(nrow(x) > 0, "No earmark records available for this legislator yet."))

    ggplot(x, aes(x = reorder(project_type, amount_usd, FUN = sum), y = amount_usd)) +
      geom_col(fill = "#1f77b4") +
      coord_flip() +
      labs(x = "Project type", y = "Amount (USD)") +
      theme_minimal(base_size = 12)
  })

  output$finance_table <- renderDT({
    datatable(results()$finance, options = list(pageLength = 5))
  })

  output$spending_table <- renderDT({
    datatable(results()$spending, options = list(pageLength = 5))
  })
}

shinyApp(ui, server)
