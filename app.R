library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

source("R/openfec.R")
source("R/usaspending.R")
source("R/earmarks.R")
source("R/translation.R")

ui <- fluidPage(
  titlePanel("Expose of Congress and the Purse"),
  sidebarLayout(
    sidebarPanel(
      textInput("legislator", "Legislator name", value = "Alexandria Ocasio-Cortez"),
      selectInput("chamber", "Chamber", choices = c("House", "Senate"), selected = "House"),
      numericInput("cycle", "Election cycle", value = 2024, min = 2000, step = 2),
      actionButton("run", "Analyze")
    ),
    mainPanel(
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
  results <- eventReactive(input$run, {
    legislator <- trimws(input$legislator)
    shiny::validate(shiny::need(nchar(legislator) > 0, "Enter a legislator name."))

    earmarks <- get_earmarks_for_legislator(legislator)
    finance <- get_openfec_summary(legislator, cycle = input$cycle, chamber = input$chamber)
    spending <- get_usaspending_context(earmarks)

    total_earmarks <- sum(earmarks$amount_usd, na.rm = TRUE)
    translation <- translate_dollars(total_earmarks)

    list(
      legislator = legislator,
      earmarks = earmarks,
      finance = finance,
      spending = spending,
      total_earmarks = total_earmarks,
      translation = translation
    )
  })

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
