#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(gargle)
library(googledrive)
library(googlesheets4)

# Define UI for application
ui <- fluidPage(
  titlePanel("Stackelberg Duopoly Game"),
  sidebarPanel(
    textInput(
      inputId = "roster",
      label = "Enter the ID of the Google Sheet with the names of the participants.",
      value = NULL
    ),
    selectInput(
      inputId = "partners",
      label = "Select whether groups are assigned at random or by student choice.",
      choices = list("Random" = "random", "Students" = "students")
    ),
    textInput(
      inputId = "sheet",
      label = "Enter the ID of the Google Sheet with the output.",
      value = NULL
    ),
    numericInput(
      inputId = "round",
      label = "Enter the round you want to calculate.",
      value = 1
    ),
    numericInput(
      inputId = "a",
      label = "Enter the intercept of the inverse demand function.",
      value = 10
    ),
    numericInput(
      inputId = "b",
      label = "Enter the slope of the inverse demand function.",
      value = -1
    ),
    numericInput(
      inputId = "c",
      label = "Enter the marginal cost of the output.",
      value = 6
    ),
    numericInput(
      inputId = "f",
      label = "Enter the fixed cost of the output.",
      value = 0
    )
  ),
  mainPanel(tabsetPanel(
    tabPanel("Outcomes",
             tabsetPanel(
               tabPanel(
                 "Tables",
                 tableOutput("payoffMatrix"),
                 tableOutput("outputMatrix"),
                 tableOutput("priceMatrix")
               ),
               tabPanel("Tree", plotOutput("treePlot"))
             )),
    tabPanel("Roles", tableOutput("roles")),
    tabPanel("Results",
             tabsetPanel(
               tabPanel("Leaders", tableOutput("leaderResults")),
               tabPanel("Followers", tableOutput("followerResults")),
               tabPanel("All", tableOutput("results")),
               tabPanel("Plot", plotOutput("outcomePlot")),
               tabPanel("Grades", tableOutput("grades"))
             ))
  ))
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    roster <- input$roster
    sheet <- input$sheet
    round <- input$round
    partners <- input$partners
    a <- input$a
    b <- input$b
    c <- input$c
    f <- input$f
    g = stackelbergGame(
      leaderSheet = input$leaderSheet,
      resultsSheet = input$resultsSheet,
      a = a,
      b = b,
      c = c,
      f = f,
      partners = partners,
      names = NULL
  )
  g
  })

  output$treePlot <- renderPlot({
      g <- data()
      g$tree}, width = 600)

  output$roles <- renderTable({
      g <- data()
      g$roles}, align = 'c')

  output$leaderResults <- renderTable({
      g <- data()
      g$leaderResults}, align = 'c')

  output$followResults <- renderTable({
      g <- data()
      g$followResults}, align = 'c')

  output$results <- renderTable({
      g <- data()
      g$results}, align = 'c')

  output$payoffMatrix <- renderTable({
      g <- data()
      g$payoff}, rownames = TRUE, align = 'lcc',
        caption = "Student Payoff Matrix", caption.placement = "top")

  output$outputMatrix <- renderTable({
      g <- data()
      g$output}, rownames = TRUE, align = 'lcc',
        caption = "Quantity Outcome Matrix", caption.placement = "top")

  output$priceMatrix <- renderTable({
      g <- data()
      g$price}, rownames = TRUE, align = 'lcc',
        caption = "Price Outcome Matrix", caption.placement = "top")

  output$outcomePlot <- renderPlot({
      g <- data()
      plot(g, round = input$round)}, width = 600)

  output$grades <- renderTable({
      g <- data()
      g$grades}, align = 'c')
}

# Run the application
shinyApp(ui = ui, server = server)
