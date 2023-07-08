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
    if(is.null(data()$g)) {gameTree(
      players = c("Leader", "Follower"),
      payoffs1 = c(2, 1.5, 2, 2),
      payoffs2 = c(2, 2.25, 1, 1))}
    else{
      g <- data()
      g$tree}}, width = 600)

  output$roles <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$roles}}, align = 'c')

  output$leaderResults <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$leaderResults}}, align = 'c')

  output$followResults <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$followResults}}, align = 'c')

  output$results <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$results}}, align = 'c')

  output$payoffMatrix <- renderTable({
    if(is.null(data()$g)) {
      data.frame(
        Follower.Defect = c("(2, 1)", "(1.5, 2.25)"),
        Follower.Collude = c("(2, 1)", "(2, 2)"),
        row.names = c("Leader.Defect", "Leader.Collude")
      )
    }
    else{
      g <- data()
      g$payoff}}, rownames = TRUE, align = 'lcc',
        caption = "Student Payoff Matrix", caption.placement = "top")

  output$outputMatrix <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$output}}, rownames = TRUE, align = 'lcc',
        caption = "Quantity Outcome Matrix", caption.placement = "top")

  output$priceMatrix <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$price}}, rownames = TRUE, align = 'lcc',
        caption = "Price Outcome Matrix", caption.placement = "top")

  output$outcomePlot <- renderPlot({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      plot(g, round = input$round)}}, width = 600)

  output$grades <- renderTable({
    if(is.null(data()$g)) {}
    else{
      g <- data()
      g$grades}}, align = 'c')

# Run the application
shinyApp(ui = ui, server = server)
