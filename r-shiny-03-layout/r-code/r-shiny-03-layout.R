# File     : r-shiny03-layout.R
# Title    : RWEPA | R - 第3集 shiny 佈置
# Date     : 2023.02.05
# Author   : Ming-Chang Lee
# RWEPA    : http://rwepa.blogspot.tw/
# YouTube  : https://www.youtube.com/@alan9956
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com

library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny Text-tabsetPanel"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "選取資料集:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "資料顯示筆數:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Using tabsetPanel
      tabsetPanel(
        
        tabPanel("資料摘要", verbatimTextOutput("summary")),
        
        tabPanel("表格", tableOutput("table"))
        
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$table <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)