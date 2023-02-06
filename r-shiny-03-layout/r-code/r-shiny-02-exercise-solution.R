# File     : r-shiny-02-exercise.R
# Title    : Chapter 2 exercise
# Author   : Ming-Chang Lee
# YouTube  : https://www.youtube.com/@alan9956
# RWEPA    : http://rwepa.blogspot.tw/
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com
# Encoding : UTF-8

library(shiny)

# 定義UI
ui <- fluidPage(# Application title
  titlePanel("RWEPA | R - shiny Chapter 2 Exercise"),
  
  # 建立側邊佈置
  sidebarLayout(
    
    # 建立下拉式選單 selectinput
    sidebarPanel(
      
      # selectInput(
      #   inputId = "variable1",
      #   label = "Choose a variable:",
      #   choices = names(iris)[-5]
      # ),
      
      # 使用 varSelectInput 建立變數的下拉式選單
      varSelectInput("variable", "Choose a variable::", iris[-5]),
      
      # 滑桿 sliderInput
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30,
        animate = TRUE
      ),
      
      # 核取方塊 checkboxInput
      checkboxInput("density", "Density(Y/N)")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("pairsPlot"),
              plotOutput("distPlot"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # 測試shiny程式
  # input <- list()
  # input$bins <- 30
  # input$variable <- "Sepal.Length"
  # input$density <- TRUE
  
  output$pairsPlot <- renderPlot({
    pairs(iris[-5], pch = 16, col = iris$Species)
  })
  
  output$distPlot <- renderPlot({
    
    # 檢查使用者是否有選取變數,如果沒有選取, 以下程式不會執行
    req(input$variable)
    
    # 解析選取的變數
    # x <- input$variable # 結果會有Error
    x <- eval(parse(text = paste0("iris$", input$variable)))
    bins <- seq(from = min(x),
                to = max(x),
                length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      prob = TRUE,
      col = "dodgerblue",
      border = "white",
      xlab = input$variable,
      main = "Histogram plot"
    )
    
    if (input$density == TRUE) {
      lines(density(x),
            lwd = 2,
            col = "chocolate3")
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
