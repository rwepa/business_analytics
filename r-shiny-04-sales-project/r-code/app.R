# File     : app.R
# Title    : RWEPA | Shiny - Sales Dashboard 
# Author   : Ming-Chang Lee
# YouTube  : https://www.youtube.com/@alan9956
# RWEPA    : http://rwepa.blogspot.tw/
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com

library(shiny)   # shinyApp
library(dplyr)   # mutate_if, mutate, filter
library(DT)      # datatable
library(ggplot2) # ggplot
library(plotly)  # plot_ly

# 線上銷售儀表板App
# https://rwepa.shinyapps.io/shinySalesDashboard/
  
# 匯入資料
# library(readxl)  # read_excel
# https://github.com/rwepa/DataDemo/blob/master/OnlineRetail.xlsx
# sales <- read_excel(path="data/OnlineRetail.xlsx") # 匯入時間 < 10 seconds
# sales # 541,909 × 8

# 資料轉換
# sales <- sales %>%
#   mutate_if(is.character, as.factor) %>%   # 轉換 character 為 factor
#   mutate(Amount=Quantity*UnitPrice) %>%    # 新增 Amount=Quantity*UnitPrice
#   filter(Quantity > 0 & UnitPrice > 0) %>% # 篩選 Quantity > 0 & UnitPrice > 0
#   na.omit()                                # 刪除 NA

# summary(sales)
# sale 397,884 × 9

# save(sales, file = "data/sales.RData")

load("data/sales.RData")

# 定義銷售儀表板UI
ui <- fluidPage(
  
  # App標題
  titlePanel("RWEPA | Sales Dashboard-v.23.02.14, RWEPA: http://rwepa.blogspot.com/"),
  
  # 設定主題色彩
  theme = shinythemes::shinytheme("cerulean"),
  
  # 側邊佈置
  sidebarLayout(
    
    # 輸入用側邊面板
    sidebarPanel(
      
      # 輸入: 選取國家
      # levels(sales$Country) = 38
      selectInput(inputId = "selectinputCountry", 
                  label = "選取國家:",
                  choices = c("ALL",levels(sales$Country)),
                  selected = "ALL"),
      
      # br() 空白列
      br(),
      
      # 輸入: 篩選銷售金額
      sliderInput(inputId = "sliderInputAmount",
                  label = "篩選銷售金額:",
                  min = 0,
                  max = 200000,
                  value = c(0, 200000))
    ),
    
    # 輸出用側邊面板
    mainPanel(
      
      # 輸出: 使用 tabsetPanel
      tabsetPanel(type = "tabs",
                  tabPanel("資料摘要", verbatimTextOutput("SaleSummary"), textOutput("SaleDimension")),
                  tabPanel("資料明細", DTOutput("SaleTable")),
                  tabPanel("國家別銷售統計表", DTOutput("SaleCountry")),
                  tabPanel("國家別銷售統計圖", plotOutput("SaleAmountBarplot")),
                  tabPanel("銷售趨勢圖", 
                           radioButtons("radioButtonsDate", 
                                        label ="選取日期單位",
                                        choices = list("日" = "day", "週" = "week", "月" = "month"),
                                        inline = TRUE),
                           plotlyOutput("SaleAmountInterativePlot"))
      )
    )
  )
)

# 定義銷售儀表板server
server <- function(input, output) {
  
  # 測試資料
  # input <- list(selectinputCountry = NA, sliderInputAmount = NA)
  # input$selectinputCountry <- "ALL"
  # input$selectinputCountry <- "United Kingdom"
  # input$sliderInputAmount <- c(0, 150000)
  
  # 反應表示式(Reactive expression): 判斷資料篩選結果
  selected <- reactive({
    
    if (input$selectinputCountry == "ALL") {         # ALL 表示選取所有國家
      df <- sales %>%
        filter(Amount >= input$sliderInputAmount[1], # filter 使用逗號表示 & (AND運算)
               Amount <= input$sliderInputAmount[2])
    } else {
      df <- sales %>%
        filter(Amount >= input$sliderInputAmount[1], 
               Amount <= input$sliderInputAmount[2],
               Country == input$selectinputCountry)
      
    }
    
  })
  
  # 反應表示式: 計算各國家銷售金額總計, 結果為 Country, TotalAmount 
  pivoted <- reactive({
    
    df <- selected() %>%
      count(Country, wt = Amount, sort = TRUE) %>%
      rename(TotalAmount = n)
    
  })
  
  # 1.資料摘要
  output$SaleSummary <- renderPrint({
    summary(selected())
  })
  
  output$SaleDimension <- renderPrint({
    cat(paste0("資料筆數: ", nrow(selected()), ", 變數個數: ", ncol(selected())))
  })
  
  # 2.資料明細
  output$SaleTable <- renderDT({
    datatable(selected(), colnames = c('ID' = 1), extensions = 'Buttons', options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 20, 100),
      columnDefs = list(list(orderable = TRUE, targets = 0)),
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf'))) %>%
      formatRound("Amount", digits = 2) %>%
      formatDate("InvoiceDate", method = "toLocaleString")
  })
  
  # 3.國家別銷售統計表
  output$SaleCountry <- renderDT({
    
    datatable(pivoted(),
              rownames = FALSE,
              extensions = 'Buttons', 
              options = list(
                pageLength = 100,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf'))) %>%
      formatRound("TotalAmount", digits = 2)
    
  })
  
  # 4.國家別銷售統計圖
  output$SaleAmountBarplot <- renderPlot({
    
    ggplot(data=pivoted(), aes(x = reorder(Country, TotalAmount), y=TotalAmount)) + # reorder 排序
      geom_bar(stat="identity", fill="steelblue") +
      coord_flip() # 水平長條圖
    
  }, height = 600)
  
  # 5.每月銷售趨勢圖
  dateselected <- reactive({
    
    switch(input$radioButtonsDate,
           "day" = "day",
           "week" = "week",
           "month" = "month")
    
  })
  
  output$SaleAmountInterativePlot <- renderPlotly({
    
    mydf <- selected() %>%
      group_by(Date = cut(InvoiceDate, dateselected())) %>% summarise(Amount = sum(Amount))
    
    plot_ly(mydf,
            x = ~Date, 
            y = ~Amount, 
            type = 'scatter', mode = 'lines+markers', height = 550) %>%
      layout(xaxis = list(title = input$radioButtonsDate))

  })
  
}

# 執行 Shiny app
shinyApp(ui, server)
# end
