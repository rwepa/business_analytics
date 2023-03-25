# File     : app.R
# Title    : RWEPA | Shiny - Human Resource Teller Dashboard 
# Author   : Ming-Chang Lee
# Date     : 2023.3.25
# YouTube  : https://www.youtube.com/@alan9956
# RWEPA    : http://rwepa.blogspot.tw/
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com

########################################
# part 1. 套件與資料處理
########################################

# 關閉套件訊息功能
# suppressPackageStartupMessages(library(dplyr))
# suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
# suppressPackageStartupMessages(library(ggcorrplot))
# suppressPackageStartupMessages(library(randomForest))
# suppressPackageStartupMessages(library(caret))
# suppressPackageStartupMessages(library(pROC))

library(readr)          # read_csv
library(DT)             # datatable
library(dplyr)          # mutate_if, mutate, group_by, summarise, select, rename_with
library(ggplot2)        # ggplot
library(ggcorrplot)     # ggcorrplot
library(randomForest)   # randomForest, importance, varImpPlot
library(caret)          # trainControl, train
library(pROC)           # plot.roc
library(shiny)          # ui server, shinyApp
library(shinydashboard) # dashboardPage, dashboardHeader

# 載入人力資源資料集
urls <- "https://raw.githubusercontent.com/rwepa/DataDemo/master/human_resource.csv"
df <- read_csv(urls)

old_names <- names(df)
new_names <- c("考核分數", "每年專案個數", "每月工作小時", "辨公時間", "工安意外", "工作滿意度", "離職", "升遷", "部門", "薪資")

# 資料轉換
df <- df %>%
  mutate_if(is.character, as.factor) %>% # 將 character 轉換為 factor
  mutate(Work_accident=factor(Work_accident, labels=c("No","Yes"))) %>% # 0: No, 1: Yes
  mutate(left=factor(left, labels=c("No","Yes"))) %>% # 0: No, 1: Yes
  mutate(promotion_last_5years=factor(promotion_last_5years, labels=c("No","Yes"))) %>% # 0: No, 1: Yes
  relocate(left, .after = last_col()) %>% # locate left to last column
  rename_with(~ new_names, all_of(old_names)) # rename all columns

# 資料結構
# str(df) # left: num數值, role, salary: chr字元

# 資料摘要
# summary(df) # 資料沒有NA

# 考核分數     0.01 ~ 1.00
# 每年專案個數 1 ~ 10
# 每月工作小時 80 ~ 350
# 辨公時間     1 ~ 10
# 工安意外     Yes, No
# 工作滿意度   0.01 ~ 1.00
# 升遷         Yes, No
# 部門         "accounting", "hr", "IT", "management", "marketing", "product_mng", "RandD", "sales", "support", "technical"
# 薪資         "high", "low", "medium"

myrole <- c("accounting", 
            "hr",
            "IT", 
            "management", 
            "marketing", 
            "product_mng", 
            "RandD", 
            "sales", 
            "support", 
            "technical")

names(myrole) <- myrole

# 使用隨機森林(random forest)
df_rf <- randomForest(離職~., data=df)

# 特徵重要性(feature importance)
# importance(df_rf)

# 建立預測模型

# step1. 建立訓練集(2/3),測試集(1/3)
set.seed(168)
trainIndex <- createDataPartition(df$離職, p = 0.7, list = FALSE)
train <- df[trainIndex,]  # 使用負號
test <- df[-trainIndex,]

# step2. 建立監督式學習模型
set.seed(168)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE,
  savePredictions = TRUE)

myformula <- as.formula("離職 ~ .")

model <- function(usedmethod, data_train, data_test) {
  
  # 使用 train 進行模型訓練
  tmp <- train(myformula, 
               data = data_train, 
               trControl = fitControl, 
               method = usedmethod)
  
  # 技巧: 使用list回傳模型結果與正確率
  mymodel <- list(trainmodel=tmp, 
                  accuracy=mean(data_test$離職 == predict(tmp, data_test)))
  return(mymodel)
}

# step3. 三大方法

# 方法1: 廣義線性模型(GLM, generalized linear model)
suppressWarnings(model_glm <- model("glm", train, test))

# 方法2: CART決策樹
suppressWarnings(model_cart <- model("rpart", train, test))

# 方法3: KNN
suppressWarnings(model_knn <- model("knn", train, test))

########################################
# part 2. UI介面
########################################

ui <- dashboardPage(
  
  dashboardHeader(title = "RWEPA | shiny小明算命師-v23.03.25, http://rwepa.blogspot.com/", 
                  titleWidth = 750),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text="資料特性", tabName = "datasummary", icon = icon("sunglasses", lib = "glyphicon")),
      menuItem(text="資料明細", tabName = "dataview", icon = icon("list", lib = "glyphicon")),
      menuItem(text="相關分析", tabName = "correlation", icon = icon("resize-small", lib = "glyphicon")),
      menuItem(text="統計檢定", tabName = "statistics", icon = icon("signal", lib = "glyphicon")),
      menuItem(text="建立模型", tabName = "modeling", icon = icon("king", lib = "glyphicon")),
      menuItem(text="預測分析", tabName = "forecasting", icon = icon("send", lib = "glyphicon"),
               # startExpanded = TRUE,
               menuSubItem("小明算命師", tabName = "hrPrediction"),
               menuSubItem("報表下載", tabName = "downReport"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "datasummary",
              
              fluidRow(
                box(
                  title = "資料摘要",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  verbatimTextOutput("Summary"),
                ),
              
                box(
                  title = "資料結構",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  verbatimTextOutput("Structure"),
                )
              )
      ),
      
      tabItem(tabName = "dataview",
              fluidRow(
                box(
                  title = "資料明細",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  DTOutput("Table")
                ),
              )
      ),
      
      tabItem(tabName = "correlation",
              
              fluidRow(
                
                box(
                  title = "離職-個數與百分比",
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "success", 
                  verbatimTextOutput("left_Summary"),
                ),
                
                box(
                  title = "離職-變數平均值",
                  width = 8,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "warning", 
                  verbatimTextOutput("left_Summary_all"),
                )
              ),
              
              fluidRow(
                box(
                  title = "相關係數矩陣圖", 
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  status = "primary", 
                  plotOutput("correlation_coefficient_plot")
                ),
                
                box(
                  title = "工作滿意度-群組盒鬚圖",
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  status = "primary",
                  plotOutput("boxplot")
                ),
                
              )
      ),
              
      tabItem(tabName = "statistics",
              
              fluidRow(
                box(width = 6, height = "600px",
                  title = "統計檢定",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "success", 
                  verbatimTextOutput("ttest")
                ),
                
                box(width = 6, height = "600px",
                  title = "特徵重要性繪圖",
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  status = "primary",
                  plotOutput("feature_plot")
                )
              )
      ),
      
      tabItem(tabName = "modeling",
              fluidRow(
                
                column(width = 5,
                  fluidRow(
                       box(width = 12,
                         title = "模型正確率",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         status = "success",
                         verbatimTextOutput("model_accuracy"),
                       )
                  ),
                  
                  fluidRow(
                       box(width = 12,
                           title = "ROC曲線",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           status = "primary",
                           plotOutput("roc_plot")
                       )
                  )
                ),

                column(width = 7,
                     
                     box(width = 12,
                         title = "GLM模型摘要",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         status = "warning",
                         verbatimTextOutput("glm_model_summary")
                     ),
                     box(width = 12,
                         title = "CART模型摘要",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         status = "warning",
                         verbatimTextOutput("cart_model_summary")
                     ),
                     box(width = 12,
                         title = "KNN模型摘要",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         status = "warning",
                         verbatimTextOutput("knn_model_summary")
                     )
                ),
              )
      ),
      
      tabItem(tabName="hrPrediction",
              
              fluidRow(
                
                column(width = 9,
                       
                       fluidRow(
                         
                         box(width = 12,
                             title = "輸入預測資料",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "success",
                             
                             fluidRow(
                               
                               column(width = 6,
                                      
                                      numericInput(inputId = "hr_evaluation",
                                                   label = "考核分數(0.01~1.00)",
                                                   value = 0.5,
                                                   min = 0.01,
                                                   max = 1.00,
                                                   step = 0.1),
                                      
                                      numericInput(inputId = "hr_project",
                                                   label = "每年專案個數(1~10)",
                                                   value = 4,
                                                   min = 1,
                                                   max = 10,
                                                   step = 1)
                                      ),
                               
                               column(width = 6,
                                      
                                      numericInput(inputId = "hr_hours",
                                                   label = "每月工作小時(80~350)",
                                                   value = 150,
                                                   min = 80,
                                                   max = 350),
                                      
                                      numericInput(inputId = "hr_company",
                                                   label = "在辨公室時間(1~10)",
                                                   value = 3,
                                                   min = 1,
                                                   max = 10)
                                      )
                               ),
                             
                             numericInput(inputId = "hr_satisfication",
                                          label = "工作滿意度(0.01~1.00)",
                                          value = 0.3,
                                          min = 0.01,
                                          max = 1,
                                          step = 0.1),
                             
                             radioButtons(inputId = "hr_accident",
                                          label = "工安意外",
                                          choices = c("有"=1, "沒有"=2),
                                          selected = 2,
                                          inline = TRUE),
                             
                             radioButtons(inputId = "hr_promotion",
                                          label = "近5年升遷",
                                          choices = c("有"=1, "沒有"=2),
                                          selected = 2,
                                          inline = TRUE),
                             
                             radioButtons(inputId = "hr_role",
                                          label = "部門",
                                          choices = myrole,
                                          selected = "sales",
                                          inline = TRUE),
                             
                             radioButtons(inputId = "hr_salary",
                                          label = "薪資",
                                          choices = c("high", "medium", "low"),
                                          selected = "low",
                                          inline = TRUE),
                             
                             tags$head(
                               tags$style(HTML('#run{background-color:SkyBlue} #run:hover{background-color:Gold}'))
                             ),
                             actionButton("run","執行預測"),
                             
                         )
                       ) # end for column 2: fluidRow
                ), # end for column 1

                column(width = 3,
                       
                  fluidRow(
                    
                    box(width = 12,
                        height = "400px",
                        title = "預測是否離職",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        status = "primary",
                        imageOutput("left_prediction")
                    )
                  )
                ) # end for column 2
              ) # end for fluidRow
      ), # end for tabItem: hrPrediction
      
      tabItem(tabName="downReport",
              
              fluidRow(
                
                box(title = "下載報表(HTML, DOCX)",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status = "success",
                    
                    textInput(inputId="hr_title",
                              label = "標題:",
                              value = paste0("人力資源報表分析-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))),
                    
                    textInput(inputId="hr_producer",
                              label = "製表人:",
                              value = paste0("李明昌(ALAN LEE)")),
                    
                    radioButtons(inputId = "hr_headdata", 
                                 label = "前6筆資料:", 
                                 choices = c("YES"=1, "NO"=2),
                                 selected = 1,
                                 inline = TRUE),
                    
                    radioButtons(inputId = "hr_str", 
                                 label = "資料結構:", 
                                 choices = c("YES"=1, "NO"=2),
                                 selected = 1,
                                 inline = TRUE),
                    
                    tags$hr(),
                    
                    downloadButton(outputId = "download_report_html", 
                                   label = "下載網頁/HTML檔案(需時間,請稍後...)"),
                    
                    tags$br(),
                    tags$br(),
                    
                    downloadButton(outputId = "download_report_docx", 
                                   label = "下載WORD/DOCXL檔案(需時間,請稍後...)")
                    
                ) # end for box 
              ) # end for fluidRow
      ) # end for tabItem: downReport
    ) # end for tabItems
  ) # end for dashboardBody
) # end for ui

########################################
# part 3. Server 介面
########################################

server <- function(input, output, session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$Summary <- renderPrint({
    summary(df)
  })
  
  output$Structure <- renderPrint({
    str(df)
  })
  
  output$Table <- renderDT({
    
    datatable(df,
              options = list(
                pageLength = 10,
                lengthMenu = c(5, 10, 20, 100),
                columnDefs = list(list(orderable = TRUE, targets = 0)),
                dom = 'Brtip'),
              rownames= FALSE)
    
  })
  
  output$left_Summary <- renderPrint({
    df %>%
      group_by(離職) %>% # 建立依left為群組
      summarise(個數 = n()) %>% # 建立群組總計Count
      mutate(百分比 = round(個數*100 / sum(個數), 0)) #建立群組百分比Freq
  })
  
  output$left_Summary_all <- renderPrint({
    df %>%
      select(-c(工安意外, 升遷, 部門, 薪資)) %>% # 刪除 工安意外,升遷,部門,薪資 4個 factor 欄位
      group_by(離職) %>% # 依left建立群組
      summarise(across(everything(), list(mean))) %>% # 計算各群組的平均值
      rename_with(~ tolower(gsub("_1", "_平均值", .x, fixed = TRUE))) # 更改欄位名稱
  })
  
  output$correlation_coefficient_plot <- renderPlot({
    
    # 相關係數矩陣(correlation matrix)
    corr <- cor(data.frame(df %>% select(-c(工安意外, 升遷, 部門, 薪資, 離職)), check.names = FALSE))
    
    # 相關係數矩陣圖(correlation matrix plot)
    ggcorrplot(corr, lab = TRUE, colors = c("sienna1", "white", "deepskyblue3")) +
      ggtitle("人力資源相關係數矩陣圖") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$boxplot <- renderPlot({
    
    # 員工工作滿意度-離職群組盒鬚圖
    ggplot(aes(y = 工作滿意度, x = 離職), data = df) + 
      geom_boxplot() +
      ggtitle("員工工作滿意度-離職群組盒鬚圖") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$ttest <- renderPrint({
    
    # 比較整體員工與離職員工的平均工作滿意度
    
    # 整體員工的平均工作滿意度
    emp_population_satisfaction <- mean(df$工作滿意度)
    
    # 離職員工的平均工作滿意度
    left_pop <- subset(df, 離職 == "Yes")
    emp_turnover_satisfaction <- mean(left_pop$工作滿意度)
    mytext1 <- paste(c('整體員工的工作滿意度: ', round(emp_population_satisfaction, 4)*100, '%'), collapse ="")
    mytext2 <- paste(c('離職員工的工作滿意度: ', round(emp_turnover_satisfaction, 4)*100, '%'), collapse ="")
    cat(paste0("===工作滿意度===", "\n", mytext1, "\n", mytext2, "\n\n"))
    
    # T檢定-比較整體員工與離職員工的平均工作滿意度
    # H0: 離職員工的平均工作滿意度等於整體員工的平均工作滿意度
    # H1: 離職員工的平均工作滿意度不等於整體員工的平均工作滿意度
    
    cat("===T檢定===\n")
    cat("H0: 離職員工的平均工作滿意度等於整體員工的平均工作滿意度\n")
    cat("H1: 離職員工的平均工作滿意度不等於整體員工的平均工作滿意度\n")
    
    ttest_satisfaction <- t.test(x=left_pop$工作滿意度, mu=emp_population_satisfaction)
    print(ttest_satisfaction)
    cat("\n")
    
    cat("===結論===\n")
    if (ttest_satisfaction$p.value <= 0.05) {
      cat("拒絕 H0, 即離職員工的平均工作滿意度不等於整體員工的平均工作滿意度")
    } else {
      cat("不拒絕 H0, 即離職員工的平均工作滿意度等於整體員工的平均工作滿意度")
    }
  })
  
  output$feature_plot <- renderPlot({
    
    # 特徵重要性繪圖(feature importance plot)
    varImpPlot(df_rf, pch = 16, main="特徵重要性繪圖")
    
  })
  
  output$model_accuracy <- renderPrint({
    
    cat(paste0("GLM正確率  = ", round(model_glm$accuracy*100, 3), "%", "\n",
               "CART正確率 = ", round(model_cart$accuracy*100, 3), "%", "\n",
               "KNN正確率  = ", round(model_knn$accuracy*100, 3), "%"))
    
  })
  
  output$glm_model_summary <- renderPrint({
    
    print(model_glm$trainmodel)
    
  })
  
  output$cart_model_summary <- renderPrint({
    
    print(model_cart$trainmodel)

  })
  
  output$knn_model_summary <- renderPrint({
    
    print(model_knn$trainmodel)
    
  })
  
  output$roc_plot <- renderPlot({
    
    # Plot roc curve - glm
    suppressWarnings(plot.roc(model_glm$trainmodel$pred$obs,
             model_glm$trainmodel$pred$Yes,
             print.auc=TRUE,
             print.auc.x=0.2, 
             print.auc.y=0.7,
             grid=TRUE,
             col=2,
             xlab="", 
             ylab=""))
    legend("bottomright", legend=c("GLM", "CART", "KNN"),
           col=c(2:4), lwd=2)
    
    # Plot roc curve - cart
    suppressWarnings(plot.roc(model_cart$trainmodel$pred$obs,
             model_cart$trainmodel$pred$Yes,
             print.auc=TRUE,
             add=TRUE,
             col=3,
             print.auc.x=0.3,
             print.auc.y=0.6,
             xlab="", 
             ylab=""))
    
    # Plot roc curve - knn
    suppressWarnings(plot.roc(model_knn$trainmodel$pred$obs,
             model_knn$trainmodel$pred$Yes,
             print.auc=TRUE,
             add=TRUE,
             col=4,
             print.auc.x=0.4, 
             print.auc.y=0.5,
             xlab="", 
             ylab=""))
    title(xlab="Specificity (True Negative Rate, TNR)",
          ylab="Sensitivity (True Positive Rate, TPR)")
    
  })
  
  output$left_prediction <- renderImage({
    list(src = "images/left_animation.gif",
         contentType = "image/gif")
  }, deleteFile = FALSE)
  
  # HTML下載
  output$download_report_html <- downloadHandler({
    
    testing <- data.frame(考核分數     = input$hr_evaluation,
                          每年專案個數 = input$hr_project,
                          每月工作小時 = input$hr_hours,
                          辨公時間     = input$hr_company,
                          工作滿意度   = input$hr_satisfication,
                          工安意外     = ifelse(input$hr_accident == 1, "Yes", "No"),
                          升遷         = ifelse(input$hr_promotion == 1, "Yes", "No"),
                          部門         = input$hr_role,
                          薪資         = input$hr_salary,
                          check.names  = FALSE)
    
    left_yes_prob <- predict(model_knn$trainmodel, newdata = testing, type = "prob")
    
    left_yes_prediction <- ifelse(left_yes_prob$Yes >= 0.5, "Yes", "No")
    
    hr_prediction <- paste0("計算完成!\n",
                        "離職(Yes)機率 = ", round(left_yes_prob$Yes, 3), "\n",
                        "離職(No) 機率 = ", round(left_yes_prob$No, 3), "\n",
                        "==========================\n",
                        "結論: 離職: ", left_yes_prediction)
    
    filename <- paste0(input$hr_title, ".html")},
    
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      tempReport <- file.path(tempdir(), "hr_teller_report_html.Rmd")
      file.copy("data/hr_teller_report_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(para_title         = input$hr_title, 
                     para_producer      = input$hr_producer, 
                     para_headdata      = input$hr_headdata, 
                     para_str           = input$hr_str,
                     para_evaluation    = input$hr_evaluation,
                     para_project       = input$hr_project,
                     para_hours         = input$hr_hours,
                     para_company       = input$hr_company,
                     para_satisfication = input$hr_satisfication,
                     para_accident      = input$hr_accident,
                     para_promotion     = input$hr_promotion,
                     para_role          = input$hr_role,
                     para_salary        = input$hr_salary,
                     para_prediction    = hr_prediction)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) # end for downloadHandler html
  
  # WORD下載
  output$download_report_docx <- downloadHandler({
    
    testing <- data.frame(考核分數     = input$hr_evaluation,
                          每年專案個數 = input$hr_project,
                          每月工作小時 = input$hr_hours,
                          辨公時間     = input$hr_company,
                          工作滿意度   = input$hr_satisfication,
                          工安意外     = ifelse(input$hr_accident == 1, "Yes", "No"),
                          升遷         = ifelse(input$hr_promotion == 1, "Yes", "No"),
                          部門         = input$hr_role,
                          薪資         = input$hr_salary,
                          check.names  = FALSE)
    
    left_yes_prob <- predict(model_knn$trainmodel, newdata = testing, type = "prob")
    
    left_yes_prediction <- ifelse(left_yes_prob$Yes >= 0.5, "Yes", "No")
    
    hr_prediction <- paste0("計算完成!\n",
                            "離職(Yes)機率 = ", round(left_yes_prob$Yes, 3), "\n",
                            "離職(No) 機率 = ", round(left_yes_prob$No, 3), "\n",
                            "==========================\n",
                            "結論: 離職: ", left_yes_prediction)
    
    filename <- paste0(input$hr_title, ".docx")},
    
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      tempReport <- file.path(tempdir(), "hr_teller_report_docx.Rmd")
      file.copy("data/hr_teller_report_docx.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(para_title         = input$hr_title, 
                     para_producer      = input$hr_producer, 
                     para_headdata      = input$hr_headdata, 
                     para_str           = input$hr_str,
                     para_evaluation    = input$hr_evaluation,
                     para_project       = input$hr_project,
                     para_hours         = input$hr_hours,
                     para_company       = input$hr_company,
                     para_satisfication = input$hr_satisfication,
                     para_accident      = input$hr_accident,
                     para_promotion     = input$hr_promotion,
                     para_role          = input$hr_role,
                     para_salary        = input$hr_salary,
                     para_prediction    = hr_prediction)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  ) # end for downloadHandler word
  
  testingPrediction <- eventReactive(input$run, {
    
    testing <- data.frame(考核分數     = input$hr_evaluation,
                          每年專案個數 = input$hr_project,
                          每月工作小時 = input$hr_hours,
                          辨公時間     = input$hr_company,
                          工作滿意度   = input$hr_satisfication,
                          工安意外     = ifelse(input$hr_accident == 1, "Yes", "No"),
                          升遷         = ifelse(input$hr_promotion == 1, "Yes", "No"),
                          部門         = input$hr_role,
                          薪資         = input$hr_salary,
                          check.names  = FALSE)
    
    left_yes_prob <- predict(model_knn$trainmodel, newdata = testing, type = "prob")
    
    left_yes_prediction <- ifelse(left_yes_prob$Yes >= 0.5, "Yes", "No")
    
    return(paste0("計算完成!\n",
                  "離職(Yes)機率 = ", round(left_yes_prob$Yes, 3), "\n",
                  "離職(No) 機率 = ", round(left_yes_prob$No, 3), "\n",
                  "==========================\n",
                  "結論: 離職: ", left_yes_prediction))
    
  })
  
  observeEvent(input$run, {
    showModal(modalDialog(
      
      title = "人力資源預測模型",
      
      verbatimTextOutput("hr_text"),
      
      footer = tagList(actionButton("close_pop", "關閉"))
    ))
  })
  
  output$hr_text <- renderText({
    testingPrediction()
  })
  
  observeEvent(input$close_pop, {
    removeModal()
  })
  
} # end for server

########################################
# part 4. 執行 shiny
########################################
shinyApp(ui = ui, server = server)
# end for shiny
