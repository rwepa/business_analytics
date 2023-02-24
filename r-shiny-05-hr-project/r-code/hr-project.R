# File     : hr-project.R
# Title    : RWEPA | Shiny - Human Resource Analytics
# Author   : Ming-Chang Lee
# Date     : 2023.2.24
# YouTube  : https://www.youtube.com/@alan9956
# RWEPA    : http://rwepa.blogspot.tw/
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com

library(readr)        # read_csv
library(dplyr)        # mutate_if, mutate, group_by, summarise, select, rename_with
library(ggcorrplot)   # ggcorrplot
library(ggplot2)      # ggplot
library(randomForest) # randomForest, importance, varImpPlot
library(caret)        # trainControl, train
library(pROC)         # plot.roc

# 載入人力資源資料集
urls <- "https://raw.githubusercontent.com/rwepa/DataDemo/master/human_resource.csv"
df <- read_csv(urls)
df # A tibble: 14,999 × 10

# 資料結構
str(df) # left: num數值, role, salary: chr字元

# 資料摘要
summary(df) # 資料沒有NA

# 資料轉換
df <- df %>%
  mutate_if(is.character, as.factor) %>% # 轉換 character 為 factor
  mutate(left=factor(left, labels=c("No","Yes")))

# 1.資料檢視
df

# 2.資料摘要與結構
# 資料結構
# factor: 3
# numeric: 7
str(df)

# 資料摘要
summary(df)

# left群組百分比
df %>%
  group_by(left) %>% # 建立依left為群組
  summarise(Count = n()) %>% # 建立群組總計Count
  mutate(Freq = round(Count / sum(Count), 2)) #建立群組百分比Freq
# A tibble: 2 × 3
#   left  Count  Freq
#   <fct> <int> <dbl>
# 1 0     11428  0.76
# 2 1      3571  0.24

# left群組的變數平均值
df %>%
  select(-c(role, salary)) %>% # 刪除 role, salary 二個欄位
  group_by(left) %>% # 依left建立群組
  summarise(across(everything(), list(mean))) %>% # 計算各群組的平均值
  rename_with(~ tolower(gsub("_1", "_mean", .x, fixed = TRUE))) # 更改欄位名稱

# 3.繪圖

# 相關係數矩陣(correlation matrix)
corr <- cor(as.data.frame(df %>% select(-c(role, salary, left))))

# 相關係數矩陣圖(correlation matrix plot)
ggcorrplot(corr, lab = TRUE, colors = c("sienna1", "white", "deepskyblue3")) +
  ggtitle("人力資源相關係數矩陣圖") +
  theme(plot.title = element_text(hjust = 0.5))

# 員工工作滿意度-left群組盒鬚圖
ggplot(aes(y = satisfaction_level, x = left), data = df) + 
  geom_boxplot() +
  ggtitle("員工工作滿意度-left群組盒鬚圖") +
  theme(plot.title = element_text(hjust = 0.5))

# 4.檢定

# 比較整體員工與離職員工的平均工作滿意度

# 整體員工的平均工作滿意度
emp_population_satisfaction <- mean(df$satisfaction_level)

# 離職員工的平均工作滿意度
left_pop <- subset(df, left == "Yes")
emp_turnover_satisfaction <- mean(left_pop$satisfaction_level)

print(paste(c('整體員工的工作滿意度: ', round(emp_population_satisfaction, 4)*100, '%'), collapse =""))
print(paste(c('離職員工的工作滿意度: ', round(emp_turnover_satisfaction, 4)*100, '%'), collapse =""))

# T檢定-比較整體員工與離職員工的平均工作滿意度
# H0: 離職員工的平均工作滿意度等於整體員工的平均工作滿意度
# H1: 離職員工的平均工作滿意度不等於整體員工的平均工作滿意度
ttest_satisfaction <- t.test(x=left_pop$satisfaction_level, mu=emp_population_satisfaction)
ttest_satisfaction

if (ttest_satisfaction$p.value <= 0.05) {
  print("Rejet H0")
} else {
  print("Do not reject H0")
}

# 5.建立預測模型

# 使用隨機森林(random forest)
df_rf <- randomForest(left~., data=df)

# 特徵重要性(feature importance)
importance(df_rf)

# 特徵重要性繪圖(feature importance plot)
varImpPlot(df_rf, pch = 16, main="人力資源特徵重要性繪圖")

# 建立訓練集(2/3),測試集(1/3)
set.seed(168)
trainIndex <- createDataPartition(df$left, p = 0.7, list = FALSE)
train <- df[trainIndex,]  # 使用負號
test <- df[-trainIndex,]

# 建立監督式學習模型
set.seed(168)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE,
  savePredictions = TRUE)

model <- function(usedmethod, data_train, data_test) {
  
  myformula <- as.formula("left ~ .")
  
  # 使用 train 進行模型訓練
  tmp <- train(myformula, 
               data = data_train, 
               trControl = fitControl, 
               method = usedmethod)
  
  # 技巧: 使用list回傳模型結果與正確率
  mymodel <- list(trainmodel=tmp, 
                  accuracy=mean(data_test$left == predict(tmp, data_test)))
  return(mymodel)
}

# 方法1: 廣義線性模型(GLM, generalized linear model)
model_glm <- model("glm", train, test)

# 顯示 summary, accuracy
summary(model_glm$trainmodel)
model_glm$accuracy  # 0.7879529

# Plot roc curve
plot.roc(model_glm$trainmodel$pred$obs,
         model_glm$trainmodel$pred$Yes,
         print.auc=TRUE,
         print.auc.x=0.2, 
         print.auc.y=0.7,
         grid=TRUE,
         col=2)
legend("bottomright", legend=c("glm", "cart", "knn"),
       col=c(2:4), lwd=2)

# 方法2: CART決策樹
model_tree <- model("rpart", train, test)

# 顯示 accuracy
# summary(model_tree$trainmodel)
model_tree$accuracy # 0.9113136

# Plot roc curve
plot.roc(model_tree$trainmodel$pred$obs,
         model_tree$trainmodel$pred$Yes,
         print.auc=TRUE,
         add=TRUE,
         col=3,
         print.auc.x=0.3, 
         print.auc.y=0.6)

# 方法3: KNN
model_knn <- model("knn", train, test)

# 顯示 accuracy
model_knn$accuracy # 0.9404312

# Plot roc curve
plot.roc(model_knn$trainmodel$pred$obs,
         model_knn$trainmodel$pred$Yes,
         print.auc=TRUE,
         add=TRUE,
         col=4,
         print.auc.x=0.4, 
         print.auc.y=0.5)

# reference:
# https://www.kaggle.com/code/ragulram/hr-analytics-exploration-and-modelling-with-r
# Data mining with Rattle – ROC curve (SVM and NN): http://rwepa.blogspot.com/2013/08/data-mining-with-rattle-roc-curve-svm.html
# ROCR package - ROC curve: http://rwepa.blogspot.com/2013/01/rocr-roc-curve.html
# end