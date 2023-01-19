# File     : r_multiple_excels.R
# Title    : RWEPA | R - multiple excels
# Date     : 2023.01.05
# Author   : Ming-Chang Lee
# RWEPA    : http://rwepa.blogspot.tw/
# YouTube  : https://www.youtube.com/@alan9956
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com
# Encoding : UTF-8

# 主題:
# 1.建立資料集
# 2.匯入多個Excel 檔案
# 3.結論

# 1.建立資料集

# 載入套件
library(writexl) # 模擬匯出並建立 Excel 檔案
library(readxl)  # 匯入 Excel 檔案

# 設定工作目錄並依照實際需求修改
setwd("C:/rdata")
getwd()

# 模擬10個Excel檔案, 每個檔案有12個變數(1-12月)
# 每個變數樣本數為5, 每個檔案只有1個工作表
nfile <- 10
ncolumns <- 12
nrows <- 5

# 設定亂數種子
set.seed(168)

# files: df_1.xlsx, df_2.xlsx, df_3.xlsx,...,df_10.xlsx
for (i in 1:nfile) {
  df <- data.frame(matrix(sample(100, ncolumns*nrows, replace = TRUE), ncol = ncolumns))
  names(df) <- month.name
  write_xlsx(x=df, path=paste0("df_", i, ".xlsx"))
}

# 2.匯入多個Excel 檔案,多個工作表

# 讀取所有檔案名稱, 使用正規表示式(Regular Expressions) pattern 參數
?regex

myfile <- list.files(pattern = "df_")
myfile

# 匯入所有工作表並儲存為list資料物件
dflist <- lapply(myfile, read_excel)
names(dflist) <- myfile
class(dflist) # list
dflist

# 將所有 list 各元素合併為1個 data.frame
df <- as.data.frame(do.call(rbind, dflist), check.names = FALSE)
str(df) # 50*12

# 使用 map_dfr {purrr} 亦可執行, 謝謝 Wber 分享 (2023.01.19).
library(purrr)
df <- map_dfr(myfile, read_xlsx)

# 3.結論
# writexl 套件 - 模擬匯出並建立 Excel 檔案
# readxl 套件 - 匯入 Excel 檔案
# lapply
# as.data.frame(do.call(rbind, 資料物件))
# end
