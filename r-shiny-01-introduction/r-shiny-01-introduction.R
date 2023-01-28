# File     : r-shiny-01-introduction.R
# Title    : RWEPA | shiny企業實務應用 第1集-白話shiny
# Author   : Ming-Chang Lee
# Date     : 2023.01.24
# YouTube  : https://www.youtube.com/@alan9956
# RWEPA    : http://rwepa.blogspot.tw/
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com
# Encoding : UTF-8

# 安裝 shiny 套件
install.packages("shiny")

# 顯示內建11個範例
dir(paste0(.libPaths(), "/shiny/examples"))

# 載入套件
library(shiny)

# 執行範例 "01_hello"
runExample("01_hello")
# end
