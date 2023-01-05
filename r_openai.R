# File     : r_openai.R
# Title    : RWEPA | R - openai 藝術創作 (openai artwork with R)
# Date     : 2023.12.31
# Author   : Ming-Chang Lee
# RWEPA    : http://rwepa.blogspot.tw/
# YouTube  : https://www.youtube.com/@alan9956
# GitHub   : https://github.com/rwepa
# Email    : alan9956@gmail.com
# Encoding : UTF-8

# 主題:
# 1.建立 OpenAI 帳號
# 2.安裝 openai 套件
# 3.建立完成物件
# 4.結論

# 1.建立 OpenAI 帳號
# https://openai.com/api/

# 2.安裝 openai 套件
# https://cran.r-project.org/web/packages/openai/index.html
install.packages("openai")

# 輸入步驟1所建立的 API key
Sys.setenv(
  OPENAI_API_KEY = '輸入步驟1所建立的 API key'
)

# 載入 openai 套件
library(openai)

# 3.建立完成物件
create_completion(
  model = "ada",
  prompt = "Generate a question and an answer"
)

# 範例1 太空人騎馬照
create_image("An astronaut riding a horse in a photorealistic style")

# 將 https:// 開始的所有字元複製到瀏覽器, 結果為太空人騎馬照, 每次執行結果不一定相同.

# 範例2 山谷飛兔照
create_image("flying rabbit in the valley")
# 將 https:// 開始的所有字元複製到瀏覽器, 結果為山谷飛兔照.

# 4.結論
# openai套件
# 建立 openai 帳號與Key
# 使用瀏覽器呈現藝術創作成果
# end
