# パッケージのインストール ----
pacman::p_load(
  tidyverse,
  haven,
  DT
)

# データの読み取りと確認 ----
df <- read_dta("data/raw/DDCGdata_final.dta")
glimpse(df)

# カラム名を確認 ----

# 方法1: 
# インタラクティブなテーブルとして表示
datatable(data.frame(カラム名 = colnames(df)))

# 方法2: 
# カラム名のデータフレームを作成してCSVに保存
write.csv(data.frame(カラム名 = colnames(df)),
          file = "docs/DDCGdata_column.csv",
          row.names = FALSE)

# 保存したCSVの中身を確認
df_column <- read_csv("docs/DDCGdata_column.csv")


