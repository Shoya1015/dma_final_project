# パッケージのインストール ----
pacman::p_load(
  plm,           # パネル回帰
  lmtest,        # 検定用
  sandwich,      # ロバスト標準誤差
  AER,           # IV推定用（ivreg など）
  modelsummary,  # 結果表作成
  tidyverse,     # データ操作（dplyr, stringr 等）
  purrr,         # 関数型プログラミング
  haven,         # Stataデータの読み込み
  car,           # deltaMethod等
  DT
)

# データの読み取りと確認 ----
data <- read_dta("data/raw/DDCGdata_final.dta")
glimpse(data)

# カラム名を確認 ----
# 方法1: 
# インタラクティブなテーブルとして表示
datatable(data.frame(カラム名 = colnames(data)))

# 方法2: 
# カラム名のデータフレームを作成してCSVに保存
write.csv(data.frame(カラム名 = colnames(data)),
          file = "docs/DDCGdata_column.csv",
          row.names = FALSE)

# 保存したCSVの中身を確認
df_column <- read_csv("docs/DDCGdata_column.csv")

# ラベルからカラムを検索 ----
# 各変数に付与されているラベルを取得
# 'attr(x, "label")' で、その変数のラベルを取得できる
var_labels <- sapply(data, function(x) attr(x, "label"))

# ラベルの一覧をデータフレーム化（tibble化）しておくと便利
labels_df <- tibble::tibble(
  variable = names(var_labels),
  label    = var_labels
)

# labels_dfをView()関数で眺めてもOK (RStudioでテーブル表示)
View(labels_df)

