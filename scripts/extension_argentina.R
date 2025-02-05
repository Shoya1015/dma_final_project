# -------------------------------
# 0. 必要パッケージの読み込み
# -------------------------------
pacman::p_load(
  tidyverse,
  haven,
  Synth
)

# -------------------------------
# 1. データの読み込み
# -------------------------------
data <- read_dta("data/raw/DDCGdata_final.dta") 

# -------------------------------
# 2. 対象国の定義
#    ※治療対象（Argentina）に加え、アルゼンチンと類似すると考えた以下の10か国を選定
# -------------------------------
countries <- c("Argentina", 
               "Brazil", "Chile", "Uruguay", "Paraguay", 
               "Peru", "Colombia", "Ecuador", "Venezuela", 
               "Bolivia", "Mexico")

# -------------------------------
# 3. 対象国のデータのみ抽出
# -------------------------------
data_arsynth <- data %>%
  filter(country_name %in% countries)

# -------------------------------
# 4. アウトカム変数 y の欠測値がある行を削除
# -------------------------------
data_arsynth <- data_arsynth %>%
  filter(!is.na(y))

# -------------------------------
# 5. 欠測値のない変数のみ抽出するための列名を取得
# -------------------------------
cols_no_na <- names(data_arsynth)[colSums(is.na(data_arsynth)) == 0]

# -------------------------------
# 6. 欠測値のない列のみ抽出して新たなデータセットを作成
# -------------------------------
data_arsynth <- data_arsynth %>%
  select(all_of(cols_no_na))

# -------------------------------
# 7. country_name を因子に変換し、数値型の識別子 unit_num を作成
# -------------------------------
data_arsynth$country_name <- factor(data_arsynth$country_name, levels = countries)
data_arsynth$unit_num <- as.numeric(data_arsynth$country_name)

# -------------------------------
# 8. 共変量の変数名をまとめる
#    （※例として、人口や貿易関連指標、政情不安等を使用）
# -------------------------------
covariates <- c("PopulationtotalSPPOPTOTL",
                "tradewbreg",
                "unrestreg",
                "demreg")

# -------------------------------
# 9. アウトカム（y）と共変量を数値型に変換し、欠測値のある行を削除
# -------------------------------
cols_to_convert <- c("gdppercapitaconstant2000us", covariates)
data_arsynth[cols_to_convert] <- lapply(data_arsynth[cols_to_convert], 
                                        function(x) as.numeric(as.character(x)))
data_arsynth <- data_arsynth %>% 
  filter(complete.cases(.[, cols_to_convert]))

# -------------------------------
# 10. Synth パッケージは base R の data.frame を要求するため、
#     tibble を data.frame に変換
# -------------------------------
data_arsynth <- as.data.frame(data_arsynth)

# -------------------------------
# 【新規追加】バランスドパネル作成：
# プロット期間（1960～2010年）に全ての年が観測されている単位のみ抽出
# -------------------------------
years_all <- 1960:2010               # プロット期間の年の範囲
n_years_total <- length(years_all)   # 必要な年数

# 各単位について、観測されている年数を確認
complete_units <- data_arsynth %>%
  group_by(unit_num) %>%
  summarize(n_years = n_distinct(year)) %>%
  filter(n_years == n_years_total) %>%  # 全ての年が存在する単位のみ抽出
  pull(unit_num)

# バランスドパネルデータを作成
data_balanced <- data_arsynth %>%
  filter(unit_num %in% complete_units)

# -------------------------------
# 【修正ポイント】
# バランスドパネル作成後に治療群とコントロール群の識別子を再抽出
# -------------------------------
treatment_id <- unique(data_balanced$unit_num[data_balanced$country_name == "Argentina"])
controls_id  <- unique(data_balanced$unit_num[data_balanced$country_name != "Argentina"])

# -------------------------------
# 11. dataprep 関数による合成コントロール法用のデータ整形
#      ・事前期間：1960～1982年
#      ・プロット期間：1960～2010年
#      ・special.predictors ではアウトカム y の細かい区間平均を利用
# -------------------------------
dataprep_out <- dataprep(
  foo = data_balanced,                 # バランスドパネルデータを指定
  predictors = covariates,             # 共変量
  predictors.op = "mean",              # 指定期間中の平均値を利用
  time.predictors.prior = 1960:1982,     # 事前期間を指定
  
  # ▼▼▼ ここを修正：y の事前期間平均を複数区間で指定 ▼▼▼
  special.predictors = list(
    list("y", 1960:1964, "mean"),
    list("y", 1965:1969, "mean"),
    list("y", 1970:1974, "mean"),
    list("y", 1975:1979, "mean"),
    list("y", 1980:1982, "mean")
  ),
  # ▲▲▲ ここを修正：y の事前期間平均を複数区間で指定 ▲▲▲
  
  dependent = "gdppercapitaconstant2000us",     # アウトカム変数
  unit.variable = "unit_num",                   # 数値型の単位識別子を指定
  time.variable = "year",                       # 時間変数の指定
  treatment.identifier = treatment_id,          # 治療対象のID（Argentina）
  controls.identifier = controls_id,            # コントロール群のID
  time.optimize.ssr = 1960:1982,                # SSR最適化の期間を指定
  time.plot = 1960:2010                         # プロット期間の指定
) 

# -------------------------------
# 12. synth 関数による合成コントロールの推定
# -------------------------------
synth_out <- synth(dataprep_out)

# -------------------------------
# 13. 結果の表示・プロット
# -------------------------------

# (a) 結果のテーブル表示（各変数の重みなど）
synth_tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
print(synth_tables)

# (b) 治療対象（Argentina）と合成コントロールの推定値のプロット
path.plot(synth.res = synth_out, dataprep.res = dataprep_out,
          Ylab = "アウトカム (経済成長 y)", Xlab = "年",
          Legend = c("Argentina", "Synthetic Argentina"),
          Main = "合成コントロール法: Argentina vs. Synthetic Control")

# 介入開始時期 (1983年) を示す垂直線を追加
abline(v = 1983, col = "red", lty = 2, lwd = 2)
# 介入開始時刻を示すテキストを追加（プロットの上端付近に表示）
usr <- par("usr")  # プロット領域の座標を取得
text(x = 1983, y = usr[4], labels = "Treatment starts", pos = 4, col = "red")

# (c) アウトカムのギャップ（実測値 - 合成コントロール）のプロット
gaps.plot(synth.res = synth_out, dataprep.res = dataprep_out,
          Ylab = "アウトカムのギャップ (経済成長 y)", Xlab = "年",
          Main = "ギャップ: Argentina - Synthetic Argentina")

# ギャッププロットにも介入開始時期を示す垂直線を追加
abline(v = 1983, col = "red", lty = 2, lwd = 2)

