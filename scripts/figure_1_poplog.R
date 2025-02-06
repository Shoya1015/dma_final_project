# -----------------------------------------------------------
# 1. 必要なパッケージの読み込み
# -----------------------------------------------------------
pacman::p_load(tidyverse, haven)

# -----------------------------------------------------------
# 2. データの読み込みと基本整形
# -----------------------------------------------------------
# Stata形式のデータファイルを読み込み、データフレーム "data" に格納
data <- read_dta("data/raw/DDCGdata_final.dta")

# 変数 "_ID" を "id" に名称変更し、各id（国や対象）のデータを年順に並べ替え
data_fx <- data %>%
  rename(id = "_ID") %>%
  group_by(id) %>%
  arrange(year) %>%
  ungroup()

# -----------------------------------------------------------
# 3. 民主化転換（処置）の識別
# -----------------------------------------------------------
# 直前の期間の "dem" の値を取得し、以下のルールで "transition" 変数を作成
#  - もし直前が非民主主義（0）で、現在が民主主義（1）なら「民主化転換」として 1
#  - もし直前も現在も非民主主義なら対照群として 0
#  - それ以外（すでに民主主義など）は NA とする
data_fx <- data_fx %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(prev_dem = dplyr::lag(dem, 1)) %>%  # 1期前の "dem" を取得
  ungroup() %>%
  mutate(transition = case_when(
    dem == 1 & prev_dem == 0 ~ 1,
    dem == 0 & prev_dem == 0 ~ 0,
    TRUE ~ NA_real_
  ))

# -----------------------------------------------------------
# 4. 対数変換とラグ値の作成
# -----------------------------------------------------------
# PopulationtotalSPPOPTOTL を対数変換して pop_log とする
data_fx <- data_fx %>%
  mutate(pop_log = log(PopulationtotalSPPOPTOTL))

# pop_log の1期前から4期前までの値を作成し、いずれかが欠損している行は除外
data_fx <- data_fx %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(
    lag1 = dplyr::lag(pop_log, 1),
    lag2 = dplyr::lag(pop_log, 2),
    lag3 = dplyr::lag(pop_log, 3),
    lag4 = dplyr::lag(pop_log, 4)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1) & !is.na(lag2) & !is.na(lag3) & !is.na(lag4))

# -----------------------------------------------------------
# 5. 相対的な pop_log 変化（基準：lag1）を計算する
# -----------------------------------------------------------
# ここでは、pop_log の直前の値（lag1）を基準に、各期間での対数値の変化（差分）を求めます。
# ※ 「相対時点」は、民主化転換前後の年を示し、以下のように命名しています：
#     - t < 0：民主化転換前。t = -15～-2 の場合、dplyr::lag(pop_log, abs(t)) - lag1 を計算
#     - t = -1：民主化直前の期間。ここは基準なので 0 とする
#     - t = 0：民主化転換直後。pop_log - lag1 を計算
#     - t > 0：民主化転換後。dplyr::lead(pop_log, t) - lag1 を計算
#
# 命名規則：
#   - 前期は "popDiff_mX" （例：t = -15 → "popDiff_m15"）
#   - 基準は t = -1 → "popDiff_m1"（値は 0）
#   - 転換直後は t = 0 → "popDiff_0"
#   - 後期は "popDiff_pX" （例：t = 1 → "popDiff_p1"）

# (a) 民主化転換前の期間：t = -15 ～ -2
for (t in -15:-2) {
  col_name <- paste0("popDiff_m", abs(t))  # 例: t = -15 → "popDiff_m15"
  data_fx <- data_fx %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lag(pop_log, abs(t)) - lag1) %>%
    ungroup()
}

# (b) 基準期間：t = -1 は基準なので 0 とする
data_fx <- data_fx %>%
  mutate(popDiff_m1 = 0)

# (c) 民主化転換直後およびその後の期間：t = 0 ～ 30
# まず、t = 0 の場合は現在の pop_log と lag1 の差を計算
data_fx <- data_fx %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(popDiff_0 = pop_log - lag1) %>%
  ungroup()

# t > 0 の場合は dplyr::lead を使って未来の pop_log を取得
for (t in 1:30) {
  col_name <- paste0("popDiff_p", t)  # 例: t = 1 → "popDiff_p1"
  data_fx <- data_fx %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lead(pop_log, t) - lag1) %>%
    ungroup()
}

# ※ 分析対象は transition (民主化転換) の値が判定できる観測に限定
data_fx <- data_fx %>% filter(!is.na(transition))

# -----------------------------------------------------------
# 6. 回帰調整推定量（ATT）を求める関数の定義
# -----------------------------------------------------------
# 以下の関数は、指定した変数（例："popDiff_m15" など）について、
# 対照群（transition == 0）のみで年ダミーを用いた回帰モデルを推定し、
# 処置群（transition == 1）に対して反実仮想のアウトカムを予測、実際の値との差を計算して平均した値（ATT）を返します。
estimateATT <- function(dataset, outcome_col) {
  # アウトカム変数と transition が欠損していないサンプルを抽出
  sub_data <- dataset %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(transition))
  
  if(nrow(sub_data) == 0) return(NA)
  
  # 年（year）を因子として扱う（水準はサブセット全体で統一）
  year_levels <- sort(unique(sub_data$year))
  sub_data <- sub_data %>%
    mutate(year_factor = factor(year, levels = year_levels))
  
  # 対照群（transition が 0）のデータを抽出
  control_data <- sub_data %>% filter(transition == 0)
  # 処置群（transition が 1）のデータを抽出
  treated_data <- sub_data %>% filter(transition == 1)
  
  # 対照群のデータが十分でなければ NA を返す
  if(nrow(control_data) < 2 || length(unique(control_data$year)) < 2) return(NA)
  
  # 年ダミー（year_factor）を説明変数として定数項なし回帰モデルを推定
  model_formula <- as.formula(paste(outcome_col, "~ year_factor - 1"))
  control_model <- tryCatch(lm(model_formula, data = control_data),
                            error = function(e) NULL)
  if(is.null(control_model)) return(NA)
  
  # 対照群で推定したモデルを用いて、処置群の反実仮想アウトカムを予測
  predicted_outcomes <- tryCatch(predict(control_model, newdata = treated_data),
                                 error = function(e) rep(NA, nrow(treated_data)))
  
  # 各処置群の観測で、実際のアウトカムと予測値との差（効果）を計算し、その平均を ATT とする
  treatment_effects <- treated_data[[outcome_col]] - predicted_outcomes
  mean(treatment_effects, na.rm = TRUE)
}

# -----------------------------------------------------------
# 7. 各「相対時点」における ATT の算出
# -----------------------------------------------------------
# 相対時点：民主化転換前は -15 ～ -1、転換後は 0 ～ 30
relative_times <- c(seq(-15, -1), seq(0, 30))
atts <- numeric(length(relative_times))

# 各相対時点ごとに、対応する pop_log 差分の変数名を決定し、ATT を計算する
for (i in seq_along(relative_times)) {
  t_val <- relative_times[i]
  if(t_val < 0) {
    # t が負の場合：例 t = -15 → "popDiff_m15"
    col_name <- paste0("popDiff_m", abs(t_val))
  } else {
    # t が 0 の場合は "popDiff_0"、正の場合は "popDiff_p{t}"
    col_name <- if(t_val == 0) "popDiff_0" else paste0("popDiff_p", t_val)
  }
  atts[i] <- estimateATT(data_fx, col_name)
}

# 結果をデータフレームにまとめる
results_df <- data.frame(RelativeTime = relative_times, ATT = atts)

# -----------------------------------------------------------
# 8. 結果のプロット
# -----------------------------------------------------------
# pop_log の変化量（対数差分）としてプロットする
figure_population <- ggplot(results_df, aes(x = RelativeTime, y = ATT)) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  labs(x = "Years around Democratization", 
       y = "Change in log(PopulationtotalSPPOPTOTL)") +
  theme_bw()

# -----------------------------------------------------------
# 9. プロットの保存
# -----------------------------------------------------------
ggsave("output/figure_population.pdf", figure_population, width = 14, height = 8, units = "cm")
