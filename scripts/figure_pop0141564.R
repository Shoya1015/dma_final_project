# -----------------------------------------------------------
# 0. 必要なパッケージの読み込み
# -----------------------------------------------------------
pacman::p_load(tidyverse, haven, patchwork)

# -----------------------------------------------------------
# 1. データの読み込み
# -----------------------------------------------------------
# Stata形式のデータファイルを読み込み、データフレーム "data" に格納
data <- read_dta("data/raw/DDCGdata_final.dta")

# ===========================================================
# 【A】 Populationages1564oftota 用前処理・分析・図表作成
# ===========================================================

# -----------------------------------------------------------
# A-1. データ整形（data_fx_1）
# -----------------------------------------------------------
data_fx_1 <- data %>%
  rename(id = "_ID") %>%
  group_by(id) %>%
  arrange(year) %>%
  ungroup()

# 民主化転換（transition）の識別
data_fx_1 <- data_fx_1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(prev_dem = dplyr::lag(dem, 1)) %>%  # 1期前の dem を取得
  ungroup() %>%
  mutate(transition = case_when(
    dem == 1 & prev_dem == 0 ~ 1,
    dem == 0 & prev_dem == 0 ~ 0,
    TRUE ~ NA_real_
  ))

# ラグ値の作成（Populationages1564oftota：1期前から4期前）
data_fx_1 <- data_fx_1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(
    lag1 = dplyr::lag(Populationages1564oftota, 1),
    lag2 = dplyr::lag(Populationages1564oftota, 2),
    lag3 = dplyr::lag(Populationages1564oftota, 3),
    lag4 = dplyr::lag(Populationages1564oftota, 4)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1) & !is.na(lag2) & !is.na(lag3) & !is.na(lag4))

# 相対的な Populationages1564oftota の変化（基準：lag1）を計算
# (a) 転換前期間 t = -15 ～ -2
for (t in -15:-2) {
  col_name <- paste0("age1564Diff_m", abs(t))  # 例: t = -15 → "age1564Diff_m15"
  data_fx_1 <- data_fx_1 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lag(Populationages1564oftota, abs(t)) - lag1) %>%
    ungroup()
}

# (b) 基準期間 t = -1 は 0
data_fx_1 <- data_fx_1 %>%
  mutate(age1564Diff_m1 = 0)

# (c) 転換直後およびその後 t = 0 ～ 30
# t = 0 の場合
data_fx_1 <- data_fx_1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(age1564Diff_0 = Populationages1564oftota - lag1) %>%
  ungroup()
# t > 0 の場合
for (t in 1:30) {
  col_name <- paste0("age1564Diff_p", t)  # 例: t = 1 → "age1564Diff_p1"
  data_fx_1 <- data_fx_1 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lead(Populationages1564oftota, t) - lag1) %>%
    ungroup()
}

# 分析対象は transition の値が判定できる観測に限定
data_fx_1 <- data_fx_1 %>% filter(!is.na(transition))

# -----------------------------------------------------------
# A-2. 回帰調整推定量（ATT）の算出関数の定義
# -----------------------------------------------------------
estimateATT <- function(dataset, outcome_col) {
  # アウトカム変数と transition が欠損していないサンプルを抽出
  sub_data <- dataset %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(transition))
  
  if (nrow(sub_data) == 0) return(NA)
  
  # 年（year）を因子化（水準はサブセット全体で統一）
  year_levels <- sort(unique(sub_data$year))
  sub_data <- sub_data %>%
    mutate(year_factor = factor(year, levels = year_levels))
  
  # 対照群（transition == 0）と処置群（transition == 1）のデータ抽出
  control_data <- sub_data %>% filter(transition == 0)
  treated_data <- sub_data %>% filter(transition == 1)
  
  # 対照群のデータが十分でなければ NA を返す
  if(nrow(control_data) < 2 || length(unique(control_data$year)) < 2) return(NA)
  
  # 年ダミー（year_factor）を説明変数とする定数項なし回帰モデルを推定
  model_formula <- as.formula(paste(outcome_col, "~ year_factor - 1"))
  control_model <- tryCatch(lm(model_formula, data = control_data),
                            error = function(e) NULL)
  if (is.null(control_model)) return(NA)
  
  # 対照群モデルに基づき、処置群の反実仮想アウトカムを予測
  predicted_outcomes <- tryCatch(predict(control_model, newdata = treated_data),
                                 error = function(e) rep(NA, nrow(treated_data)))
  
  # 各処置群観測で実際の値との差を計算し、平均を ATT とする
  treatment_effects <- treated_data[[outcome_col]] - predicted_outcomes
  mean(treatment_effects, na.rm = TRUE)
}

# -----------------------------------------------------------
# A-3. 各相対時点における ATT の算出
# -----------------------------------------------------------
relative_times <- c(seq(-15, -1), seq(0, 30))
atts_1 <- numeric(length(relative_times))

for (i in seq_along(relative_times)) {
  t_val <- relative_times[i]
  if (t_val < 0) {
    col_name <- paste0("age1564Diff_m", abs(t_val))
  } else {
    col_name <- if(t_val == 0) "age1564Diff_0" else paste0("age1564Diff_p", t_val)
  }
  atts_1[i] <- estimateATT(data_fx_1, col_name)
}

results_df_1 <- data.frame(RelativeTime = relative_times, ATT = atts_1)

# -----------------------------------------------------------
# A-4. 図表作成（Populationages1564oftota）
# -----------------------------------------------------------
figure_populationages1564oftota <- ggplot(results_df_1, aes(x = RelativeTime, y = ATT)) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  labs(x = "Years around Democratization", 
       y = "Change in Populationages1564oftota") +
  theme_bw()

# ===========================================================
# 【B】 Populationages014oftotal 用前処理・分析・図表作成
# ===========================================================

# -----------------------------------------------------------
# B-1. データ整形（data_fx_2）
# -----------------------------------------------------------
data_fx_2 <- data %>%
  rename(id = "_ID") %>%
  group_by(id) %>%
  arrange(year) %>%
  ungroup()

# 民主化転換（transition）の識別
data_fx_2 <- data_fx_2 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(prev_dem = dplyr::lag(dem, 1)) %>%  # 1期前の dem を取得
  ungroup() %>%
  mutate(transition = case_when(
    dem == 1 & prev_dem == 0 ~ 1,
    dem == 0 & prev_dem == 0 ~ 0,
    TRUE ~ NA_real_
  ))

# ラグ値の作成（Populationages014oftotal：1期前から4期前）
data_fx_2 <- data_fx_2 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(
    lag1 = dplyr::lag(Populationages014oftotal, 1),
    lag2 = dplyr::lag(Populationages014oftotal, 2),
    lag3 = dplyr::lag(Populationages014oftotal, 3),
    lag4 = dplyr::lag(Populationages014oftotal, 4)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1) & !is.na(lag2) & !is.na(lag3) & !is.na(lag4))

# 相対的な Populationages014oftotal の変化（基準：lag1）を計算
# (a) 転換前期間 t = -15 ～ -2
for (t in -15:-2) {
  col_name <- paste0("ageDiff_m", abs(t))  # 例: t = -15 → "ageDiff_m15"
  data_fx_2 <- data_fx_2 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lag(Populationages014oftotal, abs(t)) - lag1) %>%
    ungroup()
}

# (b) 基準期間 t = -1 は 0
data_fx_2 <- data_fx_2 %>%
  mutate(ageDiff_m1 = 0)

# (c) 転換直後およびその後 t = 0 ～ 30
# t = 0 の場合
data_fx_2 <- data_fx_2 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(ageDiff_0 = Populationages014oftotal - lag1) %>%
  ungroup()
# t > 0 の場合
for (t in 1:30) {
  col_name <- paste0("ageDiff_p", t)  # 例: t = 1 → "ageDiff_p1"
  data_fx_2 <- data_fx_2 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lead(Populationages014oftotal, t) - lag1) %>%
    ungroup()
}

# 分析対象は transition の値が判定できる観測に限定
data_fx_2 <- data_fx_2 %>% filter(!is.na(transition))

# -----------------------------------------------------------
# B-2. 各相対時点における ATT の算出
# -----------------------------------------------------------
atts_2 <- numeric(length(relative_times))

for (i in seq_along(relative_times)) {
  t_val <- relative_times[i]
  if (t_val < 0) {
    col_name <- paste0("ageDiff_m", abs(t_val))
  } else {
    col_name <- if(t_val == 0) "ageDiff_0" else paste0("ageDiff_p", t_val)
  }
  atts_2[i] <- estimateATT(data_fx_2, col_name)
}

results_df_2 <- data.frame(RelativeTime = relative_times, ATT = atts_2)

# -----------------------------------------------------------
# B-3. 図表作成（Populationages014oftotal）
# -----------------------------------------------------------
figure_populationages014oftotal <- ggplot(results_df_2, aes(x = RelativeTime, y = ATT)) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  labs(x = "Years around Democratization", 
       y = "Change in Populationages014oftotal") +
  theme_bw()

# ===========================================================
# 【C】 二つの図表を併記して保存
# ===========================================================
# ここでは patchwork を用いて横並び（例：左右）に配置
figure_combined <- figure_populationages1564oftota + figure_populationages014oftotal

# プロットを表示（必要に応じて）
print(figure_combined)

# -----------------------------------------------------------
# C-1. 図表の保存（例：PDFファイルに横並びで出力）
# -----------------------------------------------------------
ggsave("output/figure_combined.pdf", figure_combined, width = 14, height = 8, units = "cm")
