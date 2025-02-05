# -----------------------------------------------------------
# 0. 必要なパッケージの読み込み
# -----------------------------------------------------------
pacman::p_load(tidyverse, haven)

# -----------------------------------------------------------
# 1. データの読み込みと基本整形
# -----------------------------------------------------------
data <- read_dta("data/raw/DDCGdata_final.dta")

data_f1 <- data %>%
  rename(id = "_ID") %>%
  group_by(id) %>%
  arrange(year) %>%
  ungroup()

# -----------------------------------------------------------
# 2. 民主化転換（処置）の識別
# -----------------------------------------------------------
data_f1 <- data_f1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(prev_dem = dplyr::lag(dem, 1)) %>%  
  ungroup() %>%
  mutate(transition = case_when(
    dem == 1 & prev_dem == 0 ~ 1,
    dem == 0 & prev_dem == 0 ~ 0,
    TRUE ~ NA_real_
  ))

# -----------------------------------------------------------
# 3. GDPのラグ値作成と欠損値の除外
# -----------------------------------------------------------
data_f1 <- data_f1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(
    lag1 = dplyr::lag(y, 1),
    lag2 = dplyr::lag(y, 2),
    lag3 = dplyr::lag(y, 3),
    lag4 = dplyr::lag(y, 4)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1) & !is.na(lag2) & !is.na(lag3) & !is.na(lag4))

# -----------------------------------------------------------
# 4. 相対的なGDP変化（基準：lag1）を計算
# -----------------------------------------------------------
# (a) 民主化転換前: t = -15 ～ -2
for (t in -15:-2) {
  col_name <- paste0("gdpDiff_m", abs(t))
  data_f1 <- data_f1 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lag(y, abs(t)) - lag1) %>%
    ungroup()
}

# (b) 基準期間: t = -1 は 0 とする
data_f1 <- data_f1 %>%
  mutate(gdpDiff_m1 = 0)

# (c) 民主化転換直後および後続: t = 0 ～ 30
data_f1 <- data_f1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(gdpDiff_0 = y - lag1) %>%
  ungroup()

for (t in 1:30) {
  col_name <- paste0("gdpDiff_p", t)
  data_f1 <- data_f1 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lead(y, t) - lag1) %>%
    ungroup()
}

# 分析対象を絞る
data_f1 <- data_f1 %>%
  filter(!is.na(transition))

# -----------------------------------------------------------
# 5. 回帰調整推定量（ATET）を求める関数
# -----------------------------------------------------------
estimateATT <- function(dataset, outcome_col) {
  # 欠損がないものを抽出
  sub_data <- dataset %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(transition))
  
  if(nrow(sub_data) == 0) return(NA)
  
  # 年ダミーを作成（factor化）
  year_levels <- sort(unique(sub_data$year))
  sub_data <- sub_data %>%
    mutate(year_factor = factor(year, levels = year_levels))
  
  # 対照群・処置群に分割
  control_data <- sub_data %>% filter(transition == 0)
  treated_data <- sub_data %>% filter(transition == 1)
  
  # 対照群が少なすぎる場合などは NA
  if(nrow(control_data) < 2 || length(unique(control_data$year)) < 2) return(NA)
  
  # 対照群で年ダミー回帰 (定数項なし)
  model_formula <- as.formula(paste(outcome_col, "~ year_factor - 1"))
  control_model <- tryCatch(lm(model_formula, data = control_data),
                            error = function(e) NULL)
  if(is.null(control_model)) return(NA)
  
  # 処置群に対する反実仮想値を予測
  predicted_outcomes <- tryCatch(
    predict(control_model, newdata = treated_data),
    error = function(e) rep(NA, nrow(treated_data))
  )
  
  # 実測 - 反実仮想 の平均が ATT
  treatment_effects <- treated_data[[outcome_col]] - predicted_outcomes
  mean(treatment_effects, na.rm = TRUE)
}

# -----------------------------------------------------------
# 6. 各相対時点における ATET (ポイント推定) を計算
# -----------------------------------------------------------
relative_times <- c(seq(-15, -1), seq(0, 30))
point_estimates <- numeric(length(relative_times))

for (i in seq_along(relative_times)) {
  t_val <- relative_times[i]
  if(t_val < 0) {
    col_name <- paste0("gdpDiff_m", abs(t_val))
  } else {
    col_name <- if(t_val == 0) "gdpDiff_0" else paste0("gdpDiff_p", t_val)
  }
  point_estimates[i] <- estimateATT(data_f1, col_name)
}

results_df <- data.frame(
  RelativeTime = relative_times,
  ATT = point_estimates
)

# イベントスタディの推移をプロット (ポイント推定のみ)
figure_1 <- ggplot(results_df, aes(x = RelativeTime, y = ATT)) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  labs(x = "Years around Democratization", 
       y = "Change in GDP per capita (log points)") +
  theme_bw()

ggsave("output/figure_1.pdf", figure_1, width = 14, height = 8, units = "cm")

# -----------------------------------------------------------
# 7. (新規) 95%信頼区間を導出：クラスタ・ブートストラップ
# -----------------------------------------------------------

# (A) 相対時点ごとの ATET を再計算するヘルパー関数
#     （ブートストラップしたサンプルに対してATETベクトルを返す）
compute_atets <- function(data_boot) {
  out <- numeric(length(relative_times))
  for (i in seq_along(relative_times)) {
    t_val <- relative_times[i]
    if(t_val < 0) {
      col_name <- paste0("gdpDiff_m", abs(t_val))
    } else {
      col_name <- if(t_val == 0) "gdpDiff_0" else paste0("gdpDiff_p", t_val)
    }
    out[i] <- estimateATT(data_boot, col_name)
  }
  out
}

# (B) ブートストラップの設定
B <- 200  # 繰り返し回数（例: 200; 必要に応じて増やす）
set.seed(123)  # 乱数シード

boot_mat <- matrix(NA, nrow = B, ncol = length(relative_times))

# (C) クラスター（国 id）単位の再サンプリング
unique_ids <- unique(data_f1$id)

for (b in seq_len(B)) {
  # クラスター（id）を復元抽出
  sampled_ids <- sample(unique_ids, size = length(unique_ids), replace = TRUE)
  
  # id ごとに観測を束ねてブートストラップデータを作成
  bs_data <- lapply(sampled_ids, function(x) {
    # 同じidが重複で選ばれた場合、観測をその回数ぶん繰り返す
    data_f1[data_f1$id == x, ]
  }) %>%
    bind_rows()
  
  # ブートストラップサンプルでの ATTを算出
  boot_mat[b, ] <- compute_atets(bs_data)
}

# (D) 信頼区間を計算（2通りの方法の例）

# (1) 正規近似 (Normal Approx.) によるCI
boot_se <- apply(boot_mat, 2, sd, na.rm=TRUE)
ci_lower_normal <- point_estimates - 1.96 * boot_se
ci_upper_normal <- point_estimates + 1.96 * boot_se

# (2) パーセンタイル法 (Percentile method)
ci_lower_perc <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm=TRUE)
ci_upper_perc <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm=TRUE)

# (E) 結果をデータフレームに格納
results_with_ci <- data.frame(
  RelativeTime = relative_times,
  ATT = point_estimates,
  ciL_normal = ci_lower_normal,
  ciU_normal = ci_upper_normal,
  ciL_perc = ci_lower_perc,
  ciU_perc = ci_upper_perc
)

# -----------------------------------------------------------
# 8. プロット: 正規近似CIを例に描画
# -----------------------------------------------------------
figure_1_withCI <- ggplot(results_with_ci, aes(x = RelativeTime, y = ATT)) +
  geom_line(color = "black") +
  # リボンで 95%CI (normal approx)
  geom_ribbon(aes(ymin = ciL_normal, ymax = ciU_normal),
              fill = "gray50", alpha = 0.3) +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  labs(x = "Years around Democratization",
       y = "Change in GDP per capita (log points)") +
  theme_bw()

ggsave("output/figure_1_withCI.pdf", figure_1_withCI, width = 14, height = 8, units = "cm")
