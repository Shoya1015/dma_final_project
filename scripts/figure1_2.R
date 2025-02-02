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
data <- data %>%
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
data <- data %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(prev_dem = lag(dem, 1)) %>%  # 1期前の "dem" を取得
  ungroup() %>%
  mutate(transition = case_when(
    dem == 1 & prev_dem == 0 ~ 1,
    dem == 0 & prev_dem == 0 ~ 0,
    TRUE ~ NA_real_
  ))

# -----------------------------------------------------------
# 4. GDPのラグ値作成と欠損値の除外
# -----------------------------------------------------------
# GDP (変数 y) の1期前から4期前までの値を作成し、いずれかが欠損している行は除外
data <- data %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(
    lag1 = lag(y, 1),
    lag2 = lag(y, 2),
    lag3 = lag(y, 3),
    lag4 = lag(y, 4)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1) & !is.na(lag2) & !is.na(lag3) & !is.na(lag4))

# -----------------------------------------------------------
# 5. 相対的なGDP変化（基準：lag1）を計算する
# -----------------------------------------------------------
# ここでは、民主化直前のGDP (lag1) を基準に、各期間でのGDPの変化（差分）を求めます。
# ※ 「相対時点」は、民主化転換前後の年を示し、以下のように命名しています：
#     - t < 0：民主化転換前。t = -15～-2 の場合、lag(y, abs(t)) - lag1 を計算
#     - t = -1：民主化直前の期間。ここは基準なので 0 とする
#     - t = 0：民主化転換直後。y - lag1 を計算
#     - t > 0：民主化転換後。lead(y, t) - lag1 を計算
#
# 命名規則：
#   - 前期は "gdpDiff_mX" （例：t = -15 → "gdpDiff_m15"）
#   - 基準は t = -1 → "gdpDiff_m1"（値は 0）
#   - 転換直後は t = 0 → "gdpDiff_0"
#   - 後期は "gdpDiff_pX" （例：t = 1 → "gdpDiff_p1"）

# (a) 民主化転換前の期間：t = -15 ～ -2
for (t in -15:-2) {
  col_name <- paste0("gdpDiff_m", abs(t))  # 例: t = -15 → "gdpDiff_m15"
  data <- data %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := lag(y, abs(t)) - lag1) %>%
    ungroup()
}

# (b) 基準期間：t = -1 は基準なので 0 とする
data <- data %>%
  mutate(gdpDiff_m1 = 0)

# (c) 民主化転換直後およびその後の期間：t = 0 ～ 30
# まず、t = 0 の場合は現在のyとの差分
data <- data %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(gdpDiff_0 = y - lag1) %>%
  ungroup()

# t > 0 の場合は lead を使って未来のGDPを取得
for (t in 1:30) {
  col_name <- paste0("gdpDiff_p", t)  # 例: t = 1 → "gdpDiff_p1"
  data <- data %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := lead(y, t) - lag1) %>%
    ungroup()
}

# ※ 分析対象は transition (民主化転換) の値が判定できる観測に限定
data <- data %>% filter(!is.na(transition))

# -----------------------------------------------------------
# 6. 回帰調整推定量（ATET）を求める関数の定義
# -----------------------------------------------------------
# この関数は、指定したGDP変化のアウトカム変数（例："gdpDiff_m15" など）について、
# 対照群（transition == 0）のみで年ダミーを用いた回帰モデルを推定し、処置群（transition == 1）
# に対して反実仮想のアウトカムを予測、実際の値との差を計算して平均した値（ATET）を返します。
estimateATET <- function(dataset, outcome_col) {
  # アウトカム変数とtransitionが欠損していないサンプルを抽出
  sub_data <- dataset %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(transition))
  
  if(nrow(sub_data) == 0) return(NA)
  
  # 年（year）を因子として扱う（水準はサブセット全体で統一）
  year_levels <- sort(unique(sub_data$year))
  sub_data <- sub_data %>%
    mutate(year_factor = factor(year, levels = year_levels))
  
  # 対照群（transitionが0）のデータを抽出
  control_data <- sub_data %>% filter(transition == 0)
  # 処置群（transitionが1）のデータを抽出
  treated_data <- sub_data %>% filter(transition == 1)
  
  # コントロール群のデータが十分でなければ NA を返す
  if(nrow(control_data) < 2 || length(unique(control_data$year)) < 2) return(NA)
  
  # 年ダミー（year_factor）を説明変数として定数項なし回帰モデルを推定
  model_formula <- as.formula(paste(outcome_col, "~ year_factor - 1"))
  control_model <- tryCatch(lm(model_formula, data = control_data),
                            error = function(e) NULL)
  if(is.null(control_model)) return(NA)
  
  # 対照群で推定したモデルを用いて、処置群の反実仮想アウトカムを予測
  predicted_outcomes <- tryCatch(predict(control_model, newdata = treated_data),
                                 error = function(e) rep(NA, nrow(treated_data)))
  
  # 各処置群の観測で、実際のアウトカムと予測値との差（効果）を計算し、その平均をATETとする
  treatment_effects <- treated_data[[outcome_col]] - predicted_outcomes
  mean(treatment_effects, na.rm = TRUE)
}

# -----------------------------------------------------------
# 7. 各「相対時点」におけるATETの算出
# -----------------------------------------------------------
# 相対時点：民主化転換前は -15 ～ -1、転換後は 0 ～ 30
relative_times <- c(seq(-15, -1), seq(0, 30))
atets <- numeric(length(relative_times))

# 各相対時点ごとに、対応するGDP差分の変数名を決定し、ATETを計算する
for (i in seq_along(relative_times)) {
  t_val <- relative_times[i]
  if(t_val < 0) {
    # tが負の場合：例 t = -15 → "gdpDiff_m15"
    col_name <- paste0("gdpDiff_m", abs(t_val))
  } else {
    # tが0の場合は "gdpDiff_0"、正の場合は "gdpDiff_p{t}"
    col_name <- if(t_val == 0) "gdpDiff_0" else paste0("gdpDiff_p", t_val)
  }
  atets[i] <- estimateATET(data, col_name)
}

# 結果をデータフレームにまとめる
results_df <- data.frame(RelativeTime = relative_times, ATET = atets)

# -----------------------------------------------------------
# 8. 結果のプロット
# -----------------------------------------------------------
ggplot(results_df, aes(x = RelativeTime, y = ATET)) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = seq(-15, 30, 5)) +
  labs(x = "Years around Democratization", 
       y = "Change in GDP per capita (log points)") +
  theme_minimal()
