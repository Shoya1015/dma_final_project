# --- パッケージの読み込み ---
pacman::p_load(
  tidyverse,
  haven,
  knitr,         # LaTeX形式の表出力用
  kableExtra     # ヘッダー行追加などの拡張機能
)

# --- データの読み込み ---
data <- read_dta("data/raw/DDCGdata_final.dta")

# --- データ整形 ---
# 変数 "_ID" を "id" に名称変更し、各id（国や対象）のデータを年順に並べ替え
data_f1 <- data %>%
  rename(id = "_ID") %>%
  group_by(id) %>%
  arrange(year) %>%
  ungroup()

# -----------------------------------------------------------
# 3. 民主化転換（処置）の識別
# -----------------------------------------------------------
# 直前の期間の "dem" の値を取得し、以下のルールで "transition" 変数を作成：
#   - 直前が非民主主義（0）で現在が民主主義（1） → 1（民主化転換）
#   - 直前も現在も非民主主義（0） → 0（対照群）
#   - それ以外は NA とする
data_f1 <- data_f1 %>%
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
# 4. GDPのラグ値作成と欠損値の除外
# -----------------------------------------------------------
# GDP (変数 y) の1期前から4期前までの値を作成し、いずれかが欠損している行は除外
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
# 5. 相対的なGDP変化（基準：lag1）の計算
# -----------------------------------------------------------
# 民主化直前のGDP (lag1) を基準に、各期間でのGDP変化（差分）を求める
# 命名規則：
#   - t < 0：前期 → "gdpDiff_mX"（例: t = -15 → "gdpDiff_m15"）
#   - t = -1：基準（値は0） → "gdpDiff_m1"
#   - t = 0：転換直後 → "gdpDiff_0"
#   - t > 0：後期 → "gdpDiff_pX"（例: t = 1 → "gdpDiff_p1"）

# (a) 民主化転換前（t = -15～-2）
for (t in -15:-2) {
  col_name <- paste0("gdpDiff_m", abs(t))  # 例: t = -15 → "gdpDiff_m15"
  data_f1 <- data_f1 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lag(y, abs(t)) - lag1) %>%
    ungroup()
}

# (b) 基準期間：t = -1（固定値0）
data_f1 <- data_f1 %>%
  mutate(gdpDiff_m1 = 0)

# (c) 転換直後およびその後（t = 0～30）
#  t = 0：現在のyとの差分
data_f1 <- data_f1 %>%
  group_by(id) %>%
  arrange(year) %>%
  mutate(gdpDiff_0 = y - lag1) %>%
  ungroup()

#  t > 0：dplyr::leadを用いて未来のGDPとの差分を計算
for (t in 1:30) {
  col_name <- paste0("gdpDiff_p", t)  # 例: t = 1 → "gdpDiff_p1"
  data_f1 <- data_f1 %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(!!col_name := dplyr::lead(y, t) - lag1) %>%
    ungroup()
}

# 分析対象は transition の値が判定できる観測に限定
data_f1 <- data_f1 %>% filter(!is.na(transition))

# ======================================================
# 【IPW推定による ATT の算出関数の定義】
# ======================================================
# 以下の関数 compute_atet_ipw() は、指定したアウトカム変数 (outcome_var; 文字列)
# に対して、プロビット回帰で transition のプロペンシティスコアを推定し、
# コントロール群に対して重み (ps/(1-ps)) を付与した上で ATT を計算します。
compute_atet_ipw <- function(outcome_var, data) {
  
  # outcome_varに対応する変数が欠損でない観測を抽出
  df <- data %>%
    filter(!is.na(!!sym(outcome_var)),
           !is.na(transition),
           !is.na(lag1), !is.na(lag2), !is.na(lag3), !is.na(lag4),
           !is.na(year))
  
  # プロペンシティスコア推定（covariate: lag1～lag4 と年のダミー）
  prop_model <- glm(transition ~ lag1 + lag2 + lag3 + lag4 + factor(year),
                    data = df, family = binomial(link = "probit"))
  
  # 各観測についてのプロペンシティスコア
  df <- df %>% mutate(ps = predict(prop_model, type = "response"))
  
  # 重みの付与：treated (transition==1) は重み1、control (transition==0) は ps/(1-ps)
  df <- df %>% mutate(weight = ifelse(transition == 0, ps/(1 - ps), 1))
  
  # treated群のアウトカム平均
  treated_outcome <- df %>% filter(transition == 1) %>% pull(!!sym(outcome_var))
  
  # control群のアウトカムと対応する重み
  control_df <- df %>% filter(transition == 0)
  control_outcome <- control_df[[outcome_var]]
  control_weight  <- control_df$weight
  
  # ATT の計算：treated の平均 - 重み付き control の平均
  att <- mean(treated_outcome) - (sum(control_outcome * control_weight) / sum(control_weight))
  
  return(att)
}

# --- ブートストラップによる標準誤差の計算用関数 ---
compute_att_ipw_boot <- function(outcome_var, data, B = 200) {
  att_est <- compute_atet_ipw(outcome_var, data)
  n <- nrow(data)
  boot_est <- numeric(B)
  # ※ 各 outcome ごとに同じ seed を使うと再現性は保たれますが、必要に応じて seed の設定を調整してください
  set.seed(123)
  for (b in 1:B) {
    boot_indices <- sample(1:n, size = n, replace = TRUE)
    boot_data <- data[boot_indices, ]
    boot_est[b] <- compute_atet_ipw(outcome_var, boot_data)
  }
  se_est <- sd(boot_est)
  return(list(att = att_est, se = se_est, boot = boot_est))
}

# ======================================================
# 【各相対期間ごとの ATT を計算】
# ======================================================
# 対象アウトカム変数の名称をベクトルに格納：
#  民主化転換前（t = -15～-2）： gdpDiff_m15, ..., gdpDiff_m2
#  基準（t = -1）： gdpDiff_m1 (値は0)
#  転換直後～その後（t = 0～30）： gdpDiff_0, gdpDiff_p1, ..., gdpDiff_p30
outcome_vars <- c(
  paste0("gdpDiff_m", 15:2),
  "gdpDiff_m1",
  "gdpDiff_0",
  paste0("gdpDiff_p", 1:30)
)

# 各アウトカム変数ごとに ATT とそのブートストラップ結果を計算し、結果をリストに格納
att_results <- list()
for (var in outcome_vars) {
  att_results[[var]] <- compute_att_ipw_boot(var, data_f1, B = 200)
}

# ======================================================
# 【グループごとの平均効果と標準誤差の集計】
# ======================================================
# グループ定義（Stata の Table 5 に対応する例）
group_definitions <- list(
  "-5 to -1" = c("gdpDiff_m5", "gdpDiff_m4", "gdpDiff_m3", "gdpDiff_m2", "gdpDiff_m1"),
  "0 to 4"   = c("gdpDiff_0", "gdpDiff_p1", "gdpDiff_p2", "gdpDiff_p3", "gdpDiff_p4"),
  "5 to 9"   = paste0("gdpDiff_p", 5:9),
  "10 to 14" = paste0("gdpDiff_p", 10:14),
  "15 to 19" = paste0("gdpDiff_p", 15:19),
  "20 to 24" = paste0("gdpDiff_p", 20:24),
  "26 to 30" = paste0("gdpDiff_p", 26:30)
)

# 各グループごとに、各アウトカム変数の ATT の平均値と、
# 各ブートストラップ replicates の平均（行平均）から標準誤差を算出
group_results <- list()
for (grp in names(group_definitions)) {
  vars_in_grp <- group_definitions[[grp]]
  # 各 outcome の ATT とブート replicatesを抽出
  att_vec <- sapply(vars_in_grp, function(x) att_results[[x]]$att)
  # 組み合わせ：各 outcomeのブートベクトル（各列が outcome のブート replicates）
  boot_mat <- sapply(vars_in_grp, function(x) att_results[[x]]$boot)
  # 各ブート replicateごとにグループの平均値（列ごとの平均ではなく，各行の平均）
  grp_boot <- rowMeans(boot_mat)
  grp_att <- mean(att_vec)
  grp_se <- sd(grp_boot)
  group_results[[grp]] <- list(att = grp_att, se = grp_se)
}

# --- 作成するテーブル用に文字列を作成 ---
# 各セルは「係数（ATT）」の下に「(標準誤差)」を表示する形にする
group_names <- names(group_results)
table_values <- sapply(group_names, function(grp) {
  sprintf("%.3f", group_results[[grp]]$att)
})
table_ses <- sapply(group_names, function(grp) {
  sprintf("(%.3f)", group_results[[grp]]$se)
})
# 1セルに「値」＋改行＋「(SE)」の文字列を作成
cell_text <- mapply(function(val, se) {
  paste0(val, "\n", se)
}, table_values, table_ses, SIMPLIFY = TRUE)

# 1行のデータフレーム（列名をグループ名に）
results_df <- as.data.frame(t(cell_text))
colnames(results_df) <- group_names

results_df <- results_df |> 
  rename("-5 to -1" = "-5 to -1 (years)") |> 
  mutate(years = "ATT on GDP (Log)") 
  
results_df <- results_df |> 
  select(years, everything())

table_latex <- results_df %>%
  kable(format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        caption = "Semiparametric Estimates of the Effect of Democratizations on GDP per Capita (Log)",
        label = "tab:table_5_ipw",
        digits = 3) %>%
  add_header_above(c("Inverse propensity score reweighting" = ncol(results_df))) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

writeLines(table_latex, con = "output/table_5_ipw.tex")
