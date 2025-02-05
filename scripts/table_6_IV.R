# ---------------------------
# 1. 必要なパッケージの読み込み
# ---------------------------
pacman::p_load(
  haven,       # Stata形式のデータ読み込みに利用
  tidyverse,   # データ操作や可視化に利用
  plm,         # パネルデータの解析に利用
  texreg       # LaTeX形式の回帰結果出力に利用
)

# ---------------------------
# 2. データの読み込みと前処理
# ---------------------------
# Stata形式のデータを読み込み
data <- read_dta("data/raw/DDCGdata_final.dta")

# 各国ごとに年ごとに並べ、被説明変数 y のラグ変数 (lag1～lag8) を作成する
data_t6 <- data %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(
    lag1 = dplyr::lag(y, 1),
    lag2 = dplyr::lag(y, 2),
    lag3 = dplyr::lag(y, 3),
    lag4 = dplyr::lag(y, 4),
    lag5 = dplyr::lag(y, 5),
    lag6 = dplyr::lag(y, 6),
    lag7 = dplyr::lag(y, 7),
    lag8 = dplyr::lag(y, 8)
  ) %>%
  ungroup()

# pdata.frame に変換（パネルデータとして利用するため）
data_t6 <- pdata.frame(data_t6, index = c("country_name", "year"))

# ---------------------------
# 3. IV 推定モデルの構築
# ---------------------------
# モデル1: 説明変数は dem と y の1～4期のラグ、ツーウェイズ効果
#         外生変数（道具変数）として、demreg の1期ラグと y の1～4期ラグを利用
model_iv_1 <- plm(
  y ~ dem + plm::lag(y, 1:4) | 
    plm::lag(demreg, 1) + plm::lag(y, 1:4),
  data = data_t6,
  effect = "twoways"
)

# モデル2: IV の遅行項を demreg の1～4期ラグに変更（その他はモデル1と同様）
model_iv_2 <- plm(
  y ~ dem + plm::lag(y, 1:4) | 
    plm::lag(demreg, 1:4) + plm::lag(y, 1:4),
  data = data_t6,
  effect = "twoways"
)

# モデル3: モデル2 に加え、ソビエト連邦関連のダミー変数（sov1 ～ sov4）を追加
model_iv_3 <- plm(
  y ~ dem + plm::lag(y, 1:4) + sov1 + sov2 + sov3 + sov4 | 
    plm::lag(demreg, 1:4) + plm::lag(y, 1:4) + sov1 + sov2 + sov3 + sov4,
  data = data_t6,
  effect = "twoways"
)

# モデル4: 地域トレンド変数（rtrend2 ～ rtrend7）を追加。固定効果モデル（withinモデル）を利用
model_iv_4 <- plm(
  y ~ dem + plm::lag(y, 1:4) + rtrend2 + rtrend3 + rtrend4 + rtrend5 + rtrend6 + rtrend7 | 
    plm::lag(demreg, 1:4) + plm::lag(y, 1:4) + rtrend2 + rtrend3 + rtrend4 + rtrend5 + rtrend6 + rtrend7,
  data = data_t6,
  effect = "twoways",
  model = "within"
)

# ---------------------------
# 4. 長期効果 (Long-run effect) の計算
# ---------------------------
# 各モデルについて、dem の係数を beta_hat、y のラグ1～4の係数を gamma_hat として、
# 長期効果 = beta_hat / (1 - sum(gamma_hat)) と計算する

# モデル1の場合
beta_hat_1 <- coef(model_iv_1)["dem"]
gamma_hat_1 <- coef(model_iv_1)[2:5]
long_run_effect_1 <- beta_hat_1 / (1 - sum(gamma_hat_1))

# モデル2の場合
beta_hat_2 <- coef(model_iv_2)["dem"]
gamma_hat_2 <- coef(model_iv_2)[2:5]
long_run_effect_2 <- beta_hat_2 / (1 - sum(gamma_hat_2))

# モデル3の場合
beta_hat_3 <- coef(model_iv_3)["dem"]
gamma_hat_3 <- coef(model_iv_3)[2:5]
long_run_effect_3 <- beta_hat_3 / (1 - sum(gamma_hat_3))

# モデル4の場合
beta_hat_4 <- coef(model_iv_4)["dem"]
gamma_hat_4 <- coef(model_iv_4)[2:5]
long_run_effect_4 <- beta_hat_4 / (1 - sum(gamma_hat_4))

# 長期効果をまとめ、四捨五入して小数点第3位まで表示
lre <- c(long_run_effect_1, long_run_effect_2, long_run_effect_3, long_run_effect_4)
lre <- round(lre, 3)
print(lre)
# 結果例: 26.315 31.521 35.723 36.788

# ---------------------------
# 5. 25年後の効果 (Effect after 25 years) の計算
# ---------------------------
# 各モデルについて、短期効果を再帰的に計算し、25期目の効果を抽出

# 空のオブジェクト sre を用意（各モデルの25年後の効果を格納）
sre <- c()

# --- モデル1 の場合 ---
dem_shortrun <- coef(model_iv_1)["dem"]
lag1 <- coef(model_iv_1)[2]
lag2 <- coef(model_iv_1)[3]
lag3 <- coef(model_iv_1)[4]
lag4 <- coef(model_iv_1)[5]

# 初期効果（1期目～4期目）の計算
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun
effect4 <- (effect3 * lag1) + (effect2 * lag2) + (effect1 * lag3) + dem_shortrun

effects <- c(effect1, effect2, effect3, effect4)

# 5期目から30期目までの効果を再帰的に計算
for (i in 5:30) {
  eff <- (effects[i-1] * lag1) +
    (effects[i-2] * lag2) +
    (effects[i-3] * lag3) +
    (effects[i-4] * lag4) +
    dem_shortrun
  effects <- c(effects, eff)
}
# 25期目の効果を抽出
sre <- c(sre, effects[25])

# --- モデル2 の場合 ---
dem_shortrun <- coef(model_iv_2)["dem"]
lag1 <- coef(model_iv_2)[2]
lag2 <- coef(model_iv_2)[3]
lag3 <- coef(model_iv_2)[4]
lag4 <- coef(model_iv_2)[5]

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun
effect4 <- (effect3 * lag1) + (effect2 * lag2) + (effect1 * lag3) + dem_shortrun

effects <- c(effect1, effect2, effect3, effect4)
for (i in 5:30) {
  eff <- (effects[i-1] * lag1) +
    (effects[i-2] * lag2) +
    (effects[i-3] * lag3) +
    (effects[i-4] * lag4) +
    dem_shortrun
  effects <- c(effects, eff)
}
sre <- c(sre, effects[25])

# --- モデル3 の場合 ---
dem_shortrun <- coef(model_iv_3)["dem"]
lag1 <- coef(model_iv_3)[2]
lag2 <- coef(model_iv_3)[3]
lag3 <- coef(model_iv_3)[4]
lag4 <- coef(model_iv_3)[5]

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun
effect4 <- (effect3 * lag1) + (effect2 * lag2) + (effect1 * lag3) + dem_shortrun

effects <- c(effect1, effect2, effect3, effect4)
for (i in 5:30) {
  eff <- (effects[i-1] * lag1) +
    (effects[i-2] * lag2) +
    (effects[i-3] * lag3) +
    (effects[i-4] * lag4) +
    dem_shortrun
  effects <- c(effects, eff)
}
sre <- c(sre, effects[25])

# --- モデル4 の場合 ---
dem_shortrun <- coef(model_iv_4)["dem"]
lag1 <- coef(model_iv_4)[2]
lag2 <- coef(model_iv_4)[3]
lag3 <- coef(model_iv_4)[4]
lag4 <- coef(model_iv_4)[5]

effect1 <- dem_shortrun
effect2 <- (effect1 * lag1) + dem_shortrun
effect3 <- (effect2 * lag1) + (effect1 * lag2) + dem_shortrun
effect4 <- (effect3 * lag1) + (effect2 * lag2) + (effect1 * lag3) + dem_shortrun

effects <- c(effect1, effect2, effect3, effect4)
for (i in 5:30) {
  eff <- (effects[i-1] * lag1) +
    (effects[i-2] * lag2) +
    (effects[i-3] * lag3) +
    (effects[i-4] * lag4) +
    dem_shortrun
  effects <- c(effects, eff)
}
sre <- c(sre, effects[25])

# 25年後の効果を小数点第3位に丸める
sre <- round(sre, 3)
print(sre)
# 結果例: 20.836 24.866 27.929 32.051

# ---------------------------
# 6. Persistence (持続性) の計算
# ---------------------------
# 各モデルにおいて、y のラグ1～4の係数の和を持続性として算出
pers1 <- sum(coef(model_iv_1)[2:5])
pers2 <- sum(coef(model_iv_2)[2:5])
pers3 <- sum(coef(model_iv_3)[2:5])
pers4 <- sum(coef(model_iv_4)[2:5])

pers <- c(pers1, pers2, pers3, pers4)
pers <- round(pers, 3)
print(pers)
# 結果例: 0.963 0.964 0.964 0.954

# ---------------------------
# 7. override 用オブジェクトの定義と texreg による LaTeX 出力
# ---------------------------
# 各モデルから "dem" の係数とその標準誤差を抽出

override.coef.1 <- coef(model_iv_1)["dem", drop = FALSE]
override.coef.2 <- coef(model_iv_2)["dem", drop = FALSE]
override.coef.3 <- coef(model_iv_3)["dem", drop = FALSE]
override.coef.4 <- coef(model_iv_4)["dem", drop = FALSE]

override.se.1 <- sqrt(diag(vcov(model_iv_1)))["dem"]
override.se.2 <- sqrt(diag(vcov(model_iv_2)))["dem"]
override.se.3 <- sqrt(diag(vcov(model_iv_3)))["dem"]
override.se.4 <- sqrt(diag(vcov(model_iv_4)))["dem"]

# モデルのリスト作成
models <- list(model_iv_1, model_iv_2, model_iv_3, model_iv_4)

# texreg による LaTeX テーブルの出力
# ※ custom.coef.map を用いることで、表示する係数を "dem" のみとし、ラベルを "Democracy" としています
texreg(
  models,
  override.coef = list(
    override.coef.1,
    override.coef.2,
    override.coef.3,
    override.coef.4
  ),
  override.se = list(
    override.se.1,
    override.se.2,
    override.se.3,
    override.se.4
  ),
  custom.model.names = c("1 Lag", "4 Lags", "Soviet Dummies", "Regional Trends"),
  custom.coef.map = list(dem = "Democracy"),
  custom.gof.rows = list(
    "Persistence"           = pers,
    "Long run effect"       = lre,
    "Effect after 25 years" = sre
  ),
  file = "output/table_6_iv.tex",
  caption = "Effect of Democracy on (Log) GDP per Capita",
  include.rsquared = FALSE,
  include.adjrs    = FALSE,
  include.fstat    = FALSE
)
