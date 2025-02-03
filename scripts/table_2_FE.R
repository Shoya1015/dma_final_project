# -----------------------------------------------------------
# 1. パッケージの読み込み
# -----------------------------------------------------------
pacman::p_load(
  plm,         # パネル回帰
  tidyverse,   # データ操作
  haven,       # Stataデータの読み込み
  stargazer   # 結果出力（HTMLやテキスト）
)

# -----------------------------------------------------------
# 2. データの読み込み
# -----------------------------------------------------------
data <- read_dta("data/raw/DDCGdata_final.dta") 

# -----------------------------------------------------------
# 3. 前処理：必要な変数（ここでは1:30列）を選択し、各国ごとに年順でラグ変数を作成
# -----------------------------------------------------------
# ※ country_name, year, y, dem など、必要な変数が含まれていることを確認してください
data_t2 <- data %>% 
  select(1:30) %>% 
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

# 前処理結果の確認
head(data_t2)

# -----------------------------------------------------------
# 4. モデル1：必要な変数は y, dem, lag1
# -----------------------------------------------------------
# plm は欠損値があるとエラーとなるため、必要な変数の欠損値を除外
data_m1 <- data_t2 %>% 
  drop_na(y, dem, lag1)

# pdata.frame に変換（インデックスは country_name と year）
data_m1 <- pdata.frame(data_m1, index = c("country_name", "year"))

# 両方向固定効果モデル（within estimator）を推定
model_1 <- plm(
  y ~ dem + lag1,
  data = data_m1,
  model = "within",
  effect = "twoways"
)

# -----------------------------------------------------------
# 5. モデル2：必要な変数は y, dem, lag1, lag2
# -----------------------------------------------------------
data_m2 <- data_t2 %>% 
  drop_na(y, dem, lag1, lag2)

data_m2 <- pdata.frame(data_m2, index = c("country_name", "year"))

model_2 <- plm(
  y ~ dem + lag1 + lag2,
  data = data_m2,
  model = "within",
  effect = "twoways"
)

# -----------------------------------------------------------
# 6. モデル3：必要な変数は y, dem, lag1, lag2, lag3, lag4
# -----------------------------------------------------------
data_m3 <- data_t2 %>% 
  drop_na(y, dem, lag1, lag2, lag3, lag4)

data_m3 <- pdata.frame(data_m3, index = c("country_name", "year"))

model_3 <- plm(
  y ~ dem + lag1 + lag2 + lag3 + lag4,
  data = data_m3,
  model = "within",
  effect = "twoways"
)

# -----------------------------------------------------------
# 7. モデル4：必要な変数は y, dem, lag1, lag2, lag3, lag4, lag5, lag6, lag7, lag8
# -----------------------------------------------------------
data_m4 <- data_t2 %>% 
  drop_na(y, dem, lag1, lag2, lag3, lag4, lag5, lag6, lag7, lag8)

data_m4 <- pdata.frame(data_m4, index = c("country_name", "year"))

model_4 <- plm(
  y ~ dem + lag1 + lag2 + lag3 + lag4 + lag5 + lag6 + lag7 + lag8,
  data = data_m4,
  model = "within",
  effect = "twoways"
)

# -----------------------------------------------------------
# 8. Long Run Effect の計算
# -----------------------------------------------------------
# 各モデルにおいて、民主主義（dem）の係数（beta）とラグの係数群（gamma）から
# 長期効果を計算する（長期効果 = beta / (1 - sum(gamma))）
# モデル1
beta_hat_1  <- coef(model_1)["dem"]
gamma_hat_1 <- coef(model_1)[c("lag1")]
long_run_effect_1 <- beta_hat_1 / (1 - sum(gamma_hat_1))

# モデル2
beta_hat_2  <- coef(model_2)["dem"]
gamma_hat_2 <- coef(model_2)[c("lag1", "lag2")]
long_run_effect_2 <- beta_hat_2 / (1 - sum(gamma_hat_2))

# モデル3
beta_hat_3  <- coef(model_3)["dem"]
gamma_hat_3 <- coef(model_3)[c("lag1", "lag2", "lag3", "lag4")]
long_run_effect_3 <- beta_hat_3 / (1 - sum(gamma_hat_3))

# モデル4
beta_hat_4  <- coef(model_4)["dem"]
gamma_hat_4 <- coef(model_4)[c("lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8")]
long_run_effect_4 <- beta_hat_4 / (1 - sum(gamma_hat_4))

lre <- c(long_run_effect_1, long_run_effect_2, long_run_effect_3, long_run_effect_4)
lre <- round(lre, 3)
print(lre)

# -----------------------------------------------------------
# 9. Persistence（持続性）の計算
# -----------------------------------------------------------
# 各モデルにおいて、ラグ係数の合計（dem を除く）を Persistence とする
pers1 <- sum(coef(model_1)[2])
pers2 <- sum(coef(model_2)[2:3])
pers3 <- sum(coef(model_3)[2:5])
pers4 <- sum(coef(model_4)[2:9])
pers <- c(pers1, pers2, pers3, pers4)
pers <- round(pers, 3)
print(pers)

# -----------------------------------------------------------
# 10. 25年後の累積効果の計算
# -----------------------------------------------------------
# 各モデルについて、動的に累積効果を計算し、25年後の効果（25期後の効果）を求める

# モデル1 の場合（1期目：dem の短期効果＝直接の係数）
dem_shortrun <- coef(model_1)["dem"]
lag1_mod1 <- coef(model_1)[2]  # model_1 の lag1 係数
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod1) + dem_shortrun

effects_mod1 <- c(effect1, effect2)
for (i in 3:30) {
  eff <- (effects_mod1[i-1] * lag1_mod1) + dem_shortrun
  effects_mod1 <- c(effects_mod1, eff)
}
eff_25_1 <- effects_mod1[25]

# モデル2 の場合（2期分：lag1 と lag2 を考慮）
dem_shortrun <- coef(model_2)["dem"]
lag1_mod2 <- coef(model_2)[2]
lag2_mod2 <- coef(model_2)[3]
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod2) + dem_shortrun
effect3 <- (effect2 * lag1_mod2) + (effect1 * lag2_mod2) + dem_shortrun

effects_mod2 <- c(effect1, effect2, effect3)
for (i in 4:30) {
  eff <- (effects_mod2[i-1] * lag1_mod2) + (effects_mod2[i-2] * lag2_mod2) + dem_shortrun
  effects_mod2 <- c(effects_mod2, eff)
}
eff_25_2 <- effects_mod2[25]

# モデル3 の場合（4期分：lag1, lag2, lag3, lag4 を考慮）
dem_shortrun <- coef(model_3)["dem"]
lag1_mod3 <- coef(model_3)[2]
lag2_mod3 <- coef(model_3)[3]
lag3_mod3 <- coef(model_3)[4]
lag4_mod3 <- coef(model_3)[5]
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod3) + dem_shortrun
effect3 <- (effect2 * lag1_mod3) + (effect1 * lag2_mod3) + dem_shortrun
effect4 <- (effect3 * lag1_mod3) + (effect2 * lag2_mod3) + (effect1 * lag3_mod3) + dem_shortrun

effects_mod3 <- c(effect1, effect2, effect3, effect4)
for (i in 5:30) {
  eff <- (effects_mod3[i-1] * lag1_mod3) + (effects_mod3[i-2] * lag2_mod3) + 
    (effects_mod3[i-3] * lag3_mod3) + (effects_mod3[i-4] * lag4_mod3) + dem_shortrun
  effects_mod3 <- c(effects_mod3, eff)
}
eff_25_3 <- effects_mod3[25]

# モデル4 の場合（8期分：lag1～lag8 を考慮）
dem_shortrun <- coef(model_4)["dem"]
lag1_mod4 <- coef(model_4)[2]
lag2_mod4 <- coef(model_4)[3]
lag3_mod4 <- coef(model_4)[4]
lag4_mod4 <- coef(model_4)[5]
lag5_mod4 <- coef(model_4)[6]
lag6_mod4 <- coef(model_4)[7]
lag7_mod4 <- coef(model_4)[8]
lag8_mod4 <- coef(model_4)[9]
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod4) + dem_shortrun
effect3 <- (effect2 * lag1_mod4) + (effect1 * lag2_mod4) + dem_shortrun
effect4 <- (effect3 * lag1_mod4) + (effect2 * lag2_mod4) + (effect1 * lag3_mod4) + dem_shortrun
effect5 <- (effect4 * lag1_mod4) + (effect3 * lag2_mod4) + (effect2 * lag3_mod4) + (effect1 * lag4_mod4) + dem_shortrun
effect6 <- (effect5 * lag1_mod4) + (effect4 * lag2_mod4) + (effect3 * lag3_mod4) + (effect2 * lag4_mod4) + (effect1 * lag5_mod4) + dem_shortrun
effect7 <- (effect6 * lag1_mod4) + (effect5 * lag2_mod4) + (effect4 * lag3_mod4) + (effect3 * lag4_mod4) + (effect2 * lag5_mod4) + (effect1 * lag6_mod4) + dem_shortrun
effect8 <- (effect7 * lag1_mod4) + (effect6 * lag2_mod4) + (effect5 * lag3_mod4) + (effect4 * lag4_mod4) + (effect3 * lag5_mod4) + (effect2 * lag6_mod4) + (effect1 * lag7_mod4) + dem_shortrun

effects_mod4 <- c(effect1, effect2, effect3, effect4, effect5, effect6, effect7, effect8)
for (i in 9:30) {
  eff <- (effects_mod4[i-1] * lag1_mod4) + (effects_mod4[i-2] * lag2_mod4) + 
    (effects_mod4[i-3] * lag3_mod4) + (effects_mod4[i-4] * lag4_mod4) + 
    (effects_mod4[i-5] * lag5_mod4) + (effects_mod4[i-6] * lag6_mod4) + 
    (effects_mod4[i-7] * lag7_mod4) + (effects_mod4[i-8] * lag8_mod4) + dem_shortrun
  effects_mod4 <- c(effects_mod4, eff)
}
eff_25_4 <- effects_mod4[25]

# 25年後の累積効果を各モデルごとにまとめる
eff_25 <- c(eff_25_1, eff_25_2, eff_25_3, eff_25_4)
eff_25 <- round(eff_25, 3)
print(eff_25)

# -----------------------------------------------------------
# 11. 結果の出力：stargazer によるテーブル作成
# -----------------------------------------------------------
# モデルオブジェクトは model_1 〜 model_4 としてリストにまとめる
models <- list(model_1, model_2, model_3, model_4)

stargazer(models, 
          type = "text",                      # "text" に変更すればコンソール上で確認可能
          title = "Effect of Democracy on (Log) GDP per Capita", 
          dep.var.labels = "Log GDP per Capita",  
          covariate.labels = c("Democracy"),
          omit.stat = c("ser", "f", "adj.rsq"), 
          keep.stat = c("n", "N"),            # 観測数を表示
          omit = c("lag6", "lag7", "lag8"),   # 出力から除外する変数（必要に応じて調整）
          model.numbers = FALSE,              # モデル番号を除去
          column.labels = c("(1)", "(2)", "(3)", "(4)"), 
          add.lines = list( 
            c("Persistence: ", pers), 
            c("Long run effect: ", lre),
            c("Effect after 25 years: ", eff_25)
          ),
          notes = "The reported coefficient on democracy is multiplied by 100. Replication of Table 2 in ANRR (2019)",
          notes.append = FALSE
)
