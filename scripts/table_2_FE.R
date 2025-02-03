pacman::p_load(
  plm,         # パネル回帰
  tidyverse,   # データ操作（dplyr, tidyr など）
  haven,       # Stataデータの読み込み
  texreg       # 結果出力（LaTeXテーブルの生成）
)

data <- read_dta("data/raw/DDCGdata_final.dta") 

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

data_m1 <- data_t2 %>%
  drop_na(y, dem, lag1)
data_m1 <- pdata.frame(data_m1,
                       index = c("country_name", "year"))
model_1 <- plm(
  y ~ dem + lag1,
  data   = data_m1,
  model  = "within",
  effect = "twoways"
)

data_m2 <- data_t2 %>%
  drop_na(y, dem, lag1, lag2)
data_m2 <- pdata.frame(data_m2,
                       index = c("country_name", "year"))
model_2 <- plm(
  y ~ dem + lag1 + lag2,
  data   = data_m2,
  model  = "within",
  effect = "twoways"
)

data_m3 <- data_t2 %>%
  drop_na(y, dem, lag1, lag2, lag3, lag4)
data_m3 <- pdata.frame(data_m3,
                       index = c("country_name", "year"))
model_3 <- plm(
  y ~ dem + lag1 + lag2 + lag3 + lag4,
  data   = data_m3,
  model  = "within",
  effect = "twoways"
)

data_m4 <- data_t2 %>%
  drop_na(y, dem, lag1, lag2, lag3, lag4,
          lag5, lag6, lag7, lag8)
data_m4 <- pdata.frame(data_m4,
                       index = c("country_name", "year"))
model_4 <- plm(
  y ~ dem + lag1 + lag2 + lag3 + lag4 +
    lag5 + lag6 + lag7 + lag8,
  data   = data_m4,
  model  = "within",
  effect = "twoways"
)

beta_hat_1  <- coef(model_1)["dem"]
gamma_hat_1 <- coef(model_1)[c("lag1")]
long_run_effect_1 <- beta_hat_1 / (1 - sum(gamma_hat_1))

beta_hat_2  <- coef(model_2)["dem"]
gamma_hat_2 <- coef(model_2)[c("lag1", "lag2")]
long_run_effect_2 <- beta_hat_2 / (1 - sum(gamma_hat_2))

beta_hat_3  <- coef(model_3)["dem"]
gamma_hat_3 <- coef(model_3)[c("lag1", "lag2", "lag3", "lag4")]
long_run_effect_3 <- beta_hat_3 / (1 - sum(gamma_hat_3))

beta_hat_4  <- coef(model_4)["dem"]
gamma_hat_4 <- coef(model_4)[c("lag1", "lag2", "lag3",
                               "lag4", "lag5", "lag6",
                               "lag7", "lag8")]
long_run_effect_4 <- beta_hat_4 / (1 - sum(gamma_hat_4))

lre <- round(
  c(
    long_run_effect_1,
    long_run_effect_2,
    long_run_effect_3,
    long_run_effect_4
  ),
  3
)
print(lre)

pers1 <- sum(coef(model_1)[2])
pers2 <- sum(coef(model_2)[2:3])
pers3 <- sum(coef(model_3)[2:5])
pers4 <- sum(coef(model_4)[2:9])
pers <- round(
  c(pers1, pers2, pers3, pers4),
  3
)
print(pers)

dem_shortrun <- coef(model_1)["dem"]
lag1_mod1 <- coef(model_1)[2]
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod1) + dem_shortrun
effects_mod1 <- c(effect1, effect2)

for (i in 3:30) {
  eff <- (effects_mod1[i-1] * lag1_mod1) + dem_shortrun
  effects_mod1 <- c(effects_mod1, eff)
}
eff_25_1 <- effects_mod1[25]

dem_shortrun <- coef(model_2)["dem"]
lag1_mod2 <- coef(model_2)[2]
lag2_mod2 <- coef(model_2)[3]
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod2) + dem_shortrun
effect3 <- (effect2 * lag1_mod2) +
  (effect1 * lag2_mod2) +
  dem_shortrun
effects_mod2 <- c(effect1, effect2, effect3)

for (i in 4:30) {
  eff <- (effects_mod2[i-1] * lag1_mod2) +
    (effects_mod2[i-2] * lag2_mod2) +
    dem_shortrun
  effects_mod2 <- c(effects_mod2, eff)
}
eff_25_2 <- effects_mod2[25]

dem_shortrun <- coef(model_3)["dem"]
lag1_mod3 <- coef(model_3)[2]
lag2_mod3 <- coef(model_3)[3]
lag3_mod3 <- coef(model_3)[4]
lag4_mod3 <- coef(model_3)[5]
effect1 <- dem_shortrun
effect2 <- (effect1 * lag1_mod3) + dem_shortrun
effect3 <- (effect2 * lag1_mod3) +
  (effect1 * lag2_mod3) +
  dem_shortrun
effect4 <- (effect3 * lag1_mod3) +
  (effect2 * lag2_mod3) +
  (effect1 * lag3_mod3) +
  dem_shortrun
effects_mod3 <- c(effect1, effect2, effect3, effect4)

for (i in 5:30) {
  eff <- (effects_mod3[i-1] * lag1_mod3) +
    (effects_mod3[i-2] * lag2_mod3) +
    (effects_mod3[i-3] * lag3_mod3) +
    (effects_mod3[i-4] * lag4_mod3) +
    dem_shortrun
  effects_mod3 <- c(effects_mod3, eff)
}
eff_25_3 <- effects_mod3[25]

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
effect3 <- (effect2 * lag1_mod4) +
  (effect1 * lag2_mod4) +
  dem_shortrun
effect4 <- (effect3 * lag1_mod4) +
  (effect2 * lag2_mod4) +
  (effect1 * lag3_mod4) +
  dem_shortrun
effect5 <- (effect4 * lag1_mod4) +
  (effect3 * lag2_mod4) +
  (effect2 * lag3_mod4) +
  (effect1 * lag4_mod4) +
  dem_shortrun
effect6 <- (effect5 * lag1_mod4) +
  (effect4 * lag2_mod4) +
  (effect3 * lag3_mod4) +
  (effect2 * lag4_mod4) +
  (effect1 * lag5_mod4) +
  dem_shortrun
effect7 <- (effect6 * lag1_mod4) +
  (effect5 * lag2_mod4) +
  (effect4 * lag3_mod4) +
  (effect3 * lag4_mod4) +
  (effect2 * lag5_mod4) +
  (effect1 * lag6_mod4) +
  dem_shortrun
effect8 <- (effect7 * lag1_mod4) +
  (effect6 * lag2_mod4) +
  (effect5 * lag3_mod4) +
  (effect4 * lag4_mod4) +
  (effect3 * lag5_mod4) +
  (effect2 * lag6_mod4) +
  (effect1 * lag7_mod4) +
  dem_shortrun
effects_mod4 <- c(effect1, effect2, effect3, effect4,
                  effect5, effect6, effect7, effect8)

for (i in 9:30) {
  eff <- (effects_mod4[i-1] * lag1_mod4) +
    (effects_mod4[i-2] * lag2_mod4) +
    (effects_mod4[i-3] * lag3_mod4) +
    (effects_mod4[i-4] * lag4_mod4) +
    (effects_mod4[i-5] * lag5_mod4) +
    (effects_mod4[i-6] * lag6_mod4) +
    (effects_mod4[i-7] * lag7_mod4) +
    (effects_mod4[i-8] * lag8_mod4) +
    dem_shortrun
  effects_mod4 <- c(effects_mod4, eff)
}
eff_25_4 <- effects_mod4[25]

eff_25 <- round(
  c(eff_25_1, eff_25_2, eff_25_3, eff_25_4),
  3
)
print(eff_25)

se1 <- sqrt(diag(vcov(model_1)))
se2 <- sqrt(diag(vcov(model_2)))
se3 <- sqrt(diag(vcov(model_3)))
se4 <- sqrt(diag(vcov(model_4)))

override.coef.1 <- c(
  coef(model_1)["dem"],
  coef(model_1)["lag1"],
  NA, NA, NA, NA, NA, NA, NA
)
override.se.1 <- c(
  se1["dem"],
  se1["lag1"],
  NA, NA, NA, NA, NA, NA, NA
)

override.coef.2 <- c(
  coef(model_2)["dem"],
  coef(model_2)["lag1"],
  coef(model_2)["lag2"],
  NA, NA, NA, NA, NA, NA
)
override.se.2 <- c(
  se2["dem"],
  se2["lag1"],
  se2["lag2"],
  NA, NA, NA, NA, NA, NA
)

override.coef.3 <- c(
  coef(model_3)["dem"],
  coef(model_3)["lag1"],
  coef(model_3)["lag2"],
  coef(model_3)["lag3"],
  coef(model_3)["lag4"],
  NA, NA, NA, NA
)
override.se.3 <- c(
  se3["dem"],
  se3["lag1"],
  se3["lag2"],
  se3["lag3"],
  se3["lag4"],
  NA, NA, NA, NA
)

override.coef.4 <- c(
  coef(model_4)["dem"],
  coef(model_4)["lag1"],
  coef(model_4)["lag2"],
  coef(model_4)["lag3"],
  coef(model_4)["lag4"],
  coef(model_4)["lag5"],
  coef(model_4)["lag6"],
  coef(model_4)["lag7"],
  coef(model_4)["lag8"]
)
override.se.4 <- c(
  se4["dem"],
  se4["lag1"],
  se4["lag2"],
  se4["lag3"],
  se4["lag4"],
  se4["lag5"],
  se4["lag6"],
  se4["lag7"],
  se4["lag8"]
)

models <- list(model_1, model_2, model_3, model_4)

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
  custom.model.names = c(
    "(1)", "(2)", "(3)", "(4)"
  ),
  custom.coef.names = c(
    "Democracy", "Lag 1", "Lag 2",
    "Lag 3", "Lag 4", "Lag 5",
    "Lag 6", "Lag 7", "Lag 8"
  ),
  custom.gof.rows = list(
    "Persistence"          = pers,
    "Long run effect"      = lre,
    "Effect after 25 years" = eff_25
  ),
  file = "output/table_2_FE.tex",
  caption = "Effect of Democracy on (Log) GDP per Capita"
)