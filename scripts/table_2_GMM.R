#############################################
## 1. パッケージの読み込みとデータ前処理 ##
#############################################

# 必要なパッケージの読み込み
pacman::p_load(
  plm,         # パネル回帰（固定効果、GMM推定など）
  tidyverse,   # データ操作（dplyr, tidyr など）
  haven,       # Stata形式データの読み込み
  texreg       # 推定結果をLaTeXテーブルに出力
)

# --- データの読み込み ---
# ※ファイルパス等は環境に合わせて変更してください。
data <- read_dta("data/raw/DDCGdata_final.dta")

# --- ラグ変数の作成 ---
# 国（ここでは country_name）ごとにソートし、y の 1～8 期のラグ変数を作成する
data_t2 <- data %>%
  select(1:30) %>%                        # 必要な変数の選択（適宜調整）
  group_by(country_name) %>%              # パネルの個体識別子（例：国名）
  arrange(year) %>%                       # 年度順に並べ替え
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

# --- パネルデータとして整形 ---
# モデルごとに、必要なラグが欠損していないサンプルを抽出し、pdata.frame に変換

# モデル1（ラグ1のみ）
data_m1 <- data_t2 %>% drop_na(y, dem, lag1)
data_m1 <- pdata.frame(data_m1, index = c("country_name", "year"))

# モデル2（ラグ1, ラグ2）
data_m2 <- data_t2 %>% drop_na(y, dem, lag1, lag2)
data_m2 <- pdata.frame(data_m2, index = c("country_name", "year"))

# モデル3（ラグ1～ラグ4）
data_m3 <- data_t2 %>% drop_na(y, dem, lag1, lag2, lag3, lag4)
data_m3 <- pdata.frame(data_m3, index = c("country_name", "year"))

# モデル4（ラグ1～ラグ8）
data_m4 <- data_t2 %>% drop_na(y, dem, lag1, lag2, lag3, lag4, lag5, lag6, lag7, lag8)
data_m4 <- pdata.frame(data_m4, index = c("country_name", "year"))

#############################################
## 2. GMM推定 (Arellano‐Bond) の実行     ##
#############################################

# STATAコードでは、y の楽器として laglimits(2 .)（すなわち2期目以降全て）、
# dem の楽器として laglimits(1 .)（すなわち1期目以降全て）を指定しています。
# ここでは、パネル期間が 1960～2009 と仮定し、最大ラグを 49（2009-1960）としています。
maxlag <- 49

# モデル1：説明変数は dem と lag(y, 1)
model_1_gmm <- pgmm(
  y ~ dem + lag(y, 1) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m1,
  effect = "twoways",      # 個体固定効果と時点固定効果を考慮
  model = "twosteps",       # 2段階推定
  transformation = "d"      # 差分変換（Arellano‐Bond推定の標準設定）
)

# モデル2：説明変数は dem と lag(y, 1) および lag(y, 2)
model_2_gmm <- pgmm(
  y ~ dem + lag(y, 1) + lag(y, 2) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m2,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# モデル3：説明変数は dem と lag(y, 1～4)
model_3_gmm <- pgmm(
  y ~ dem + lag(y, 1) + lag(y, 2) + lag(y, 3) + lag(y, 4) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m3,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# モデル4：説明変数は dem と lag(y, 1～8)
model_4_gmm <- pgmm(
  y ~ dem + lag(y, 1) + lag(y, 2) + lag(y, 3) + lag(y, 4) +
    lag(y, 5) + lag(y, 6) + lag(y, 7) + lag(y, 8) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m4,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

####################################################
## 3. 動学的効果（再帰的乗数）の計算関数の定義      ##
####################################################
# この関数は、短期効果（dem の係数）と各ラグの係数から、
# 再帰的に n 期後の総効果（動学的乗数）を計算します。
compute_dynamic_effect <- function(dem_coef, lag_coefs, n_periods) {
  # n_periods：計算する期間数（例：25年後の効果を求める場合は25）
  effects <- numeric(n_periods)
  effects[1] <- dem_coef  # 初期期は短期効果
  k <- length(lag_coefs)  # ラグの数
  if (n_periods > 1) {
    for (i in 2:n_periods) {
      eff <- dem_coef
      for (j in 1:min(i - 1, k)) {
        eff <- eff + effects[i - j] * lag_coefs[j]
      }
      effects[i] <- eff
    }
  }
  return(effects[n_periods])
}

####################################################
## 4. 各モデルの動学的効果・長期効果・持続性の計算 ##
####################################################
# 短期効果：推定された dem の係数
# 長期効果：dem の係数 / (1 - ラグ係数の和)
# 持続性：ラグ係数の和
# 25年後の効果：再帰的計算関数を用いて算出

# モデル1（1ラグ）の計算
coef_1 <- coef(model_1_gmm)
dem_coef_1 <- coef_1["dem"]
lag1_1     <- coef_1["lag(y, 1)"]
lre1       <- dem_coef_1 / (1 - lag1_1)
pers1      <- lag1_1
eff_25_1   <- compute_dynamic_effect(dem_coef_1, c(lag1_1), 25)

# モデル2（2ラグ）の計算
coef_2 <- coef(model_2_gmm)
dem_coef_2 <- coef_2["dem"]
lag1_2     <- coef_2["lag(y, 1)"]
lag2_2     <- coef_2["lag(y, 2)"]
lre2       <- dem_coef_2 / (1 - (lag1_2 + lag2_2))
pers2      <- lag1_2 + lag2_2
eff_25_2   <- compute_dynamic_effect(dem_coef_2, c(lag1_2, lag2_2), 25)

# モデル3（4ラグ）の計算
coef_3 <- coef(model_3_gmm)
dem_coef_3 <- coef_3["dem"]
lag1_3     <- coef_3["lag(y, 1)"]
lag2_3     <- coef_3["lag(y, 2)"]
lag3_3     <- coef_3["lag(y, 3)"]
lag4_3     <- coef_3["lag(y, 4)"]
lre3       <- dem_coef_3 / (1 - (lag1_3 + lag2_3 + lag3_3 + lag4_3))
pers3      <- lag1_3 + lag2_3 + lag3_3 + lag4_3
eff_25_3   <- compute_dynamic_effect(dem_coef_3, c(lag1_3, lag2_3, lag3_3, lag4_3), 25)

# モデル4（8ラグ）の計算
coef_4 <- coef(model_4_gmm)
dem_coef_4 <- coef_4["dem"]
lag1_4     <- coef_4["lag(y, 1)"]
lag2_4     <- coef_4["lag(y, 2)"]
lag3_4     <- coef_4["lag(y, 3)"]
lag4_4     <- coef_4["lag(y, 4)"]
lag5_4     <- coef_4["lag(y, 5)"]
lag6_4     <- coef_4["lag(y, 6)"]
lag7_4     <- coef_4["lag(y, 7)"]
lag8_4     <- coef_4["lag(y, 8)"]
lre4       <- dem_coef_4 / (1 - (lag1_4 + lag2_4 + lag3_4 + lag4_4 +
                                   lag5_4 + lag6_4 + lag7_4 + lag8_4))
pers4      <- lag1_4 + lag2_4 + lag3_4 + lag4_4 + lag5_4 + lag6_4 + lag7_4 + lag8_4
eff_25_4   <- compute_dynamic_effect(dem_coef_4, c(lag1_4, lag2_4, lag3_4, lag4_4,
                                                   lag5_4, lag6_4, lag7_4, lag8_4), 25)

# 結果の丸め
lre  <- round(c(lre1, lre2, lre3, lre4), 3)      # 長期効果
pers <- round(c(pers1, pers2, pers3, pers4), 3)   # 持続性
eff_25 <- round(c(eff_25_1, eff_25_2, eff_25_3, eff_25_4), 3)  # 25年後の効果

####################################################
## 5. texregによる結果出力                     ##
####################################################
# GMM推定結果の表示用に、各モデルの係数・標準誤差のベクトルを整形します。
# モデル間で含むラグ数が異なるため、使用していない項目は NA で埋め、全モデルとも長さ9（dem および lag1～lag8）とします。

# 各モデルの標準誤差の抽出
se1 <- sqrt(diag(vcov(model_1_gmm)))
se2 <- sqrt(diag(vcov(model_2_gmm)))
se3 <- sqrt(diag(vcov(model_3_gmm)))
se4 <- sqrt(diag(vcov(model_4_gmm)))

# モデル1（1ラグ）の係数・標準誤差：長さ9にパディング
override.coef.1 <- c(
  coef_1["dem"],
  coef_1["lag(y, 1)"],
  rep(NA, 7)
)
override.se.1 <- c(
  se1["dem"],
  se1["lag(y, 1)"],
  rep(NA, 7)
)

# モデル2（2ラグ）の係数・標準誤差
override.coef.2 <- c(
  coef_2["dem"],
  coef_2["lag(y, 1)"],
  coef_2["lag(y, 2)"],
  rep(NA, 6)
)
override.se.2 <- c(
  se2["dem"],
  se2["lag(y, 1)"],
  se2["lag(y, 2)"],
  rep(NA, 6)
)

# モデル3（4ラグ）の係数・標準誤差
override.coef.3 <- c(
  coef_3["dem"],
  coef_3["lag(y, 1)"],
  coef_3["lag(y, 2)"],
  coef_3["lag(y, 3)"],
  coef_3["lag(y, 4)"],
  rep(NA, 4)
)
override.se.3 <- c(
  se3["dem"],
  se3["lag(y, 1)"],
  se3["lag(y, 2)"],
  se3["lag(y, 3)"],
  se3["lag(y, 4)"],
  rep(NA, 4)
)

# モデル4（8ラグ）の係数・標準誤差
override.coef.4 <- c(
  coef_4["dem"],
  coef_4["lag(y, 1)"],
  coef_4["lag(y, 2)"],
  coef_4["lag(y, 3)"],
  coef_4["lag(y, 4)"],
  coef_4["lag(y, 5)"],
  coef_4["lag(y, 6)"],
  coef_4["lag(y, 7)"],
  coef_4["lag(y, 8)"]
)
override.se.4 <- c(
  se4["dem"],
  se4["lag(y, 1)"],
  se4["lag(y, 2)"],
  se4["lag(y, 3)"],
  se4["lag(y, 4)"],
  se4["lag(y, 5)"],
  se4["lag(y, 6)"],
  se4["lag(y, 7)"],
  se4["lag(y, 8)"]
)

# モデルリストの作成
models <- list(model_1_gmm, model_2_gmm, model_3_gmm, model_4_gmm)

# texreg() による LaTeX テーブルの作成
texreg(
  models,
  override.coef = list(override.coef.1, override.coef.2, override.coef.3, override.coef.4),
  override.se = list(override.se.1, override.se.2, override.se.3, override.se.4),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.names = c(
    "Democracy", "Lag 1", "Lag 2", "Lag 3", "Lag 4",
    "Lag 5", "Lag 6", "Lag 7", "Lag 8"
  ),
  custom.gof.rows = list(
    "Persistence" = pers,
    "Long run effect" = lre,
    "Effect after 25 years" = eff_25
  ),
  file = "output/TableMain_GMM.tex",
  caption = "Effect of Democracy on (Log) GDP per Capita: Arellano–Bond GMM Estimation"
)
