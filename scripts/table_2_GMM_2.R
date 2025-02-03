#############################################
## 1. パッケージの読み込みとデータ前処理 ##
#############################################

# 必要なパッケージの読み込み
# plm: パネル回帰、pgmm() による GMM 推定を実施するためのパッケージ
# tidyverse: データの操作や整形に利用するパッケージ群
# haven: Stata ファイル (.dta) を読み込むためのパッケージ
# texreg: 推定結果を LaTeX 形式のテーブルに出力するためのパッケージ
pacman::p_load(
  plm,
  tidyverse,
  haven,
  texreg
)

# --- データの読み込み ---
# 指定のパスから Stata ファイルを読み込みます（ファイルパスは環境に合わせて変更してください）
data <- read_dta("data/raw/DDCGdata_final.dta")

# --- ラグ変数の作成 ---
# 各国（country_name）ごとにソートし、変数 y の 1～8 期のラグ変数を作成します
# ※ dplyr::lag() を用いて、時系列順にずらした変数を生成します
data_t2 <- data %>%
  select(1:30) %>%            # 必要な変数のみ選択（適宜変更してください）
  group_by(country_name) %>%  # 各国ごとにグループ化
  arrange(year) %>%           # 年度順に並べ替え
  mutate(
    lag1 = dplyr::lag(y, 1),  # 1期前の y
    lag2 = dplyr::lag(y, 2),  # 2期前の y
    lag3 = dplyr::lag(y, 3),  # 3期前の y
    lag4 = dplyr::lag(y, 4),  # 4期前の y
    lag5 = dplyr::lag(y, 5),  # 5期前の y
    lag6 = dplyr::lag(y, 6),  # 6期前の y
    lag7 = dplyr::lag(y, 7),  # 7期前の y
    lag8 = dplyr::lag(y, 8)   # 8期前の y
  ) %>%
  ungroup()  # グループ化解除

# --- パネルデータとしての整形 ---
# モデルごとに、必要なラグ変数が欠損していないサンプルを抽出し、pdata.frame に変換します
# pdata.frame は plm パッケージでパネルデータとして扱うための形式です

# モデル1：lag1 のみを使用する場合（欠損のない観測値を抽出）
data_m1 <- data_t2 %>% drop_na(y, dem, lag1)
data_m1 <- pdata.frame(data_m1, index = c("country_name", "year"))

# モデル2：lag1 と lag2 を使用
data_m2 <- data_t2 %>% drop_na(y, dem, lag1, lag2)
data_m2 <- pdata.frame(data_m2, index = c("country_name", "year"))

# モデル3：lag1～lag4 を使用
data_m3 <- data_t2 %>% drop_na(y, dem, lag1, lag2, lag3, lag4)
data_m3 <- pdata.frame(data_m3, index = c("country_name", "year"))

# モデル4：lag1～lag8 を使用
data_m4 <- data_t2 %>% drop_na(y, dem, lag1, lag2, lag3, lag4, lag5, lag6, lag7, lag8)
data_m4 <- pdata.frame(data_m4, index = c("country_name", "year"))

#############################################
## 2. GMM推定 (Arellano‐Bond) の実行     ##
#############################################

# STATAコードでは、被説明変数 y の楽器変数として laglimits(2 .)（すなわち 2 期目以降全て）、
# 変数 dem の楽器変数として laglimits(1 .)（すなわち 1 期目以降全て）を指定しています。
# ここでは、パネル期間を 1960～2009 と仮定して、最大ラグ（楽器として使用するラグの上限）を 49 に設定しています。
maxlag <- 49

# pgmm() 関数を用いて、差分変換後のモデルに対して GMM 推定を実施します。
# effect = "twoways": 個体固定効果と時点固定効果を考慮
# model = "twosteps": 2段階推定を実施
# transformation = "d": 一階差分変換を行う（Arellano‐Bond 推定の標準的手法）
#
# 以下、各モデルについて説明変数と楽器変数の指定を行います。
# ※楽器指定部分では、y については lag(y, 2:maxlag) を、dem については lag(dem, 1:maxlag) を指定しています。

# モデル1：説明変数は dem と lag(y, 1)
model_1_gmm <- pgmm(
  formula = y ~ dem + lag(y, 1) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m1,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# モデル2：説明変数は dem と lag(y, 1) および lag(y, 2)
model_2_gmm <- pgmm(
  formula = y ~ dem + lag(y, 1) + lag(y, 2) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m2,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# モデル3：説明変数は dem と lag(y, 1～4)
model_3_gmm <- pgmm(
  formula = y ~ dem + lag(y, 1) + lag(y, 2) + lag(y, 3) + lag(y, 4) | lag(y, 2:maxlag) + lag(dem, 1:maxlag),
  data = data_m3,
  effect = "twoways",
  model = "twosteps",
  transformation = "d"
)

# モデル4：説明変数は dem と lag(y, 1～8)
model_4_gmm <- pgmm(
  formula = y ~ dem + lag(y, 1) + lag(y, 2) + lag(y, 3) + lag(y, 4) +
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
# 例として、25 期後の効果を求める場合には、25 を n_periods に指定します。
compute_dynamic_effect <- function(dem_coef, lag_coefs, n_periods) {
  # n_periods: 計算する期間数（例：25 期後）
  effects <- numeric(n_periods)   # 各期の効果を格納する数値型ベクトルを初期化
  effects[1] <- dem_coef            # 初期（短期効果）は dem の係数
  k <- length(lag_coefs)            # 使用するラグの数を取得
  if(n_periods > 1) {
    # 2期目以降の効果を再帰的に計算するループ
    for(i in 2:n_periods) {
      eff <- dem_coef             # 各期の効果はまず dem の係数から開始
      for(j in 1:min(i - 1, k)) {
        # 前期から j 期前までの効果にラグ係数を乗じて加算
        eff <- eff + effects[i - j] * lag_coefs[j]
      }
      effects[i] <- eff           # 計算結果を格納
    }
  }
  return(effects[n_periods])      # n 期後の効果を返す
}

####################################################
## 4. 各モデルの動学的効果・長期効果・持続性の計算 ##
####################################################
# 以下では、各モデルに対して、
# ・短期効果：dem の係数
# ・長期効果：dem の係数 / (1 - ラグ係数の和)
# ・持続性：各ラグ係数の和
# ・25 期後の効果：再帰的計算関数 compute_dynamic_effect() により算出
# を計算します。

# モデル1（1ラグ）の計算
coef_1 <- coef(model_1_gmm)
dem_coef_1 <- coef_1["dem"]                # 短期効果（dem の係数）
lag1_1     <- coef_1["lag(y, 1)"]            # 1期前の y の係数
lre1       <- dem_coef_1 / (1 - lag1_1)       # 長期効果の計算：β_dem / (1 - α1)
pers1      <- lag1_1                        # 持続性は 1 期前の係数（ここでは 1 ラグのみ）
eff_25_1   <- compute_dynamic_effect(dem_coef_1, c(lag1_1), 25)  # 25期後の効果

# モデル2（2ラグ）の計算
coef_2 <- coef(model_2_gmm)
dem_coef_2 <- coef_2["dem"]
lag1_2     <- coef_2["lag(y, 1)"]
lag2_2     <- coef_2["lag(y, 2)"]
lre2       <- dem_coef_2 / (1 - (lag1_2 + lag2_2))  # 長期効果：β_dem / (1 - (α1 + α2))
pers2      <- lag1_2 + lag2_2                      # 持続性：α1 + α2
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

# 結果を丸める（小数点第3位まで）
lre  <- round(c(lre1, lre2, lre3, lre4), 3)      # 長期効果
pers <- round(c(pers1, pers2, pers3, pers4), 3)   # 持続性
eff_25 <- round(c(eff_25_1, eff_25_2, eff_25_3, eff_25_4), 3)  # 25期後の効果

####################################################
## 5. texregによる結果出力                     ##
####################################################
# 以下では、各モデルの推定結果から係数と標準誤差を抽出し、
# モデル間でラグの数が異なるため、使用していない項目は NA で補完して、
# 全モデルとも長さ 9（dem と lag1～lag8）になるよう整形しています。
# その後、texreg() を用いて LaTeX 形式のテーブルを作成します。

# 各モデルの標準誤差を vcov() から計算
se1 <- sqrt(diag(vcov(model_1_gmm)))
se2 <- sqrt(diag(vcov(model_2_gmm)))
se3 <- sqrt(diag(vcov(model_3_gmm)))
se4 <- sqrt(diag(vcov(model_4_gmm)))

# モデル1（1ラグ）の係数・標準誤差：NA を用いて全体で 9 項目にパディング
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

# モデルリストの作成（texreg() に渡すためのリスト）
models <- list(model_1_gmm, model_2_gmm, model_3_gmm, model_4_gmm)

# texreg() を用いて LaTeX テーブルのファイルを作成
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
    "Persistence" = pers,           # 各モデルの持続性（ラグ係数の和）
    "Long run effect" = lre,          # 各モデルの長期効果
    "Effect after 25 years" = eff_25  # 25期後の効果
  ),
  file = "output/TableMain_GMM.tex",
  caption = "Effect of Democracy on (Log) GDP per Capita: Arellano–Bond GMM Estimation"
)
