# 0. パッケージの読み込み
pacman::p_load(
  tidyverse,
  haven,
  knitr,
  kableExtra
)

# 1. データ読み込み
data <- read_dta("data/raw/DDCGdata_final.dta")

# 2. 「変数名」と「表に表示したいラベル」の対応表を定義
var_info <- tibble(
  var = c("gdppercapitaconstant2000us",  # 実データ上の列名
          "ginv",
          "tradewb",
          "prienr",
          "secenr",
          "taxratio",
          "mortnew",
          "unrestn",
          "marketref"),
  label = c("GDP per capita",                       # 表示用ラベル
            "Investment share of GDP",
            "Trade share of GDP",
            "Primary-school enrollment rate",
            "Secondary-school enrollment rate",
            "Tax revenue share of GDP",
            "Child mortality per 1,000 births",
            "Unrest rate",
            "Market reforms index (0–100)")
)

# 3. 集計（記述統計）を行う関数
calc_summary <- function(df, var, group_var) {
  df %>%
    filter(!is.na(.data[[var]])) %>%         # NAを除外
    group_by({{ group_var }}) %>%
    summarise(
      Observations = n(),                   # サンプル数
      Mean         = mean(.data[[var]], na.rm = TRUE),
      SD           = sd(.data[[var]], na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    mutate(Variable = var)                  # どの変数を集計したか記録
}

# 4. var_info の var 列を集計したい変数リストとして利用
var_list <- var_info$var

# 5. 上の関数で集計 → pivot_wider で「dem=0/1」を列に広げ → ラベル付け
summary_table <- lapply(var_list, function(x) calc_summary(data, x, dem)) %>%
  bind_rows() %>%
  pivot_wider(
    names_from  = dem, 
    values_from = c(Observations, Mean, SD),
    names_glue  = "{.value}_dem{dem}"
  ) %>%
  # 集計後の列をわかりやすい名前に付け替え
  rename(
    Observations_Nondem    = Observations_dem0,
    Mean_Nondem            = Mean_dem0,
    SD_Nondem              = SD_dem0,
    Observations_Dem       = Observations_dem1,
    Mean_Dem               = Mean_dem1,
    SD_Dem                 = SD_dem1
  ) %>%
  # var_info と結合して「変数のラベル」を付ける
  left_join(var_info, by = c("Variable" = "var")) %>%
  # 表示順に列を並べ替え、ラベルを「Variable」列として使う
  select(label,
         Observations_Nondem, Mean_Nondem, SD_Nondem,
         Observations_Dem,    Mean_Dem,    SD_Dem
  ) %>%
  rename(Variable = label)

# 6. kableExtra で PDF 向けの表に整形
#    - 多段ヘッダを使い、Nondemocracies / Democracies で3列ずつまとめています
colnames(summary_table) <- c("Variable",
                             "Observations", "Mean", "SD",
                             "Observations", "Mean", "SD")

latex_table <- summary_table %>%
  kbl(
    caption = "Summary Statistics by Democracy Status",
    format = "latex",
    booktabs = TRUE,
    digits = 2
  ) %>%
  add_header_above(c(" " = 1, "Nondemocracies" = 3, "Democracies" = 3)) %>%
  kable_styling(latex_options = c("HOLD_position", "striped"))

# 7. テーブルを tex 形式のファイルに出力する
save_kable(latex_table, file = "output/table_1.tex")
