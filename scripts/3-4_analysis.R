# scripts/3-4_analysis.R
#
# 更新 v2: 將 summary() 包在 print() 中，以確保結果能在 source() 執行時被印出。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)

cat("--- 步驟 1：套件載入成功 ---\n\n")

# 2. 載入所有原始資料
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)
birth_cohort_data <- read_csv("data/tcte_birth_cohort_statistics_109_113.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---\n\n")

# 3. 準備與合併資料
cat("--- 步驟 3：建立迴歸分析專用資料集 ---
")

industry_salary_annual <- salary_data %>%
  filter(行業別 != "工業及服務業總計", between(年度, 109, 113)) %>%
  group_by(行業別, 年度) %>%
  summarise(行業平均總薪資 = mean(總薪資, na.rm = TRUE), .groups = "drop")

registration_annual <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All", between(年度, 109, 113)) %>%
  select(群類名稱, 年度, 報名人數)

birth_annual <- birth_cohort_data %>%
  filter(between(統測學年度, 109, 113)) %>%
  select(年度 = 統測學年度, 該年出生人數)

mapping_table_full <- tribble(
  ~行業別, ~群類名稱,
  "住宿及餐飲業", "餐旅群",
  "出版、影音製作、程式設計及資通訊服務業", "電機與電子群資電類",
  "電子零組件製造業", "電機與電子群資電類",
  "金融及保險業", "商業與管理群",
  "批發及零售業", "商業與管理群",
  "製造業", "機械群",
  "製造業", "動力機械群",
  "營建工程業", "土木與建築群",
  "醫療保健及社會工作服務業", "衛生與護理類",
  "藝術、娛樂及休閒服務業", "藝術群影視類",
  "化學材料製造業", "化工群"
)

# 使用 relationship = "many-to-many" 來抑制 dplyr 的警告
regression_data <- inner_join(registration_annual, mapping_table_full, by = "群類名稱", relationship = "many-to-many") %>%
  inner_join(industry_salary_annual, by = c("行業別", "年度")) %>%
  inner_join(birth_annual, by = "年度")

cat("已建立迴歸分析資料集，共", nrow(regression_data), "筆觀測值.\n")
cat("資料集預覽：\n")
print(head(regression_data))
cat("\n")


# 4. 執行多元迴歸分析
cat("--- 步驟 4：執行多元迴歸分析 ---
")

regression_data_scaled <- regression_data %>%
  mutate(
    出生人數_scaled = scale(該年出生人數),
    薪資_scaled = scale(行業平均總薪資)
  )

regression_model <- lm(報名人數 ~ 出生人數_scaled + 薪資_scaled, data = regression_data_scaled)

# 【關鍵修正】使用 print() 明確地將 summary 結果印出
cat("多元迴歸模型結果摘要：\n\n")
print(summary(regression_model))

cat("\n--- 腳本執行完畢 ---
")