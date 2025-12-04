# scripts/3-4_analysis.R
#
# 更新 v4: 修正 mapping_table_final 中的錯字，以確保資料能正確合併。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)

cat("--- 步驟 1：套件載入成功 ---

")

# 2. 載入原始資料
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---

")


# 3. 準備年度資料
# ---------------------------
industry_salary_annual <- salary_data %>%
  filter(行業別 != "工業及服務業總計", between(年度, 109, 113)) %>%
  select(行業別, 年度, 總薪資)

registration_annual <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All", between(年度, 109, 113)) %>%
  select(群類名稱, 年度, 報名人數)

cat("--- 步驟 3：已準備好歷年薪資與報名人數資料 ---

")


# 4. 建立對應表並合併資料
# ------------------------------------
# 【關鍵修正】修正行業別名稱的錯字
mapping_table_final <- tribble(
  ~行業別, ~群類名稱,
  "住宿及餐飲業", "餐旅群",
  "出版影音及資通訊業", "電機與電子群資電類", # 修正錯字
  "金融及保險業", "商業與管理群",
  "製造業", "機械群",
  "製造業", "動力機械群",
  "營建工程業", "土木與建築群",
  "醫療保健及社會工作服務業", "衛生與護理類",
  "藝術娛樂及休閒服務業", "藝術群影視類", # 修正錯字
  "製造業", "化工群"
)

# 進行合併
merged_annual_data <- left_join(registration_annual, mapping_table_final, by = "群類名稱") %>%
  inner_join(industry_salary_annual, by = c("行業別", "年度"))

cat("--- 步驟 4：已合併年度資料 ---

")


# 5. 計算年增率
# --------------------------------
analysis_data_for_regression <- merged_annual_data %>%
  group_by(行業別, 群類名稱) %>%
  arrange(年度, .by_group = TRUE) %>%
  mutate(
    薪資年增率 = (總薪資 - lag(總薪資)) / lag(總薪資),
    報名人數年增率 = (報名人數 - lag(報名人數)) / lag(報名人數)
  ) %>%
  ungroup() %>%
  filter(!is.na(薪資年增率) & !is.na(報名人數年增率))

cat("--- 步驟 5：已計算年增率，準備進行迴歸分析 ---
")
print(analysis_data_for_regression, n = Inf)
cat("\n")


# 6. 執行「增長率模型」的迴歸分析
# ------------------------------------
cat("--- 步驟 6：執行迴歸分析 lm(報名人數年增率 ~ 薪資年增率) ---
")

# 建立模型
growth_rate_model <- lm(報名人數年增率 ~ 薪資年增率, data = analysis_data_for_regression)

# 輸出模型摘要
cat("增長率迴歸模型結果摘要：\n\n")
print(summary(growth_rate_model))

cat("\n--- 腳本執行完畢 ---
")
