# scripts/3-2_analysis.R
#
# 目標：執行 3.2 節所需的單因子變異數分析 (One-way ANOVA)。
# (此腳本不再產生圖表，圖表功能已移至 2_descriptive_analysis.R)
#
# -----------------------------------------------------------------------------

# 1. 載入套件
library(readr)
library(dplyr)

cat("--- 步驟 1：套件載入成功 (3-2_analysis.R) ---\n\n")

# 2. 讀取資料
file_path <- "data/salary_data_109_113.csv"
salary_data <- read_csv(file_path, show_col_types = FALSE)

cat("--- 步驟 2：資料載入成功 ---\n\n")


# 3. 準備 ANOVA 資料
# ------------------------------------
salary_for_analysis <- salary_data %>%
  filter(行業別 != "工業及服務業總計", !is.na(總薪資)) %>%
  select(行業別, 總薪資) %>%
  mutate(行業別 = as.factor(行業別))

cat("--- 步驟 3：已準備好用於 ANOVA 分析的資料 ---\n\n")


# 4. 執行 One-way ANOVA 分析
# ------------------------------------
cat("--- 步驟 4：執行 One-way ANOVA 分析 ---\n")

# 建立線性模型並提取 ANOVA 結果
linear_model <- lm(總薪資 ~ 行業別, data = salary_for_analysis)
anova_summary_table <- anova(linear_model)

cat("One-way ANOVA 結果摘要：\n")
print(anova_summary_table)

f_statistic <- anova_summary_table$`F value`[1]
p_value <- anova_summary_table$`Pr(>F)`[1]

cat("\n-----------------------------------------------------\n")
cat(paste("F 統計量 (F-statistic):", round(f_statistic, 2), "\n"))
cat(paste("p 值 (p-value):", format.pval(p_value, digits = 3, eps = 0.001), "\n"))
cat("-----------------------------------------------------\n\n")

cat("\n腳本 (3-2_analysis.R) 執行完畢。\n")
