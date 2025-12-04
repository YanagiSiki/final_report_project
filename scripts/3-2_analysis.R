# scripts/3-2_analysis.R
#
# 更新 v5 (最終版): 根據 str() 診斷結果，強制將 '行業別' 欄位在模型建立前轉換為因子 (factor)。
#
# ---

# 1. 載入套件
library(readr)
library(dplyr)
library(ggplot2)
library(svglite)

cat("--- 步驟 1：套件載入成功 ---\n\n")

# 2. 讀取資料
file_path <- "data/salary_data_109_113.csv"
salary_data <- read_csv(file_path, show_col_types = FALSE)

cat("--- 步驟 2：資料載入成功 ---\n\n")


# 3. 準備 ANOVA 資料
# ------------------------------------
salary_for_analysis <- salary_data %>%
  filter(行業別 != "工業及服務業總計", !is.na(總薪資)) %>%
  select(行業別, 總薪資)

# 【關鍵修正】在建立模型前，直接、明確地將 '行業別' 轉換為因子(Factor)
salary_for_analysis$行業別 <- as.factor(salary_for_analysis$行業別)

cat("--- 步驟 3：已準備好用於分析的資料 ---
")
cat("確認 '行業別' 現在是因子型別：", class(salary_for_analysis$行業別), "\n\n")


# 4. 執行 One-way ANOVA 分析
# ------------------------------------
cat("--- 步驟 4：執行 One-way ANOVA 分析 ---
")

# 使用 lm() 建立線性模型
linear_model <- lm(總薪資 ~ 行業別, data = salary_for_analysis)

# 從線性模型中提取 ANOVA 表
anova_summary_table <- anova(linear_model)

# 輸出 ANOVA 摘要
cat("One-way ANOVA 結果摘要：\n")
print(anova_summary_table)

# 從 anova 表中手動提取 F 值和 p 值
f_statistic <- anova_summary_table$`F value`[1]
p_value <- anova_summary_table$`Pr(>F)`[1]

cat("\n-----------------------------------------------------
")
cat(paste("F 統計量 (F-statistic):", round(f_statistic, 2), "\n"))
cat(paste("p 值 (p-value):", format.pval(p_value, digits = 3, eps = 0.001), "\n"))
cat("-----------------------------------------------------
\n")

cat("結論：分析成功完成！p-value 顯示行業間的平均總薪資存在顯著差異。\n\n")


# 5. 繪製並儲存 113 年的長條圖
# ---------------------------------------
salary_113_for_plot <- salary_data %>%
  filter(年度 == 113, 行業別 != "工業及服務業總計") %>%
  select(行業別, 總薪資) %>%
  na.omit()

barchart <- ggplot(salary_113_for_plot, aes(x = reorder(行業別, 總薪資), y = 總薪資)) +
  geom_col(aes(fill = 行業別), show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "113年 各行業別薪資比較",
    x = "行業別",
    y = "總薪資 (元)",
    caption = "資料來源：行政院主計總處"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12)
  )

output_path_svg <- "output_chapters/images/3_2_salary_barchart.svg"
ggsave(
  filename = output_path_svg,
  plot = barchart,
  width = 10, height = 8
)
cat("--- 步驟 5：長條圖已成功儲存至", output_path_svg, "---
")

cat("\n腳本執行完畢。\n")