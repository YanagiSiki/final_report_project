# scripts/3-2_analysis.R
#
# 更新 v7: 修正 gghighlight 的問題，確保能同時突顯薪資最高與最低的行業。
#
# ---

# 1. 載入套件
library(readr)
library(dplyr)
library(ggplot2)
library(svglite)
library(gghighlight)

cat("--- 步驟 1：套件載入成功 ---

")

# 2. 讀取資料
file_path <- "data/salary_data_109_113.csv"
salary_data <- read_csv(file_path, show_col_types = FALSE)

cat("--- 步驟 2：資料載入成功 ---

")


# 3. 準備 ANOVA 資料
# ------------------------------------
salary_for_analysis <- salary_data %>%
  filter(行業別 != "工業及服務業總計", !is.na(總薪資)) %>%
  select(行業別, 總薪資) %>%
  mutate(行業別 = as.factor(行業別))

cat("--- 步驟 3：已準備好用於 ANOVA 分析的資料 ---

")


# 4. 執行 One-way ANOVA 分析
# ------------------------------------
cat("--- 步驟 4：執行 One-way ANOVA 分析 ---
")

linear_model <- lm(總薪資 ~ 行業別, data = salary_for_analysis)
anova_summary_table <- anova(linear_model)

cat("One-way ANOVA 結果摘要：\n")
print(anova_summary_table)

f_statistic <- anova_summary_table$`F value`[1]
p_value <- anova_summary_table$`Pr(>F)`[1]

cat("\n-----------------------------------------------------
")
cat(paste("F 統計量 (F-statistic):", round(f_statistic, 2), "\n"))
cat(paste("p 值 (p-value):", format.pval(p_value, digits = 3, eps = 0.001), "\n"))
cat("-----------------------------------------------------

")


# 5. 繪製並儲存 109-113 年的趨勢折線圖
# -----------------------------------------------
cat("--- 步驟 5：產生 109-113 年薪資趨勢折線圖 ---
")

# 準備繪圖資料
salary_for_plot <- salary_data %>%
  filter(行業別 != "工業及服務業總計", between(年度, 109, 113))

# 【關鍵修正】明確指定要突顯的行業名稱，確保穩定性
highlighted_industries <- c("金融及保險業", "住宿及餐飲業")

# 繪製折線圖
line_chart <- ggplot(salary_for_plot, aes(x = 年度, y = 總薪資, color = 行業別, group = 行業別)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  gghighlight(行業別 %in% highlighted_industries, label_params = list(nudge_x = 0.5, segment.color = NA), use_direct_label = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 109:113) +
  labs(
    title = "109-113年 各主要行業別平均總薪資趨勢",
    subtitle = "圖中突顯薪資最高與最低的行業別",
    x = "年度",
    y = "總薪資 (元)",
    caption = "資料來源：行政院主計總處"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# 儲存圖表
output_path_svg <- "output_chapters/images/3_2_salary_trends.svg"
ggsave(
  filename = output_path_svg,
  plot = line_chart,
  width = 10, height = 7
)
cat(paste("--- 趨勢折線圖已成功儲存至", output_path_svg, "---
"))

cat("\n腳本執行完畢。\n")