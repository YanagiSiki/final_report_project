# scripts/3-2_analysis.R
#
# 目標：執行 3.2 節所需的單因子變異數分析 (One-way ANOVA)。
# 重點：強調「金融保險業」與「住宿及餐飲業」的顯著差異。
#
# -----------------------------------------------------------------------------

# 1. 載入套件
library(readr)
library(dplyr)
library(ggplot2)

cat("--- 步驟 1：套件載入成功 (3-2_analysis.R) ---\n\n")

# 2. 讀取資料
file_path <- "data/salary_data_100_113.csv"
salary_data <- read_csv(file_path, show_col_types = FALSE)

cat("--- 步驟 2：資料載入成功 ---\n\n")


# 3. 準備 ANOVA 資料
# ------------------------------------
salary_for_analysis <- salary_data %>%
  filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", !is.na(值)) %>%
  select(行業別 = 業別, 總薪資 = 值) %>%
  mutate(行業別 = as.factor(行業別))

cat("--- 步驟 3：已準備好用於 ANOVA 分析的資料 ---\n\n")

# 3.1 繪製各行業平均總薪資盒鬚圖 (Boxplot) - 強調金融 vs 餐飲
# -----------------------------------------------------------------------------

# 計算每個行業 5 年來的平均薪資，用於排序
industry_order <- salary_data %>%
  filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", !is.na(值)) %>%
  group_by(業別) %>%
  summarise(mean_salary = mean(值, na.rm = TRUE)) %>%
  arrange(mean_salary) %>%
  pull(業別)

# 轉換為 factor 以控制繪圖順序
salary_for_plot <- salary_data %>%
  filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", !is.na(值)) %>%
  rename(行業別 = 業別, 總薪資 = 值)
salary_for_plot$行業別 <- factor(salary_for_plot$行業別, levels = industry_order)

# 設定顏色：預設灰色，金融與餐飲特別標色
salary_for_plot <- salary_for_plot %>%
  mutate(highlight = case_when(
    行業別 == "金融及保險業" ~ "High",
    行業別 == "住宿及餐飲業" ~ "Low",
    TRUE ~ "Normal"
  ))

p_boxplot <- ggplot(salary_for_plot, aes(x = 行業別, y = 總薪資, fill = highlight)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + # 隱藏 outlier，改用 jitter point 顯示
  geom_point(position = position_jitter(width = 0.15, seed = 123), size = 1.5, alpha = 0.6) + # 加入散佈點
  scale_fill_manual(values = c("High" = "#E74C3C", "Low" = "#3498DB", "Normal" = "#BDC3C7")) +
  coord_flip() + # 轉置座標軸，讓長行業名稱好讀
  labs(
    title = "產業薪資M型化：金融業 vs 餐飲業",
    subtitle = "各行業 100-113 年平均總薪資分佈 (ANOVA 檢定差異顯著)",
    x = "行業別",
    y = "平均總薪資 (元)",
    caption = "資料來源：行政院主計總處"
  ) +
  theme_minimal(base_family = "Microsoft JhengHei") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none" # 隱藏圖例
  )

# 儲存圖表
ggsave("output/figures/3_2_salary_boxplot.png", plot = p_boxplot, width = 10, height = 8, dpi = 300)
cat("已儲存: output/figures/3_2_salary_boxplot.png\n")


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
