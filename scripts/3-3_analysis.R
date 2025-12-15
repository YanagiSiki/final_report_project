# scripts/3-3_analysis.R
#
# 目標：執行 3.3 節分析 - 報名人數與薪資之關聯 (The Demographic Mask)
# 驗證「產業平均薪資」與「統測報名人數」的關係，並揭示少子化的遮蔽效應。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

cat("--- 步驟 1：套件載入成功 (3-3_analysis.R) ---\n\n")


# 2. 載入原始資料
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---\n\n")


# 3. 準備年度資料
# ---------------------------
# a. 準備歷年行業薪資資料
industry_salary_annual <- salary_data %>%
  filter(行業別 != "工業及服務業總計", between(年度, 109, 113)) %>%
  select(行業別, 年度, 總薪資)

# b. 準備歷年科系報名人數資料
registration_annual <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All", between(年度, 109, 113)) %>%
  select(群類名稱, 年度, 報名人數)

cat("--- 步驟 3：已準備好歷年薪資與報名人數資料 ---\n\n")


# 4. 建立對應表並合併資料
# ------------------------------------
mapping_table_final <- tribble(
  ~行業別, ~群類名稱,
  "住宿及餐飲業", "餐旅群",
  "出版影音及資通訊業", "電機與電子群資電類",
  "金融及保險業", "商業與管理群",
  "製造業", "機械群",
  "製造業", "動力機械群",
  "營建工程業", "土木與建築群",
  "醫療保健及社會工作服務業", "衛生與護理類",
  "藝術、娛樂及休閒服務業", "藝術群影視類",
  "製造業", "化工群",
)

# 進行合併
merged_data <- left_join(registration_annual, mapping_table_final, by = "群類名稱") %>%
  inner_join(industry_salary_annual, by = c("行業別", "年度")) %>%
  filter(!is.na(總薪資)) # 移除沒有對應到薪資的科系

cat("--- 步驟 4：已合併年度資料 ---\n\n")


# 5. 統計分析：簡單線性迴歸 (報名人數 ~ 總薪資)
# ------------------------------------------------
cat("--- 步驟 5：執行迴歸分析 ---\n")

model <- lm(報名人數 ~ 總薪資, data = merged_data)
summary_model <- summary(model)

# 輸出結果到文字檔
output_file <- "output/registration_regression_results.txt"
sink(output_file)
cat("=== 3-3 報名人數與薪資迴歸分析結果 ===\n\n")
print(summary_model)
cat("\n\n解讀：\n")
cat(paste("R-squared:", round(summary_model$r.squared, 4), "\n"))
cat(paste("p-value:", format.pval(summary(model)$coefficients[2, 4]), "\n"))
sink()

cat(paste("統計結果已儲存至", output_file, "\n\n"))


# 6. 視覺化：薪資 vs 報名人數散佈圖 (標示抗跌與重災區)
# ------------------------------------------------
cat("--- 步驟 6：繪製散佈圖 ---\n")

# 定義要特別標示的群類
highlight_groups <- c("電機與電子群資電類", "衛生與護理類", "餐旅群", "外語群英語類")

plot_data <- merged_data %>%
  mutate(
    Label = ifelse(群類名稱 %in% highlight_groups, 群類名稱, NA),
    Category = case_when(
      群類名稱 %in% c("電機與電子群資電類", "衛生與護理類") ~ "抗跌區 (Resilient)",
      群類名稱 %in% c("餐旅群", "外語群英語類") ~ "重災區 (Hardest Hit)",
      TRUE ~ "其他"
    )
  )

p <- ggplot(plot_data, aes(x = 總薪資, y = 報名人數)) +
  geom_point(aes(color = Category, size = Category), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "gray", linetype = "dashed", alpha = 0.2) +
  geom_text(aes(label = 群類名稱), vjust = -0.8, size = 3, check_overlap = TRUE) + # 加入群類名稱標籤
  scale_color_manual(values = c("抗跌區 (Resilient)" = "#E74C3C", "重災區 (Hardest Hit)" = "#3498DB", "其他" = "#95A5A6")) +
  scale_size_manual(values = c("抗跌區 (Resilient)" = 4, "重災區 (Hardest Hit)" = 4, "其他" = 2)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "薪資高不一定人多：少子化的遮蔽效應",
    subtitle = paste0("R² = ", round(summary_model$r.squared, 4), " (相關性極低)"),
    x = "產業平均年薪 (元)",
    y = "統測報名人數 (人)"
  ) +
  theme_minimal(base_family = "Microsoft JhengHei") +
  theme(legend.position = "bottom")

ggsave("output/figures/registration_vs_salary_scatter.png", plot = p, width = 10, height = 8, dpi = 300)
cat("圖表已輸出至 output/figures/registration_vs_salary_scatter.png\n")

cat("\n腳本 (3-3_analysis.R) 執行完畢。\n")
