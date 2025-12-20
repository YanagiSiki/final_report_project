# scripts/3-3_analysis.R
#
# 目標：執行 3.3 節分析 - 報名人數與薪資之關聯 (The Demographic Mask)
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

cat("--- 步驟 1：套件載入成功 (3-3_analysis.R) ---\n\n")

# 2. 載入原始資料
salary_data <- read_csv("data/salary_data_100_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_100_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---\n\n")

# 3. 準備年度資料
# a. 準備歷年行業薪資資料
industry_salary_annual <- salary_data %>%
  filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", between(年度, 100, 113)) %>%
  select(行業別 = 業別, 年度, 總薪資 = 值)

# b. 準備歷年科系報名人數資料
registration_annual <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All", between(年度, 100, 113)) %>%
  select(群類名稱, 年度, 報名人數)

cat("--- 步驟 3：已準備好歷年薪資與報名人數資料 ---\n\n")

# 4. 建立對應表並合併資料，並計算成長率
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

# 進行合併並計算成長率
merged_data <- left_join(registration_annual, mapping_table_final, by = "群類名稱", relationship = "many-to-many") %>%
  inner_join(industry_salary_annual, by = c("行業別", "年度")) %>%
  filter(!is.na(總薪資)) %>%
  arrange(群類名稱, 年度) %>%
  group_by(群類名稱) %>%
  mutate(
    報名人數成長率 = (報名人數 - lag(報名人數)) / lag(報名人數),
    薪資成長率 = (總薪資 - lag(總薪資)) / lag(總薪資)
  ) %>%
  ungroup() %>%
  filter(!is.na(報名人數成長率))

cat("--- 步驟 4：已合併年度資料並計算成長率 ---\n\n")

# 5. 統計分析：成長率迴歸分析
cat("--- 步驟 5：執行迴歸分析 ---\n")

model_dynamic <- lm(報名人數成長率 ~ 薪資成長率, data = merged_data)
summary_dynamic <- summary(model_dynamic)
model_resilience <- lm(報名人數成長率 ~ 總薪資, data = merged_data)
summary_resilience <- summary(model_resilience)

output_file <- "output/registration_regression_results.txt"
sink(output_file)
cat("=== 3-3 報名人數成長率分析結果 ===\n\n")
cat("--- 模型 1: 動態反應 (報名人數成長率 ~ 薪資成長率) ---\n")
print(summary_dynamic)
cat("\n")
cat("--- 模型 2: 抗跌性檢定 (報名人數成長率 ~ 絕對薪資) ---\n")
print(summary_resilience)
cat("\n")
sink()

cat(paste("統計結果已儲存至", output_file, "\n\n"))

# 6. 視覺化：薪資成長率 vs 報名人數成長率
cat("--- 步驟 6：繪製成長率散佈圖 ---\n")

avg_reg_growth <- mean(merged_data$報名人數成長率, na.rm = TRUE)

plot_data <- merged_data %>%
  mutate(
    Label = ifelse(年度 == 113, 群類名稱, NA),
    Performance = case_when(
      報名人數成長率 > 0 ~ "逆勢成長 (Growth)",
      報名人數成長率 > avg_reg_growth ~ "相對抗跌 (Resilient)",
      TRUE ~ "嚴重衰退 (Decline)"
    )
  )

p1 <- ggplot(plot_data, aes(x = 薪資成長率, y = 報名人數成長率)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = avg_reg_growth, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = Performance, size = 總薪資), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", alpha = 0.3) +
  geom_text_repel(
    data = subset(plot_data, 年度 == 113 | abs(報名人數成長率) > 0.15 | abs(薪資成長率) > 0.1),
    aes(label = paste0(群類名稱, "(", 年度, ")")),
    size = 3,
    max.overlaps = 50,
    box.padding = 0.5,
    force = 2
  ) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "薪資成長率 vs 報名人數成長率 (101-113學年度)",
    subtitle = paste0("紅色虛線為平均成長率 (", round(avg_reg_growth * 100, 1), "%)，點大小代表絕對薪資水準"),
    x = "薪資年增率 (YoY)",
    y = "報名人數年增率 (YoY)",
    color = "表現分類",
    size = "平均月薪"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Microsoft JhengHei"))

# 修改檔名以符合章節
ggsave("output/figures/3_3_salary_vs_reg_growth.png", p1, width = 8, height = 6)

# 註解掉未使用的圖表
# ggsave("output/figures/3-3_resilience_scatter.png", p2, width = 8, height = 6)

cat("--- 圖表已輸出至 output/figures/ ---\n")
cat("\n腳本 (3-3_analysis.R) 執行完畢。\n")
