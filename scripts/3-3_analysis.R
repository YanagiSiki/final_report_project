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
library(ggrepel)

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


# 4. 建立對應表並合併資料，並計算成長率
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

# 進行合併並計算成長率
merged_data <- left_join(registration_annual, mapping_table_final, by = "群類名稱") %>%
  inner_join(industry_salary_annual, by = c("行業別", "年度")) %>%
  filter(!is.na(總薪資)) %>% # 移除沒有對應到薪資的科系
  arrange(群類名稱, 年度) %>%
  group_by(群類名稱) %>%
  mutate(
    報名人數成長率 = (報名人數 - lag(報名人數)) / lag(報名人數),
    薪資成長率 = (總薪資 - lag(總薪資)) / lag(總薪資)
  ) %>%
  ungroup() %>%
  filter(!is.na(報名人數成長率)) # 移除第一年 (109年) 無法計算成長率的資料

cat("--- 步驟 4：已合併年度資料並計算成長率 ---\n\n")


# 5. 統計分析：成長率迴歸分析
# ------------------------------------------------
cat("--- 步驟 5：執行迴歸分析 ---\n")

# 模型 1: 動態反應 (薪資漲幅 vs 報名回升)
model_dynamic <- lm(報名人數成長率 ~ 薪資成長率, data = merged_data)
summary_dynamic <- summary(model_dynamic)

# 模型 2: 抗跌性 (絕對薪資 vs 報名跌幅)
# 測試高薪科系是否跌得比較少 (成長率較高/較不負)
model_resilience <- lm(報名人數成長率 ~ 總薪資, data = merged_data)
summary_resilience <- summary(model_resilience)

# 輸出結果到文字檔
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
# ------------------------------------------------
cat("--- 步驟 6：繪製成長率散佈圖 ---\n")

# 計算平均報名人數成長率 (作為基準線)
avg_reg_growth <- mean(merged_data$報名人數成長率, na.rm = TRUE)

plot_data <- merged_data %>%
  mutate(
    Label = ifelse(年度 == 113, 群類名稱, NA), # 只標示最新年份或特定點
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
    aes(label = paste0(群類名稱, "(", 年度, ")")),
    size = 3,
    max.overlaps = 20,
    box.padding = 0.5
  ) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "薪資成長率 vs 報名人數成長率 (110-113學年度)",
    subtitle = paste0("紅色虛線為平均成長率 (", round(avg_reg_growth * 100, 1), "%)，點大小代表絕對薪資水準"),
    x = "薪資年增率 (YoY)",
    y = "報名人數年增率 (YoY)",
    color = "表現分類",
    size = "平均月薪"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Microsoft JhengHei"))

ggsave("output/figures/3-3_growth_rate_scatter.png", p1, width = 8, height = 6)

# 額外繪製：絕對薪資 vs 報名人數成長率 (抗跌性圖表)
p2 <- ggplot(plot_data, aes(x = 總薪資, y = 報名人數成長率)) +
  geom_hline(yintercept = avg_reg_growth, linetype = "dashed", color = "red") +
  geom_point(aes(color = 群類名稱), alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "絕對薪資 vs 報名人數成長率 (抗跌性分析)",
    subtitle = "測試高薪是否能減緩報名人數的跌幅",
    x = "產業平均月薪 (元)",
    y = "報名人數年增率 (YoY)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Microsoft JhengHei"),
    legend.position = "none" # 隱藏圖例避免太亂
  )

ggsave("output/figures/3-3_resilience_scatter.png", p2, width = 8, height = 6)

cat("--- 圖表已輸出至 output/figures/ ---\n")

cat("\n腳本 (3-3_analysis.R) 執行完畢。\n")
