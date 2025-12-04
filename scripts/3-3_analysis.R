# scripts/3-3_analysis.R
#
# 目標：執行「關鍵決策矩陣」分析，探討「行業薪資水平」與「科系報名人數變動率」的關係。
# 更新：
# 1. 採用完整的行業-科系對應表。
# 2. 計算 113 年行業平均薪資水平。
# 3. 計算 109-114 學年度科系報名人數變動率。
# 4. 根據上述變數繪製散佈圖 (決策矩陣圖)。
# 5. 移除原先不符合 3.3 節目標的相關性與迴歸分析。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
# ---------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(stringr) # 用於 geom_text 中的文字截斷

cat("--- 步驟 1：套件載入成功 ---
\n")

# 2. 載入原始資料
# ---------------------------
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---
\n")

# 3. 【修正】準備分析所需的指標資料
# ---------------------------------

# 計算 113 年各行業的平均總薪資 (用於 X 軸)
# 排除總計行，並過濾出 113 年資料
industry_salary_113 <- salary_data %>%
  filter(年度 == 113, 行業別 != "工業及服務業總計") %>%
  group_by(行業別) %>%
  summarise(
    行業平均總薪資_113年 = mean(總薪資, na.rm = TRUE),
    .groups = "drop"
  )

cat("--- 已計算 113 年各行業平均總薪資 ---
")
print(head(industry_salary_113))
cat("\n")

# 計算各科系群類 109-114 學年度報名人數變動率 (用於 Y 軸)
# 步驟：pivot_longer -> 篩選 109 & 114 年度資料 -> pivot_wider -> 計算變動率
registration_change_rate <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"), # 匹配學年度的欄位
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All") %>% # 移除 NA 和總計行
  group_by(群類名稱) %>%
  summarise(
    人數_109年 =報名人數[年度 == 109],
    人數_114年 = 報名人數[年度 == 114],
    .groups = "drop"
  ) %>%
  # 避免分母為零，並處理 NA 情況
  mutate(
    報名人數變動率 = ifelse(人數_109年 == 0, NA, (人數_114年 - 人數_109年) / 人數_109年)
  ) %>%
  select(群類名稱, 報名人數變動率) %>%
  na.omit()

cat("--- 已計算各科系群類 109-114 學年度報名人數變動率 ---
")
print(head(registration_change_rate))
cat("\n")


# 4. 【修正】建立完整的對應表並合併資料
# ------------------------------------
# 採用 prompt_master_outline.md 中提供的完整對應表
mapping_table_full <- tribble(
  ~行業別, ~群類名稱,
  "住宿及餐飲業", "餐旅群",
  "出版、影音製作、程式設計及資通訊服務業", "電機與電子群資電類", # 對應 04 資電類 / 03 電機類
  "電子零組件製造業", "電機與電子群資電類", # 新增 電子零組件製造業
  "金融及保險業", "商業與管理群",
  "批發及零售業", "商業與管理群", # 新增 批發及零售業
  "製造業", "機械群", # 對應 01 機械群 / 02 動力機械群 (製造業泛稱)
  "營建工程業", "土木與建築群",
  "醫療保健及社會工作服務業", "衛生與護理類", # 新增 衛生與護理類
  "藝術、娛樂及休閒服務業", "藝術群影視類", # 對應 20 藝術群影視類
  "化學材料製造業", "化工群" # 新增 化工群，並明確為「化學材料製造業」
)

# 為了簡化，如果行業別在 mapping_table_full 中有多個群類對應，我們只取第一個
# 通常這裡會更嚴謹地處理多對多關係，但為了一對一合併，我們保持簡潔
# 另外，為了避免重複合併，我們讓 mapping_table_full 中的行業別唯一
mapping_table_unique <- mapping_table_full %>% distinct(行業別, .keep_all = TRUE)


# 進行合併
analysis_data_final <- inner_join(mapping_table_full, industry_salary_113, by = "行業別") %>%
  inner_join(registration_change_rate, by = "群類名稱")

# 確保所有變數都已正確準備
analysis_data_final <- analysis_data_final %>%
  select(行業別, 群類名稱, 行業平均總薪資_113年, 報名人數變動率) %>%
  na.omit()

cat("--- 已建立完整對應表並合併資料 ---
")
print(head(analysis_data_final))
cat("\n")


# 5. 【修正】產生新的視覺化圖表 (決策矩陣散佈圖)
# ------------------------------------------
cat("--- 步驟 5：產生決策矩陣散佈圖 ---
")

# 計算平均值以繪製中心線
avg_salary <- mean(analysis_data_final$行業平均總薪資_113年, na.rm = TRUE)
avg_change_rate <- mean(analysis_data_final$報名人數變動率, na.rm = TRUE)

scatter_plot <- ggplot(analysis_data_final, aes(x = 行業平均總薪資_113年, y = 報名人數變動率)) +
  geom_point(aes(color = 群類名稱, size = 行業平均總薪資_113年 / 10000), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed") + # 趨勢線
  geom_vline(xintercept = avg_salary, linetype = "dotted", color = "gray") + # 薪資平均線
  geom_hline(yintercept = avg_change_rate, linetype = "dotted", color = "gray") + # 變動率平均線
  geom_text(aes(label = str_wrap(群類名稱, 10)), # 使用 str_wrap 讓標籤換行
            vjust = -1.5, hjust = 0.5, size = 3, check_overlap = TRUE, color = "black") + # 添加群類名稱標籤
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "行業平均薪資 vs. 科系群類報名人數變動率決策矩陣圖",
    subtitle = "每個點代表一個科系群類，點的大小反映其對應行業薪資水平",
    x = "113年行業平均總薪資 (元)",
    y = "109-114學年度報名人數變動率",
    size = "行業薪資水平 (萬)",
    color = "科系群類",
    caption = "資料來源：行政院主計總處、技專校院入學測驗中心"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  )

# 儲存圖表，使用報告中指定的檔名
output_path_svg <- "output_chapters/images/3_3_decision_matrix.svg"
ggsave(
  filename = output_path_svg,
  plot = scatter_plot,
  width = 12, height = 9
)
cat(paste("--- 步驟 5：決策矩陣圖已儲存至", output_path_svg, "---
"))

cat("\n腳本執行完畢。
")
