# scripts/2_descriptive_analysis.R
#
# 目標：執行第二章所需的描述性統計與探索性資料分析 (EDA)。
# 此腳本旨在產生一系列視覺化圖表，以呈現資料的宏觀趨勢。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
# ---------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(gghighlight)
library(scales)

cat("--- 步驟 1：套件載入成功 (2_descriptive_analysis.R) ---\n\n")

# 2. 資料與路徑設定
# ---------------------------
data_path <- "data/"
image_output_path <- "output/figures/"

# 讀取所需資料
birth_cohort_data <- read_csv(file.path(data_path, "tcte_birth_cohort_statistics_100_113.csv"), show_col_types = FALSE)
registration_data <- read_csv(file.path(data_path, "tcte_registration_100_114.csv"), show_col_types = FALSE)
salary_data <- read_csv(file.path(data_path, "salary_data_100_113.csv"), show_col_types = FALSE)

cat("--- 步驟 2：資料載入成功 ---\n\n")


# --- 3. 生成所有描述性圖表 ---

# 圖表 2.1: 歷年產業薪資趨勢
# -----------------------------------------------
cat("--- 步驟 3a：生成 2_1_salary_trends.svg ---\n")
salary_for_plot <- salary_data %>%
  filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", between(年度, 100, 113)) %>%
  rename(行業別 = 業別, 總薪資 = 值)

highlighted_industries <- c("金融及保險業", "住宿及餐飲業")

line_chart <- ggplot(salary_for_plot, aes(x = 年度, y = 總薪資, color = 行業別, group = 行業別)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  gghighlight(行業別 %in% highlighted_industries, label_params = list(nudge_x = 0.5, segment.color = NA), use_direct_label = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 100:113) +
  labs(
    title = "100-113年 各主要行業別平均總薪資趨勢",
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

ggsave(
  filename = file.path(image_output_path, "2_1_salary_trends.svg"),
  plot = line_chart,
  width = 10, height = 7
)
cat("--- 2_1_salary_trends.svg 已儲存 ---\n\n")


# 圖表 2.2: 各科系群類報名人數趨勢
# ----------------------------------------------
cat("--- 步驟 3b：生成 2_2_registration_trends_by_group.svg ---\n")
registration_long <- registration_data %>%
  pivot_longer(
    cols = ends_with("學年度"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(學年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All")

highlight_groups <- c("餐旅群", "商業與管理群", "電機與電子群資電類", "衛生與護理類")

registration_trend_plot <- ggplot(registration_long, aes(x = 學年度, y = 報名人數, color = 群類名稱, group = 群類名稱)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 100:114) +
  gghighlight(群類名稱 %in% highlight_groups, label_params = list(nudge_x = 1.5, segment.color = NA, max.overlaps = 15), use_direct_label = TRUE) +
  labs(
    title = "各科系群類報名人數趨勢變化 (100-114年)",
    subtitle = "圖中突顯餐旅、商管、資電、衛生護理類",
    x = "學年度",
    y = "報名人數",
    caption = "資料來源：技專校院入學測驗中心"
  ) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "none")

ggsave(
  filename = file.path(image_output_path, "2_2_registration_trends_by_group.svg"),
  plot = registration_trend_plot,
  width = 10, height = 7
)
cat("--- 2_2_registration_trends_by_group.svg 已儲存 ---\n\n")


# 圖表 2.3: 薪資年增率熱力圖
# ------------------------------------------
cat("--- 步驟 3c：生成 2_3_salary_growth_heatmap.svg ---\n")
salary_growth_data <- salary_data %>%
  filter(行業別 != "工業及服務業總計") %>%
  group_by(行業別) %>%
  arrange(年度) %>%
  mutate(
    去年薪資 = lag(總薪資),
    年增率 = (總薪資 - 去年薪資) / 去年薪資
  ) %>%
  filter(!is.na(年增率))

salary_heatmap_plot <- ggplot(salary_growth_data, aes(x = as.factor(年度), y = reorder(行業別, 總薪資), fill = 年增率)) +
  geom_tile(color = "white", lwd = 1.5) +
  geom_text(aes(label = scales::percent(年增率, accuracy = 0.1)), color = "white", size = 3) +
  scale_fill_gradient2(
    low = "#E57373", mid = "grey80", high = "#64B5F6", midpoint = 0,
    labels = scales::percent
  ) +
  labs(
    title = "各行業薪資年增率熱力圖 (101-113年)",
    subtitle = "藍色為正增長，紅色為負增長",
    x = "年度",
    y = "行業別",
    fill = "年增率",
    caption = "資料來源：行政院主計總處"
  ) +
  theme_minimal(base_family = "sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(image_output_path, "2_3_salary_growth_heatmap.svg"),
  plot = salary_heatmap_plot,
  width = 10, height = 8
)
cat("--- 2_3_salary_growth_heatmap.svg 已儲存 ---\n\n")


cat("\n描述性統計圖表腳本 (2_descriptive_analysis.R) 執行完畢。\n")
