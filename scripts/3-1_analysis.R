
# 執行前請先安裝套件:
# install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "svglite", "gghighlight"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(gghighlight)

# --- 0. SETUP ---
data_path <- "data/"
image_output_path <- "output_chapters/images/"

theme_set(theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray40"),
    legend.position = "none" # gghighlight creates its own labels
  ))

# --- 1. LOAD DATA ---
salary_data <- read_csv(file.path(data_path, "salary_data_109_113.csv"), show_col_types = FALSE)
birth_cohort_data <- read_csv(file.path(data_path, "tcte_birth_cohort_statistics_109_113.csv"), show_col_types = FALSE)
registration_data <- read_csv(file.path(data_path, "tcte_registration_109_114.csv"), show_col_types = FALSE)

cat("---", "正在產生新的 3.1.2 視覺化圖表 ---", "\n\n")

# --- PLOT 1: Salary Trends by Industry (Multi-line Chart) ---
salary_trend_plot <- salary_data %>%
  filter(行業別 != "工業及服務業總計") %>%
  ggplot(aes(x = 年度, y = 總薪資, color = 行業別, group = 行業別)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 109:113) +
  gghighlight(max(總薪資) > 80000, label_params = list(nudge_x = 1, segment.color = NA)) +
  labs(
    title = "各行業總薪資趨勢變化",
    subtitle = "圖中突顯113年總薪資最高的行業",
    x = "年度",
    y = "總薪資 (元)",
    caption = "資料來源：行政院主計總處"
  )

ggsave(
  filename = file.path(image_output_path, "3_1_salary_trends_by_industry.svg"),
  plot = salary_trend_plot,
  width = 10, height = 7
)
cat("已儲存圖表: 3_1_salary_trends_by_industry.svg\n")


# --- PLOT 2: Birth Cohort & Registration Trend (Line Chart) ---
birth_reg_plot <- ggplot(birth_cohort_data, aes(x = 統測學年度)) +
  geom_line(aes(y = 該年出生人數, color = "該年出生人數"), linewidth = 1.2) +
  geom_line(aes(y = 統測報名人數, color = "統測報名人數"), linewidth = 1.2) +
  geom_point(aes(y = 該年出生人數), size = 2.5) +
  geom_point(aes(y = 統測報名人數), size = 2.5) +
  scale_y_continuous(name = "人數", labels = scales::comma) +
  scale_color_manual(name = "指標", values = c("該年出生人數" = "#D55E00", "統測報名人數" = "#0072B2")) +
  theme(legend.position = "top") +
  labs(
    title = "歷年出生人口與統測總報名人數趨勢",
    x = "學年度",
    y = "人數",
    caption = "資料來源：內政部戶政司、技專校院入學測驗中心"
  )

ggsave(
  filename = file.path(image_output_path, "3_1_birth_reg_trend.svg"),
  plot = birth_reg_plot,
  width = 8, height = 5
)
cat("已儲存圖表: 3_1_birth_reg_trend.svg\n")


# --- PLOT 3: TCTE Registrations Trend by Group (Multi-line Chart) ---
registration_long <- registration_data %>%
  filter(群類代號 != "All") %>%
  pivot_longer(
    cols = ends_with("學年度"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(學年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數))

# Define groups to highlight
highlight_groups <- c("餐旅群", "商業與管理群", "設計群", "電機與電子群資電類")

registration_trend_plot <- ggplot(registration_long, aes(x = 學年度, y = 報名人數, color = 群類名稱, group = 群類名稱)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 109:114) +
  gghighlight(群類名稱 %in% highlight_groups, label_params = list(nudge_x = 1.5, segment.color = NA, max.overlaps = 15)) +
  labs(
    title = "各科系群類報名人數趨勢變化",
    subtitle = "圖中突顯餐旅、商管、設計、資電類",
    x = "學年度",
    y = "報名人數",
    caption = "資料來源：技專校院入學測驗中心"
  )

ggsave(
  filename = file.path(image_output_path, "3_1_registration_trends_by_group.svg"),
  plot = registration_trend_plot,
  width = 10, height = 7
)
cat("已儲存圖表: 3_1_registration_trends_by_group.svg\n")


cat("\n--- 所有圖表已全數更新完畢。 ---\n")
