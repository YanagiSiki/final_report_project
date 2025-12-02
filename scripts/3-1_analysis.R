# 執行前請先安裝套件:
# install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "svglite", "gghighlight", "GGally"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(gghighlight)
library(GGally)


# --- 0. SETUP ---
data_path <- "data/"
image_output_path <- "output_chapters/images/"

theme_set(theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray40"),
    legend.position = "top"
  ))

# --- 1. LOAD DATA ---
salary_data <- read_csv(file.path(data_path, "salary_data_109_113.csv"), show_col_types = FALSE)
birth_cohort_data <- read_csv(file.path(data_path, "tcte_birth_cohort_statistics_109_113.csv"), show_col_types = FALSE)
registration_data <- read_csv(file.path(data_path, "tcte_registration_109_114.csv"), show_col_types = FALSE)

cat("---", "正在產生 3.1.2 章節的視覺化圖表 ---", "\n\n")

# --- PLOT 1: Salary Trends by Industry (Multi-line Chart) ---
salary_trend_plot <- salary_data %>%
  filter(行業別 != "工業及服務業總計") %>%
  ggplot(aes(x = 年度, y = 總薪資, color = 行業別, group = 行業別)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 109:113) +
  gghighlight(max(總薪資) > 80000, label_params = list(nudge_x = 1, segment.color = NA), use_direct_label = TRUE) +
  theme(legend.position = "none") +
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
  pivot_longer(
    cols = ends_with("學年度"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(學年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All")

# Define groups to highlight
highlight_groups <- c("餐旅群", "商業與管理群", "設計群", "電機與電子群資電類")

registration_trend_plot <- ggplot(registration_long, aes(x = 學年度, y = 報名人數, color = 群類名稱, group = 群類名稱)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 109:114) +
  gghighlight(群類名稱 %in% highlight_groups, label_params = list(nudge_x = 1.5, segment.color = NA, max.overlaps = 15), use_direct_label = TRUE) +
  theme(legend.position = "none") +
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


# --- PLOT 4: Salary Growth Rate Heatmap ---
salary_growth_data <- salary_data %>%
  filter(行業別 != "工業及服務業總計") %>%
  group_by(行業別) %>%
  arrange(年度) %>%
  mutate(
    去年薪資 = lag(總薪資),
    年增率 = (總薪資 - 去年薪資) / 去年薪資
  ) %>%
  filter(!is.na(年增率)) # Remove rows where growth rate couldn't be calculated (i.e., the first year)

salary_heatmap_plot <- ggplot(salary_growth_data, aes(x = as.factor(年度), y = reorder(行業別, 總薪資), fill = 年增率)) +
  geom_tile(color = "white", lwd = 1.5) +
  geom_text(aes(label = scales::percent(年增率, accuracy = 0.1)), color = "white", size = 3) +
  scale_fill_gradient2(
    low = "#E57373", mid = "grey80", high = "#64B5F6", midpoint = 0,
    labels = scales::percent
  ) +
  labs(
    title = "各行業薪資年增率熱力圖",
    subtitle = "藍色為正增長，紅色為負增長",
    x = "年度",
    y = "行業別",
    fill = "年增率",
    caption = "資料來源：行政院主計總處"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(image_output_path, "3_1_salary_growth_heatmap.svg"),
  plot = salary_heatmap_plot,
  width = 10, height = 8
)
cat("已儲存圖表: 3_1_salary_growth_heatmap.svg\n")


# --- PLOT 5: Registration Total Change Bar Chart ---
reg_change_data <- registration_long %>%
  filter(學年度 %in% c(109, 114)) %>%
  select(-學年度_str) %>% # 移除此欄位，避免 pivot_wider 行為錯誤
  pivot_wider(names_from = 學年度, values_from = 報名人數, names_prefix = "y") %>%
  mutate(
    總人數變化 = y114 - y109
  ) %>%
  filter(!is.na(總人數變化))

reg_change_plot <- ggplot(reg_change_data, aes(x = 總人數變化, y = reorder(群類名稱, 總人數變化))) +
  geom_col(aes(fill = 總人數變化 > 0)) +
  geom_text(aes(label = scales::comma(總人數變化), hjust = ifelse(總人數變化 > 0, -0.1, 1.1)), size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"), guide = "none") +
  labs(
    title = "各科系群類 109-114 學年度總人數變化",
    subtitle = "比較 114 學年度與 109 學年度的報名人數差異",
    x = "總人數變化量",
    y = "科系群類",
    caption = "資料來源：技專校院入學測驗中心"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(linetype = "dashed", color = "gray80")
  )

ggsave(
  filename = file.path(image_output_path, "3_1_reg_total_change_bar.svg"),
  plot = reg_change_plot,
  width = 10, height = 8
)
cat("已儲存圖表: 3_1_reg_total_change_bar.svg\n")


# --- PLOT 6: Registration Share Stacked Area Chart ---
# To make the chart readable, we'll keep the top 5 groups and aggregate the rest.
top_groups <- registration_long %>%
  group_by(群類名稱) %>%
  summarise(total_reg = sum(報名人數)) %>%
  slice_max(order_by = total_reg, n = 5) %>%
  pull(群類名稱)

reg_share_data <- registration_long %>%
  mutate(群類標籤 = ifelse(群類名稱 %in% top_groups, 群類名稱, "其他")) %>%
  group_by(學年度, 群類標籤) %>%
  summarise(報名人數 = sum(報名人數), .groups = "drop") %>%
  group_by(學年度) %>%
  mutate(報名佔比 = 報名人數 / sum(報名人數))

reg_share_plot <- ggplot(reg_share_data, aes(x = 學年度, y = 報名佔比, fill = 群類標籤, group = 群類標籤)) +
  geom_area(alpha = 0.8, color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 109:114) +
  labs(
    title = "各科系群類報名人數佔比變化",
    subtitle = "圖中顯示佔比前五大群類，其餘歸為「其他」",
    x = "學年度",
    y = "報名人數佔比",
    fill = "科系群類",
    caption = "資料來源：技專校院入學測驗中心"
  )

ggsave(
  filename = file.path(image_output_path, "3_1_reg_share_area.svg"),
  plot = reg_share_plot,
  width = 10, height = 7
)
cat("已儲存圖表: 3_1_reg_share_area.svg\n")


# --- PLOT 7: Correlation Matrix ---
# Prepare data for correlation matrix (109-113)
# 1. Birth and Total Registration data
core_data_part1 <- birth_cohort_data %>%
  select(學年度 = 統測學年度, 出生人數 = 該年出生人數, 總報名人數 = 統測報名人數)

# 2. Key departments registration data
# (using the same highlighted groups from Plot 3)
core_data_part2 <- registration_long %>%
  filter(群類名稱 %in% highlight_groups) %>%
  group_by(學年度) %>%
  summarise(重點科系人數 = sum(報名人數))

# 3. Average salary data
core_data_part3 <- salary_data %>%
  group_by(年度) %>%
  summarise(平均總薪資 = mean(總薪資)) %>%
  mutate(學年度 = 年度) %>%
  select(學年度, 平均總薪資)

# Combine all parts

correlation_df <- core_data_part1 %>%
  inner_join(core_data_part2, by = "學年度") %>%
  inner_join(core_data_part3, by = "學年度") %>%
  select(-學年度) # ggpairs works best with just numeric columns

# Custom function for upper panel to show correlation with significance
custom_upper <- function(data, mapping) {
  # 取得變數名稱
  x <- as_label(mapping$x)
  y <- as_label(mapping$y)
  # 計算相關係數
  corr <- cor.test(data[[x]], data[[y]])
  r <- corr$estimate
  p <- corr$p.value

  # 顯示顯著性星號
  stars <- case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )

  text_label <- paste0(round(r, 2), "\n", stars)

  ggally_text(
    label = text_label,
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    size = 5,
    color = "black"
  ) +
    theme_void() +
    theme(panel.background = element_rect(fill = "grey95", color = NA))
}

correlation_matrix_plot <- ggpairs(
  correlation_df,
  title = "核心變數相關性矩陣圖",
  upper = list(continuous = custom_upper),
  lower = list(continuous = wrap("points", alpha = 0.5, size = 3), combo = "dot", discrete = "facetbar"),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5))
) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold", size = 10)
  )

ggsave(
  filename = file.path(image_output_path, "3_1_correlation_matrix.svg"),
  plot = correlation_matrix_plot,
  width = 12, height = 12
)
cat("已儲存圖表: 3_1_correlation_matrix.svg\n")

cat("\n--- 所有 3.1.2 章節的圖表已全數更新完畢。 ---\n")
