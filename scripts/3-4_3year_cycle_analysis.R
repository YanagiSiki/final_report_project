# scripts/3-4_3year_cycle_analysis.R
#
# 目標：執行「3年1動」週期分析 - 薪資成長率 vs 科系人數占比成長率 (3-Year Cycle Analysis)
# 說明：規劃每3年為一個週期，觀察長期趨勢
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

cat("--- 步驟 1：套件載入成功 (3-4_3year_cycle_analysis.R) ---\n\n")

# 2. 載入原始資料 (使用完整 100-113 年資料)
salary_data <- read_csv("data/salary_data_100_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_100_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---\n\n")

# 3. 準備資料
# a. 準備歷年行業薪資資料
industry_salary_annual <- salary_data %>%
    filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", between(年度, 100, 113)) %>%
    select(行業別 = 業別, 年度, 總薪資 = 值)

# b. 準備歷年科系報名人數資料，並計算「占比」(Market Share)
registration_annual <- registration_data %>%
    pivot_longer(
        cols = matches("^\\d{3}學年度$"),
        names_to = "學年度_str",
        values_to = "報名人數"
    ) %>%
    mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
    filter(!is.na(報名人數), 群類代號 != "All", between(年度, 100, 113)) %>%
    group_by(年度) %>%
    mutate(當年度總報名人數 = sum(報名人數)) %>%
    ungroup() %>%
    mutate(報名占比 = 報名人數 / 當年度總報名人數) %>%
    select(群類名稱, 年度, 報名人數, 報名占比)

# 4. 建立對應表
mapping_table_final <- tribble(
    ~行業別, ~群類名稱,
    "住宿及餐飲業", "餐旅群",
    "出版影音及資通訊業", "電機與電子群資電類",
    "金融及保險業", "商業與管理群",
    "製造業", "機械群",
    "製造業", "動力機械群",
    "營建工程業", "土木與建築群",
    "醫療保健業", "衛生與護理類",
    "藝術﹑娛樂及休閒服務業", "藝術群影視類",
    "製造業", "化工群",
)


# 5. 依每3年分組計算平均市佔率與平均薪資
periods <- list(
    `100-102` = 100:102,
    `103-105` = 103:105,
    `106-108` = 106:108,
    `109-111` = 109:111,
    `112-113` = 112:113
)

get_period <- function(year) {
    for (p in names(periods)) {
        if (year %in% periods[[p]]) {
            return(p)
        }
    }
    return(NA)
}

registration_annual$period <- sapply(registration_annual$年度, get_period)
industry_salary_annual$period <- sapply(industry_salary_annual$年度, get_period)

# 計算每群類每3年期的平均市佔率
reg_3y <- registration_annual %>%
    filter(!is.na(period)) %>%
    group_by(群類名稱, period) %>%
    summarise(平均市佔率 = mean(報名占比, na.rm = TRUE), .groups = "drop")

# 計算每行業每3年期的平均薪資
sal_3y <- industry_salary_annual %>%
    filter(!is.na(period)) %>%
    group_by(行業別, period) %>%
    summarise(平均薪資 = mean(總薪資, na.rm = TRUE), .groups = "drop")

# 合併對應表
analysis_3y <- reg_3y %>%
    inner_join(mapping_table_final, by = "群類名稱") %>%
    inner_join(sal_3y, by = c("行業別", "period"))

cat("--- 步驟 5：已計算每3年平均市佔率與平均薪資 ---\n")
print(head(analysis_3y))

# 6. 執行回歸分析（市佔率 ~ 薪資）
model_3y_avg <- lm(平均市佔率 ~ 平均薪資, data = analysis_3y)
summary_3y_avg <- summary(model_3y_avg)

# 輸出迴歸結果
output_file <- "output/3year_cycle_regression_results.txt"
sink(output_file)
cat("=== 3年期平均資料回歸分析結果 (100-113年) ===\n")
cat("模型：科系市佔率3年平均 ~ 行業總薪資3年平均\n\n")
print(summary_3y_avg)
sink()

cat(paste0("\n回歸分析結果已儲存至：", output_file, "\n"))

# 7. 繪製散點圖
library(ggrepel)
p <- ggplot(analysis_3y, aes(x = 平均薪資, y = 平均市佔率, color = 群類名稱)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid") +
    geom_text_repel(aes(label = paste0(群類名稱, "\n", period)), size = 3, max.overlaps = 50) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
        title = "3年期平均：薪資 vs 科系市佔率 (100-113年)",
        subtitle = "每點代表該群類於每3年期的平均值",
        x = "行業總薪資（3年平均, 元）",
        y = "科系市佔率（3年平均）",
        caption = "資料來源：教育部、主計總處"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

ggsave("output/figures/3-4_dynamics_3year_cycle.png", p, width = 10, height = 8, bg = "white")

cat("散點圖已儲存至：output/figures/3-4_dynamics_3year_cycle.png\n")
