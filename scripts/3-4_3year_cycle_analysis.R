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
    "醫療保健及社會工作服務業", "衛生與護理類",
    "藝術、娛樂及休閒服務業", "藝術群影視類",
    "製造業", "化工群",
)

# 5. 合併資料並計算「3年1動」成長率
# 定義：3年成長率 = (Value_t - Value_{t-3}) / Value_{t-3}

analysis_data <- registration_annual %>%
    inner_join(mapping_table_final, by = "群類名稱", relationship = "many-to-many") %>%
    inner_join(industry_salary_annual, by = c("行業別", "年度")) %>%
    arrange(群類名稱, 行業別, 年度) %>%
    group_by(群類名稱, 行業別) %>%
    mutate(
        薪資_3年成長率 = (總薪資 - lag(總薪資, 3)) / lag(總薪資, 3),
        占比_3年成長率 = (報名占比 - lag(報名占比, 3)) / lag(報名占比, 3)
    ) %>%
    ungroup() %>%
    filter(!is.na(薪資_3年成長率), !is.na(占比_3年成長率))

cat("--- 步驟 5：已計算 3 年週期成長率 ---\n")
print(head(analysis_data))

# 6. 執行迴歸分析
# 模型：占比_3年成長率 ~ 薪資_3年成長率
model_3y <- lm(占比_3年成長率 ~ 薪資_3年成長率, data = analysis_data)
summary_3y <- summary(model_3y)

# 輸出迴歸結果
output_file <- "output/3year_cycle_regression_results.txt"
sink(output_file)
cat("=== 3年週期迴歸分析結果 (100-113年) ===\n")
cat("模型：科系報名占比3年成長率 ~ 行業總薪資3年成長率\n\n")
print(summary_3y)
sink()

cat(paste0("\n迴歸分析結果已儲存至：", output_file, "\n"))

# 7. 繪製散佈圖
p <- ggplot(analysis_data, aes(x = 薪資_3年成長率, y = 占比_3年成長率)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = 群類名稱), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid") +
    geom_text_repel(
        data = subset(analysis_data, 年度 == 113 | abs(占比_3年成長率) > 0.15 | abs(薪資_3年成長率) > 0.15),
        aes(label = paste0(群類名稱, "(", 年度, ")")),
        size = 3,
        max.overlaps = 50,
        box.padding = 0.5,
        force = 2
    ) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    labs(
        title = "3年週期分析：薪資成長 vs 科系占比成長 (100-113年)",
        subtitle = "每點代表該年度與3年前相比的成長率",
        x = "行業總薪資 3年成長率",
        y = "科系報名占比 3年成長率",
        caption = "資料來源：教育部、主計總處"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

ggsave("output/figures/3-4_dynamics_3year_cycle.png", p, width = 10, height = 8, bg = "white")

cat("散佈圖已儲存至：output/figures/3-4_dynamics_3year_cycle.png\n")
