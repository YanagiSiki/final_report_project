# scripts/3-3_proportion.R
#
# 目的：執行 3.3 節分析 - 科系選擇偏好與薪資關聯分析 (Market Share Analysis)
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
cat("--- 步驟 1：套件載入成功 ---\n")

# 2. 載入原始資料
salary_data <- read_csv("data/salary_data_100_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_100_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---\n")

# 3. 資料前處理：計算科系佔比 (Market Share)
registration_long <- registration_data %>%
    pivot_longer(
        cols = matches("^\\d{3}學年度$"),
        names_to = "學年度_str",
        values_to = "報名人數"
    ) %>%
    mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
    filter(!is.na(報名人數), 群類代號 != "All", between(年度, 100, 113)) %>%
    select(群類名稱, 年度, 報名人數)

registration_proportion <- registration_long %>%
    group_by(年度) %>%
    mutate(
        當年度總報名人數 = sum(報名人數),
        報名佔比 = 報名人數 / 當年度總報名人數,
        報名佔比百分比 = 報名佔比 * 100
    ) %>%
    ungroup()

cat("--- 步驟 3：科系佔比計算完成 ---\n")

# 4. 建立對應表並合併薪資資料
mapping_table <- tribble(
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

industry_salary <- salary_data %>%
    filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", between(年度, 100, 113)) %>%
    select(行業別 = 業別, 年度, 總薪資 = 值)

analysis_data <- registration_proportion %>%
    inner_join(mapping_table, by = "群類名稱", relationship = "many-to-many") %>%
    inner_join(industry_salary, by = c("行業別", "年度")) %>%
    arrange(群類名稱, 年度) %>%
    group_by(群類名稱) %>%
    mutate(
        佔比年增率 = (報名佔比 - lag(報名佔比)) / lag(報名佔比),
        薪資年增率 = (總薪資 - lag(總薪資)) / lag(總薪資)
    ) %>%
    ungroup() %>%
    filter(!is.na(佔比年增率))

cat("--- 步驟 4：資料合併與成長率計算完成 ---\n")

# 5. 視覺化分析
key_groups <- c("電機與電子群資電類", "衛生與護理類", "機械群", "商業與管理群", "餐旅群")

# 5.1 科系佔比趨勢圖 (Line Chart) - 註解掉未使用
# ggsave("output/figures/market_share_trends_line.png", p1, width = 10, height = 6, dpi = 300)

# 5.2 薪資成長率 vs 佔比成長率 散佈圖 (四象限分析)
plot_data_scatter <- analysis_data %>%
    mutate(
        Highlight = ifelse(群類名稱 %in% key_groups, 群類名稱, "其他"),
        Alpha = ifelse(群類名稱 %in% key_groups, 0.9, 0.4),
        Quadrant = case_when(
            薪資年增率 > 0 & 佔比年增率 > 0 ~ "Q1: 雙贏 (薪資漲/佔比增)",
            薪資年增率 <= 0 & 佔比年增率 > 0 ~ "Q2: 高CP值 (薪資平/佔比增)",
            薪資年增率 <= 0 & 佔比年增率 <= 0 ~ "Q3: 雙輸 (薪資平/佔比減)",
            薪資年增率 > 0 & 佔比年增率 <= 0 ~ "Q4: 錯失良機 (薪資漲/佔比減)"
        )
    )

p2 <- ggplot(plot_data_scatter, aes(x = 薪資年增率, y = 佔比年增率)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = Highlight, alpha = Alpha, size = 總薪資)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray30", linetype = "dotted", alpha = 0.5) +
    geom_text_repel(
        data = subset(plot_data_scatter, 年度 == 113 | abs(佔比年增率) > 0.1 | abs(薪資年增率) > 0.1),
        aes(label = paste0(群類名稱, "(", 年度, ")")),
        size = 3,
        max.overlaps = 50,
        box.padding = 0.5,
        force = 2
    ) +
    scale_color_manual(values = c(
        "電機與電子群資電類" = "#E74C3C", # 紅
        "衛生與護理類" = "#9B59B6", # 紫
        "機械群" = "#F39C12", # 橘
        "商業與管理群" = "#2ECC71", # 綠
        "餐旅群" = "#3498DB", # 藍
        "其他" = "#BDC3C7" # 灰
    )) +
    scale_alpha_identity() +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    labs(
        title = "薪資成長率 vs 市場佔有率成長率 (101-113年)",
        subtitle = "檢視「加薪」是否能帶動「市佔率擴張」",
        x = "薪資年增率 (YoY)",
        y = "報名佔比年增率 (YoY)",
        color = "群類名稱",
        size = "平均月薪"
    ) +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(legend.position = "right")

# 修改檔名以符合章節
ggsave("output/figures/3_3_salary_vs_share_growth.png", p2, width = 10, height = 6, dpi = 300)
cat("已儲存: output/figures/3_3_salary_vs_share_growth.png\n")

cat("--- 步驟 5：圖表已輸出至 output/figures/ ---\n")

# 6. 統計模型分析
# 靜態模型：使用所有年度資料，檢視長期平均關係
model_static <- lm(報名佔比 ~ 總薪資, data = analysis_data)
# 動態模型：使用所有年度成長率資料
model_dynamic <- lm(佔比年增率 ~ 薪資年增率, data = analysis_data)

# 6.1 靜態模型視覺化：薪資 vs 報名佔比散佈圖（含回歸線與重點群類標註）
static_scatter_data <- analysis_data %>%
    group_by(群類名稱) %>%
    summarise(
        平均薪資 = mean(總薪資, na.rm = TRUE),
        平均報名佔比 = mean(報名佔比, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Highlight = ifelse(群類名稱 %in% key_groups, 群類名稱, "其他"))

p_static <- ggplot(static_scatter_data, aes(x = 平均薪資, y = 平均報名佔比)) +
    geom_point(aes(color = Highlight), size = 4, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "#34495E", linetype = "dashed") +
    geom_text_repel(
        data = subset(static_scatter_data, Highlight != "其他"),
        aes(label = 群類名稱),
        size = 4,
        box.padding = 0.5,
        force = 2
    ) +
    scale_color_manual(values = c(
        "電機與電子群資電類" = "#E74C3C",
        "衛生與護理類" = "#9B59B6",
        "機械群" = "#F39C12",
        "商業與管理群" = "#2ECC71",
        "餐旅群" = "#3498DB",
        "其他" = "#BDC3C7"
    )) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
        title = "薪資 vs 報名佔比（靜態模型）",
        subtitle = "各群類平均薪資與平均報名佔比關係，含回歸線",
        x = "平均總薪資（元）",
        y = "平均報名佔比",
        color = "群類名稱"
    ) +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(legend.position = "right")

ggsave("output/figures/3_3_salary_vs_share_static.png", p_static, width = 9, height = 6, dpi = 300)
cat("已儲存: output/figures/3_3_salary_vs_share_static.png\n")

output_file <- "output/proportion_regression_results.txt"
sink(output_file)
cat("======================================================\n")
cat("分析報告：科系報名佔比與薪資之關聯分析 (100-113年)\n")
cat("======================================================\n\n")
cat("1. 靜態模型 (Model Static)\n")
cat("說明：檢視「絕對薪資」與「報名佔比」的關係\n")
print(summary(model_static))
cat("\n------------------------------------------------------\n\n")
cat("2. 動態模型 (Model Dynamic)\n")
cat("說明：檢視「薪資年增率」與「佔比年增率」的關係\n")
print(summary(model_dynamic))
cat("\n------------------------------------------------------\n\n")
cat("3. 資料摘要 (前 10 筆)\n")
print(head(analysis_data, 10))
sink()

cat("--- 步驟 6：統計分析完成，結果已儲存至", output_file, "---\n")
