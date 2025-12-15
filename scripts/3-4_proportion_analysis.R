# scripts/3-4_proportion_analysis.R
#
# 目的：執行 3.4 節分析 - 科系選擇偏好與薪資關聯分析 (Market Share Analysis)
# 說明：將報名人數轉換為當年度佔比，消除少子化總人數下降的影響，聚焦於學生選擇偏好的變化。
# 重點：特別標示五大關鍵產業（電資、醫護、製造、金融、餐飲）。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 設定圖表輸出目錄
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("--- 步驟 1：套件載入成功 ---\n")

# 2. 載入原始資料
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)

cat("--- 步驟 2：原始資料載入成功 ---\n")

# 3. 資料前處理：計算科系佔比 (Market Share)
# --------------------------------------------------

# 3.1 整理報名人數為長格式 (Long Format)
registration_long <- registration_data %>%
    pivot_longer(
        cols = matches("^\\d{3}學年度$"),
        names_to = "學年度_str",
        values_to = "報名人數"
    ) %>%
    mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
    filter(!is.na(報名人數), 群類代號 != "All", between(年度, 109, 113)) %>%
    select(群類名稱, 年度, 報名人數)

# 3.2 計算每年度總人數與各科系佔比
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
# ------------------------------------
mapping_table <- tribble(
    ~行業別, ~群類名稱,
    "住宿及餐飲業", "餐旅群",
    "出版影音及資通訊業", "電機與電子群資電類",
    "金融及保險業", "商業與管理群",
    "製造業", "機械群",
    "製造業", "動力機械群",
    "營建工程業", "土木與建築群",
    "醫療保健及社會工作服務業", "衛生與護理類",
    "藝術、娛樂及休閒服務業", "藝術群影視類", # 修正名稱
    "製造業", "化工群"
)

# 4.1 準備薪資資料
industry_salary <- salary_data %>%
    filter(行業別 != "工業及服務業總計", between(年度, 109, 113)) %>%
    select(行業別, 年度, 總薪資, `總薪資年增率(%)`)

# 4.2 合併資料
# 注意：這裡只保留有對應到產業的科系進行分析
analysis_data <- registration_proportion %>%
    inner_join(mapping_table, by = "群類名稱") %>%
    inner_join(industry_salary, by = c("行業別", "年度"))

cat("--- 步驟 4：資料合併完成 (僅包含有對應產業之科系) ---\n")

# 5. 視覺化分析
# ------------------------------------

# 定義五大關鍵產業群組
key_groups <- c("電機與電子群資電類", "衛生與護理類", "機械群", "商業與管理群", "餐旅群")

# 5.1 科系佔比趨勢圖 (Line Chart) - 強調五大產業
plot_data_trend <- analysis_data %>%
    mutate(
        Highlight = ifelse(群類名稱 %in% key_groups, 群類名稱, "其他"),
        Alpha = ifelse(群類名稱 %in% key_groups, 1, 0.3),
        Size = ifelse(群類名稱 %in% key_groups, 1.5, 0.8)
    )

p1 <- ggplot(plot_data_trend, aes(x = 年度, y = 報名佔比百分比, color = Highlight, group = 群類名稱)) +
    geom_line(aes(alpha = Alpha, size = Size)) +
    geom_point(aes(alpha = Alpha), size = 2) +
    scale_color_manual(values = c(
        "電機與電子群資電類" = "#E74C3C", # 紅
        "衛生與護理類" = "#9B59B6", # 紫
        "機械群" = "#F39C12", # 橘
        "商業與管理群" = "#2ECC71", # 綠
        "餐旅群" = "#3498DB", # 藍
        "其他" = "#BDC3C7" # 灰
    )) +
    scale_alpha_identity() +
    scale_size_identity() +
    labs(
        title = "歷年各群類報名佔比趨勢 (Market Share)",
        subtitle = "排除少子化影響，觀察學生選擇偏好變化 (五大關鍵產業)",
        x = "學年度",
        y = "佔當年度總報名人數比例 (%)",
        color = "群類名稱"
    ) +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(legend.position = "right")

ggsave("output/figures/market_share_trends_line.png", p1, width = 10, height = 6, dpi = 300)
cat("已儲存: output/figures/market_share_trends_line.png\n")

# 5.2 薪資 vs 佔比 散佈圖 - 強調五大產業
plot_data_scatter <- analysis_data %>%
    mutate(
        Highlight = ifelse(群類名稱 %in% key_groups, 群類名稱, "其他"),
        Alpha = ifelse(群類名稱 %in% key_groups, 0.9, 0.4)
    )

p2 <- ggplot(plot_data_scatter, aes(x = 總薪資, y = 報名佔比百分比)) +
    geom_point(aes(color = Highlight, alpha = Alpha), size = 4) +
    geom_smooth(method = "lm", se = TRUE, color = "gray30", linetype = "dashed", alpha = 0.2) +
    geom_text(aes(label = 年度), vjust = -0.8, size = 3, check_overlap = FALSE) + # 加入年份標籤
    scale_color_manual(values = c(
        "電機與電子群資電類" = "#E74C3C", # 紅
        "衛生與護理類" = "#9B59B6", # 紫
        "機械群" = "#F39C12", # 橘
        "商業與管理群" = "#2ECC71", # 綠
        "餐旅群" = "#3498DB", # 藍
        "其他" = "#BDC3C7" # 灰
    )) +
    scale_alpha_identity() +
    scale_x_continuous(labels = comma) +
    labs(
        title = "產業平均薪資 vs 科系報名佔比",
        subtitle = "檢視高薪產業是否吸引更高比例的學生",
        x = "產業平均總薪資 (元)",
        y = "科系報名佔比 (%)",
        color = "群類名稱"
    ) +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(legend.position = "right")

ggsave("output/figures/salary_vs_share_scatter.png", p2, width = 10, height = 6, dpi = 300)
cat("已儲存: output/figures/salary_vs_share_scatter.png\n")

cat("--- 步驟 5：圖表已輸出至 output/figures/ ---\n")

# 6. 統計模型分析
# ------------------------------------

# 6.1 計算變數的年增率 (針對佔比)
regression_data <- analysis_data %>%
    group_by(群類名稱) %>%
    arrange(年度) %>%
    mutate(
        佔比年增率 = (報名佔比 - lag(報名佔比)) / lag(報名佔比),
        薪資年增率 = (總薪資 - lag(總薪資)) / lag(總薪資)
    ) %>%
    ungroup() %>%
    filter(!is.na(佔比年增率))

# 6.2 模型 A: 靜態模型 (佔比 ~ 薪資)
# 檢驗：薪資越高的產業，其對應科系的佔比是否越高？
model_static <- lm(報名佔比 ~ 總薪資, data = analysis_data)

# 6.3 模型 B: 動態模型 (佔比成長 ~ 薪資成長)
# 檢驗：薪資漲幅越大的產業，其對應科系的佔比是否成長越快？
model_dynamic <- lm(佔比年增率 ~ 薪資年增率, data = regression_data)

# 6.4 輸出統計結果
output_file <- "output/proportion_regression_results.txt"
sink(output_file)

cat("======================================================\n")
cat("分析報告：科系報名佔比與薪資之關聯分析\n")
cat("======================================================\n\n")

cat("1. 靜態模型 (Model Static)\n")
cat("   依變數：科系報名佔比 (Market Share)\n")
cat("   自變數：產業總薪資\n")
cat("   解釋：測試「高薪」是否對應「高市佔率」\n")
print(summary(model_static))
cat("\n------------------------------------------------------\n\n")

cat("2. 動態模型 (Model Dynamic)\n")
cat("   依變數：科系佔比年增率 (Share Growth Rate)\n")
cat("   自變數：薪資年增率 (Salary Growth Rate)\n")
cat("   解釋：測試「加薪」是否帶動「市佔率擴張」\n")
print(summary(model_dynamic))
cat("\n------------------------------------------------------\n\n")

cat("3. 資料摘要 (前 10 筆)\n")
print(head(regression_data, 10))

sink()

cat("--- 步驟 6：統計分析完成，結果已儲存至", output_file, "---\n")
