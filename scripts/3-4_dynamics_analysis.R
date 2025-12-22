# scripts/3-4_dynamics_analysis.R
#
# 本腳本依據 AGENT.md 專案規格執行，所有變數對應、指標定義、分析流程、圖表標題與註解皆嚴格遵循 AGENT.md。
# 目標：3.4 薪資的磁吸效應：市佔率流動
# 產出：
# 1. 產業軌跡圖（依 AGENT.md 4.3 Pull Factor 驗證）
# 2. 3年週期迴歸圖（請見 scripts/3-4_3year_cycle_analysis.R）
# -----------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 1. 載入資料
salary_data <- read_csv("data/salary_data_100_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_100_114.csv", show_col_types = FALSE)

# 2. 資料處理
# 2.1 薪資資料
# 依 AGENT.md 3.1 產業對應表與 3.2 指標定義
industry_salary <- salary_data %>%
    filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", between(年度, 100, 113)) %>%
    select(行業別 = 業別, 年度, 薪資 = 值)

# 2.2 報名佔比資料
# 市佔率（Market Share）計算公式依 AGENT.md 3.2：
# $\text{Market Share} = \frac{\text{Group Registration}}{\text{Total Registration}}$
registration_share <- registration_data %>%
    pivot_longer(cols = matches("^\\d{3}學年度$"), names_to = "學年度_str", values_to = "報名人數") %>%
    mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
    filter(!is.na(報名人數), 群類代號 != "All", between(年度, 100, 113)) %>%
    group_by(年度) %>%
    mutate(總報名人數 = sum(報名人數), 市佔率 = 報名人數 / 總報名人數) %>%
    ungroup() %>%
    select(群類名稱, 年度, 市佔率)

# 2.3 對應表（依 AGENT.md 3.1 變數對應表，嚴格一致）
mapping_table <- tribble(
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

# 2.4 合併資料
# 僅保留 AGENT.md 3.1 對應群類與產業
merged_data <- mapping_table %>%
    left_join(industry_salary, by = "行業別") %>%
    left_join(registration_share, by = c("群類名稱", "年度")) %>%
    filter(!is.na(薪資), !is.na(市佔率))

# 2.5 年增率（Growth Rate）計算（依 AGENT.md 3.2）
# $\text{Growth Rate} = \frac{V_t - V_{t-1}}{V_{t-1}}$
# 這裡以市佔率為例，計算每群類每年度市佔率年增率
growth_data <- merged_data %>%
    group_by(群類名稱) %>%
    arrange(年度) %>%
    mutate(市佔率年增率 = (市佔率 - lag(市佔率)) / lag(市佔率)) %>%
    ungroup()

# -----------------------------------------------------------------------------
# 圖表 1：產業軌跡圖（依 AGENT.md 4.3 Pull Factor 驗證）
# -----------------------------------------------------------------------------

# 篩選起點（100年）與終點（113年）
trajectory_data <- merged_data %>%
    filter(年度 %in% c(100, 113)) %>%
    arrange(群類名稱, 年度)

# 繪圖
p1 <- ggplot(trajectory_data, aes(x = 薪資, y = 市佔率, group = 群類名稱, color = 群類名稱)) +
    geom_path(arrow = arrow(length = unit(0.3, "cm"), type = "closed"), size = 1, alpha = 0.7) +
    geom_point(size = 2) +
    geom_text_repel(
        data = subset(trajectory_data, 年度 == 113),
        aes(label = 群類名稱),
        size = 3,
        nudge_x = 1000,
        box.padding = 0.5
    ) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = percent) +
    labs(
        title = "產業軌跡圖（100年→113年）",
        subtitle = "箭頭顯示從 100 年到 113 年的移動方向：右上方代表「薪資漲、市佔漲」",
        x = "經常性薪資（元）",
        y = "報名人數市佔率（%）",
        caption = "資料來源：教育部統計處、勞動部；分析依 AGENT.md Pull Factor 假說"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("output/figures/3-4_dynamics_trajectory.png", p1, width = 10, height = 7)
cat("圖表 1 已輸出至 output/figures/3-4_dynamics_trajectory.png\n")

# -----------------------------------------------------------------------------
# 圖表 2：3年週期迴歸圖請見 scripts/3-4_3year_cycle_analysis.R，並依 AGENT.md 4.3 Pull Factor 動態驗證規範執行。
# -----------------------------------------------------------------------------
