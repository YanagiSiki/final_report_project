# scripts/3-3_stability_analysis.R
#
# 目標：3.3 薪資的穩定效應：抗跌性分析
# 產出：群類平均表現散佈圖 (Aggregation Plot)
# -----------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

# 建立輸出目錄
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 1. 載入資料
salary_data <- read_csv("data/salary_data_100_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_100_114.csv", show_col_types = FALSE)

# 2. 資料處理
# 2.1 薪資資料 (取 100-113 年平均)
industry_salary <- salary_data %>%
    filter(類別 == "經常性薪資", 業別 != "工業及服務業總計", between(年度, 100, 113)) %>%
    group_by(業別) %>%
    summarise(平均薪資 = mean(值, na.rm = TRUE))

# 2.2 報名人數資料 (計算年增率後取平均)
registration_growth <- registration_data %>%
    pivot_longer(cols = matches("^\\d{3}學年度$"), names_to = "學年度_str", values_to = "報名人數") %>%
    mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
    filter(!is.na(報名人數), 群類代號 != "All", between(年度, 100, 113)) %>%
    group_by(群類名稱) %>%
    arrange(年度) %>%
    mutate(
        年增率 = (報名人數 - lag(報名人數)) / lag(報名人數)
    ) %>%
    filter(!is.na(年增率)) %>%
    summarise(平均年增率 = mean(年增率, na.rm = TRUE))

# 2.3 建立對應表
mapping_table <- tribble(
    ~行業別, ~群類名稱,
    "住宿及餐飲業", "餐旅群",
    "出版影音及資通訊業", "電機與電子群資電類",
    "金融及保險業", "商業與管理群",
    "製造業", "機械群",
    "製造業", "動力機械群",
    "營建工程業", "土木與建築群",
    "醫療保健及社會工作服務業", "衛生與護理類",
    "教育業", "外語群英語類",
    "教育業", "外語群日語類",
    "專業科學及技術服務業", "設計群",
    "農林漁牧業", "農業群",
    "製造業", "食品群",
    "其他服務業", "家政群生活應用類",
    "醫療保健及社會工作服務業", "家政群幼保類",
    "運輸及倉儲業", "海事群",
    "製造業", "化工群",
    "藝術娛樂及休閒服務業", "藝術群影視類"
)

# 3. 合併資料
plot_data <- mapping_table %>%
    left_join(industry_salary, by = c("行業別" = "業別")) %>%
    left_join(registration_growth, by = "群類名稱") %>%
    filter(!is.na(平均薪資), !is.na(平均年增率))

# 4. 繪圖：群類平均表現散佈圖
p <- ggplot(plot_data, aes(x = 平均薪資, y = 平均年增率)) +
    geom_smooth(method = "lm", color = "lightgray", se = FALSE, linetype = "dashed") +
    geom_point(aes(color = 平均薪資), size = 4) +
    geom_text_repel(aes(label = 群類名稱), size = 3.5, box.padding = 0.5) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_c(option = "D", direction = -1) +
    labs(
        title = "群類平均表現散佈圖 (100-113年)",
        subtitle = "高薪資群類展現較強的抗跌性 (平均年增率較高)",
        x = "14年平均經常性薪資 (元)",
        y = "14年平均報名人數年增率 (%)",
        caption = "資料來源：教育部統計處、勞動部"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("output/figures/3-3_stability_aggregation.png", p, width = 8, height = 6)
cat("圖表已輸出至 output/figures/3-3_stability_aggregation.png\n")
