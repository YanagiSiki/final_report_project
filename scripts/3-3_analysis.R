# scripts/3-3_analysis.R
#
# 目標：執行更深層的相關性與迴歸分析，探討「行業薪資變化」與「對應科系報名人數變化」的關係。
#
# 腳本將執行以下操作：
# 1. 載入並準備行業薪資與科系報名人數的細分資料。
# 2. **建立一個合理的「行業別」與「科系群類」對應表 (Mapping Table)。**
# 3. **根據對應表合併兩個資料集。**
# 4. **執行新的相關性與迴歸分析。**
# 5. **產生新的視覺化圖表 (散佈圖) 並儲存。**
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
# ---------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)

cat("步驟 1：套件載入成功。\n")

# 2. 載入原始資料 (保留細節)
# ---------------------------
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)

cat("步驟 2：原始資料載入成功。\n")

# 3. 準備行業薪資資料
# ---------------------------------
industry_salary_prepared <- salary_data %>%
  filter(行業別 != "工業及服務業總計") %>%
  group_by(行業別, 年度) %>%
  summarise(
    行業平均總薪資 = mean(總薪資, na.rm = TRUE),
    .groups = "drop"
  )

cat("步驟 3：已準備好各行業別的薪資數據。\n")


# 4. 準備科系群類報名人數資料
# ---------------------------------
registration_group_prepared <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All") %>%
  group_by(群類名稱, 年度) %>%
  summarise(
    群類總報名人數 = sum(報名人數, na.rm = TRUE),
    .groups = "drop"
  )

cat("步驟 4：已準備好各科系群類的報名人數數據。\n")

# 5. 建立對應表並合併資料
# ---------------------------------
# 這是關鍵步驟。我們手動建立一個合理的對應關係。
# 注意：這是一個基於名稱的簡化對應，實際研究可能需要更嚴謹的分類。
mapping_table <- tribble(
  ~行業別, ~群類名稱,
  "出版、影音製作、程式設計及資通訊服務業", "電機與電子群資電類",
  "藝術、娛樂及休閒服務業", "設計群",
  "住宿及餐飲業", "餐旅群",
  "金融及保險業", "商業與管理群",
  "批發及零售業", "商業與管理群",
  "製造業", "機械群",
  "營建工程業", "土木與建築群"
)

# 進行合併
analysis_data <- inner_join(mapping_table, industry_salary_prepared, by = "行業別") %>%
  inner_join(registration_group_prepared, by = c("群類名稱", "年度"))

# 計算年增率 (在合併後計算，確保數據對齊)
analysis_data_final <- analysis_data %>%
  group_by(行業別, 群類名稱) %>%
  arrange(年度) %>%
  mutate(
    薪資年增率 = (行業平均總薪資 - lag(行業平均總薪資)) / lag(行業平均總薪資),
    報名人數年增率 = (群類總報名人數 - lag(群類總報名人數)) / lag(群類總報名人數)
  ) %>%
  filter(!is.na(薪資年增率) & !is.na(報名人數年增率)) # 移除無法計算年增率的資料

cat("步驟 5：已建立對應表並成功合併資料。\n")
cat("--- 合併後的最終分析資料 (前5行) ---
")
print(head(analysis_data_final))
cat("------------------------------------
")


# 6. 執行新的相關性與迴歸分析
# ---------------------------------
cat("\n步驟 6：執行新的相關性與迴歸分析。\n")

# 相關性分析
# 我們需要處理可能只有一個觀測點的情況，這會導致 cor.test 失敗
if (nrow(analysis_data_final) > 2) {
    correlation_test <- cor.test(analysis_data_final$薪資年增率, analysis_data_final$報名人數年增率)
    cat("--- 相關性分析結果 ---
")
    print(correlation_test)
    cat("------------------------
")

    # 迴歸分析
    regression_model <- lm(報名人數年增率 ~ 薪資年增率, data = analysis_data_final)
    cat("--- 迴歸分析結果 ---
")
    print(summary(regression_model))
    cat("----------------------
")
} else {
    cat("資料不足，無法執行相關性與迴歸分析 (需要至少3個觀測點)。\n")
}


# 7. 產生新的視覺化圖表 (散佈圖)
# ---------------------------------
# 只有在有足夠數據時才繪圖
if (nrow(analysis_data_final) > 0) {
    scatter_plot <- ggplot(analysis_data_final, aes(x = 薪資年增率, y = 報名人數年增率)) + 
      geom_point(aes(color = 行業別, size = 行業平均總薪資 / 10000), alpha = 0.7) + 
      geom_smooth(method = "lm", se = FALSE, color = "firebrick") + 
      geom_text(aes(label = paste(stringr::str_trunc(行業別, 10), 年度, sep="\n")), vjust = -1, size = 2.5, check_overlap = TRUE) + 
      scale_x_continuous(labels = scales::percent) + 
      scale_y_continuous(labels = scales::percent) + 
      labs(
        title = "行業薪資年增率 vs 對應科系報名人數年增率",
        subtitle = "每個點代表一個行業在某年度的表現，點的大小反映該年薪資水平",
        x = "行業平均總薪資年增率",
        y = "對應科系總報名人數年增率",
        size = "薪資水平 (萬)",
        color = "行業別"
      ) + 
      theme_minimal(base_family = "sans") + 
      theme(legend.position = "bottom")

    # 儲存圖表
    output_path <- "output_chapters/images/3_3_industry_level_analysis.svg"
    ggsave(output_path, plot = scatter_plot, width = 12, height = 9)

    cat(paste("\n步驟 7：新的散佈圖已儲存至:", output_path, "\n"))
} else {
    cat("\n步驟 7：無足夠資料可繪製圖表。\n")
}

cat("\n腳本執行完畢。\n")
#
# 注意：腳本中增加了對 `stringr::str_trunc` 的使用，
# 如果您尚未安裝 `stringr` 套件，請執行 `install.packages('stringr')`。
# 如果出現錯誤，可將 geom_text 中的 `stringr::str_trunc(行業別, 10)` 改回 `行業別`。