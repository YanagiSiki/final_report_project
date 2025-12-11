# scripts/3-3_analysis.R
#
# 更新 v8: 修改 print 指令，使其能顯示所有資料列。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(stringr)

cat("--- 步驟 1：套件載入成功 ---

")


# 2. 載入原始資料
salary_data <- read_csv("data/salary_data_109_113.csv", show_col_types = FALSE)
registration_data <- read_csv("data/tcte_registration_109_114.csv", show_col_types = FALSE)
birth_data <- read_csv("data/tcte_birth_cohort_statistics_109_113.csv", show_col_types = FALSE)
# 2a. 計算出生人口年增率
birth_growth <- birth_data %>%
  select(統測學年度, 該年出生人數) %>%
  arrange(統測學年度) %>%
  mutate(
    出生人口年增率 = (該年出生人數 - lag(該年出生人數)) / lag(該年出生人數)
  ) %>%
  rename(年度 = 統測學年度) %>%
  select(年度, 出生人口年增率)


cat("--- 步驟 2：原始資料載入成功 ---

")


# 3. 準備年度資料
# ---------------------------
# a. 準備歷年行業薪資資料
industry_salary_annual <- salary_data %>%
  filter(行業別 != "工業及服務業總計", between(年度, 109, 113)) %>%
  select(行業別, 年度, 總薪資)

# b. 準備歷年科系報名人數資料
registration_annual <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"),
    names_to = "學年度_str",
    values_to = "報名人數"
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>%
  filter(!is.na(報名人數), 群類代號 != "All", between(年度, 109, 113)) %>%
  select(群類名稱, 年度, 報名人數)

cat("--- 步驟 3：已準備好歷年薪資與報名人數資料 ---

")


# 4. 建立對應表並合併資料
# ------------------------------------
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
  "製造業", "化工群"
)

# 進行合併
merged_annual_data <- left_join(registration_annual, mapping_table_final, by = "群類名稱") %>%
  inner_join(industry_salary_annual, by = c("行業別", "年度"))

cat("--- 步驟 4：已合併年度資料 ---

")



# 5. 計算年增率並合併出生人口年增率，儲存 CSV
# --------------------------------
analysis_data_final <- merged_annual_data %>%
  group_by(行業別, 群類名稱) %>%
  arrange(年度, .by_group = TRUE) %>%
  mutate(
    薪資年增率 = (總薪資 - lag(總薪資)) / lag(總薪資),
    報名人數年增率 = (報名人數 - lag(報名人數)) / lag(報名人數)
  ) %>%
  ungroup() %>%
  filter(!is.na(薪資年增率) & !is.na(報名人數年增率)) %>%
  left_join(birth_growth, by = "年度")

cat("--- 步驟 5a：已計算年增率 ---
")
# 【關鍵修正】使用 print(n = Inf) 來顯示所有資料列
print(analysis_data_final, n = Inf)
cat("\n")

# 儲存修正後的最終分析資料
output_csv_path <- "data/final_analysis_data_for_3-3.csv"
write.csv(
  analysis_data_final,
  output_csv_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
cat(paste("--- 步驟 5b：已將包含年增率的分析資料儲存至", output_csv_path, "---
\n"))


# 6. 產生視覺化圖表
# ------------------------------------------
cat("--- 步驟 6：產生年增率對比的散佈圖 (象限圖) ---
")

if (nrow(analysis_data_final) > 0) {
  # 計算平均值作為象限分割線 (或者使用 0 作為分割線)
  # 這裡使用 0 作為分割線，因為我們看的是"成長"與"衰退"
  x_mid <- 0
  y_mid <- 0

  p_quadrant <- ggplot(analysis_data_final, aes(x = 薪資年增率, y = 報名人數年增率)) +
    # 加入象限背景色 (可選，這裡用線條區隔保持簡潔)
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +

    # 繪製點
    geom_point(aes(color = 群類名稱, shape = as.factor(年度)), size = 4, alpha = 0.8) +

    # 加入趨勢線
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid", size = 0.8) +

    # 加入象限標籤
    annotate("text", x = max(analysis_data_final$薪資年增率, na.rm = TRUE), y = max(analysis_data_final$報名人數年增率, na.rm = TRUE), label = "高薪資成長\n人數正成長", hjust = 1, vjust = 1, color = "gray40", size = 3) +
    annotate("text", x = max(analysis_data_final$薪資年增率, na.rm = TRUE), y = min(analysis_data_final$報名人數年增率, na.rm = TRUE), label = "高薪資成長\n人數負成長\n(少子化衝擊區)", hjust = 1, vjust = 0, color = "gray40", size = 3) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "圖三、行業薪資與群類人數變動率散佈圖 (Quadrant Chart)",
      subtitle = "多數科系落入第四象限（薪資漲、人數跌），顯示少子化推力大於薪資拉力",
      x = "行業平均總薪資年增率 (%)",
      y = "統測群類報名人數年增率 (%)",
      color = "統測群類",
      shape = "年度",
      caption = "註：虛線代表零成長界線；紅線為線性迴歸趨勢線"
    ) +
    theme_minimal(base_family = "Microsoft JhengHei") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 16, face = "bold")
    )

  # 儲存圖表
  ggsave("output/figures/3_3_quadrant_chart.svg", plot = p_quadrant, width = 10, height = 7)
  print("已儲存: output/figures/3_3_quadrant_chart.svg")
} else {
  cat("--- 步驟 6：錯誤！沒有資料可用於繪圖。---
")
}

cat("\n腳本執行完畢。\n")
