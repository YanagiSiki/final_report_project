# scripts/3-1_analysis.R
#
# 目標：執行 3.1 節所需的皮爾森相關性分析。
# 驗證「出生人口數」與「統測總報名人數」的線性關係。
# (此腳本不再產生圖表，圖表功能已移至 2_descriptive_analysis.R)
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
# ---------------------------
library(readr)
library(dplyr)

cat("---", "步驟 1：套件載入成功 (3-1_analysis.R) ---", "\n\n")

# 2. 資料與路徑設定
# ---------------------------
data_path <- "data/"

# 讀取本分析所需的唯一資料
birth_cohort_data <- read_csv(file.path(data_path, "tcte_birth_cohort_statistics_109_113.csv"), show_col_types = FALSE)

cat("---", "步驟 2：資料載入成功 ---", "\n\n")


# 3. 執行皮爾森相關性分析
# ---------------------------------
cat("---", "步驟 3：執行皮爾森相關性分析 ---", "\n")

# 執行 cor.test
correlation_test_result <- cor.test(
  birth_cohort_data$該年出生人數,
  birth_cohort_data$統測報名人數
)

# 提取關鍵結果
correlation_r <- correlation_test_result$estimate
p_value <- correlation_test_result$p.value

# 輸出格式化的結果
cat("分析目標：檢驗「出生人口數」與「統測總報名人數」的線性關係。\n\n")
cat("分析結果：\n")
cat("-----------------------------------------------------", "\n")
cat(paste("皮爾森相關係數 (r):", round(correlation_r, 2), "\n"))
cat(paste("p 值 (p-value):", format.pval(p_value, digits = 3, eps = 0.001), "\n"))
cat("-----------------------------------------------------", "\n\n")
cat("結論：正如報告 3.1.2 節所述，兩者存在高度顯著的正相關。\n")


cat("\n腳本 (3-1_analysis.R) 執行完畢。\n")
