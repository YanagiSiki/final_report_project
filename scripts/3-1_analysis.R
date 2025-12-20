# scripts/3-1_analysis.R
#
# 目標：執行 3.1 節所需的皮爾森相關性分析與視覺化。
# 驗證「出生人口數」與「統測總報名人數」的線性關係。
#
# -----------------------------------------------------------------------------

# 1. 載入必要套件
# ---------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

cat("---", "步驟 1：套件載入成功 (3-1_analysis.R) ---", "\n\n")

# 2. 資料與路徑設定
# ---------------------------
data_path <- "data/"
output_path <- "output/figures/"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# 讀取本分析所需的唯一資料
birth_cohort_data <- read_csv(file.path(data_path, "tcte_birth_cohort_statistics_100_113.csv"), show_col_types = FALSE)

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

# 4. 視覺化：出生人數 vs 報名人數趨勢 (3_1_birth_reg_trend.svg)
# --------------------------------------------------------------
cat("--- 步驟 4：生成 3_1_birth_reg_trend.svg ---\n")
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
  ) +
  theme_minimal(base_family = "sans")

ggsave(
  filename = file.path(output_path, "3_1_birth_reg_trend.svg"),
  plot = birth_reg_plot,
  width = 8, height = 5
)
cat("--- 3_1_birth_reg_trend.svg 已儲存 ---\n\n")

cat("\n腳本 (3-1_analysis.R) 執行完畢。\n")
