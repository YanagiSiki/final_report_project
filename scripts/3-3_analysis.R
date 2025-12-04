# scripts/3-3_analysis.R
#
# 目標：根據報告 3.3 節的描述，執行相關性與迴歸分析。
# 腳本將執行以下操作：
# 1. 載入薪資、技專校院報名人數、與出生世代等多個資料集。
# 2. 按「年份」將資料進行匯總。
# 3. 合併資料集。
# 4. 計算並視覺化相關係數矩陣。
# 5. 建立並摘要一個簡單的線性迴歸模型。
#
# 注意：本腳本已根據實際 CSV 檔案欄位名稱 (如'年度', '總薪資' 等) 進行修正。
# -----------------------------------------------------------------------------

# 1. 載入必要套件
# ---------------------------
# 確保在執行前已安裝好這些套件 (如果尚未安裝，請使用 install.packages("套件名稱"))
library(readr)
library(dplyr)
library(tidyr) # 新增 tidyr 用於 pivot_longer
library(corrplot)
library(svglite)

cat("步驟 1：套件載入成功.\n")

# 2. 載入資料
# ---------------------------
# 使用 read_csv 讀取資料，它能較好地處理 UTF-8 編碼
salary_data <- read_csv("data/salary_data_109_113.csv")
registration_data <- read_csv("data/tcte_registration_109_114.csv")
birth_data <- read_csv("data/tcte_birth_cohort_statistics_109_113.csv")

cat("步驟 2：原始資料載入成功.\n")

# 3. 資料前處理與匯總
# ---------------------------------
# 根據實際欄位名稱修正
# 匯總薪資資料：計算每年全國平均總薪資
salary_annual <- salary_data %>%
  group_by(年度) %>%
  summarise(avg_total_salary = mean(總薪資, na.rm = TRUE)) # 使用 '總薪資'

# 匯總報名資料：從寬格式轉換為長格式，並計算每年總報名人數
registration_annual <- registration_data %>%
  pivot_longer(
    cols = matches("^\\d{3}學年度$"), # 匹配 '109學年度' 這樣的欄位
    names_to = "學年度_str",          # 新的欄位名稱為 '學年度_str'
    values_to = "registration_count"  # 新的欄位名稱為 'registration_count'
  ) %>%
  mutate(年度 = as.numeric(gsub("學年度", "", 學年度_str))) %>% # 從 '109學年度' 提取 '109' 並轉為數值
  group_by(年度) %>%
  summarise(total_registrations = sum(registration_count, na.rm = TRUE))

# 出生人口資料：選取正確欄位，並將 '統測學年度' 重新命名為 '年度'
birth_annual <- birth_data %>%
  select(年度 = 統測學年度, birth_count = 該年出生人數) # 使用 '統測學年度' 和 '該年出生人數'

cat("步驟 3：資料已按年份匯總.\n")

# 4. 合併資料
# ----------------
# 透過 inner_join 將三個資料集依 '年度' 合併
analysis_data <- salary_annual %>%
  inner_join(registration_annual, by = "年度") %>%
  inner_join(birth_annual, by = "年度")

# 為了統一後續操作，將 '年度' 欄位名稱重新命名為 'year' (此為分析用內部名稱)
analysis_data_final <- analysis_data %>%
  rename(year = 年度)

# 為了圖表與報告美觀，建立一個具中文名稱的資料框供相關性分析與迴歸結果顯示
analysis_data_renamed_for_display <- analysis_data_final %>%
  rename(
    "平均總薪資" = avg_total_salary,
    "總報名人數" = total_registrations,
    "總出生人口" = birth_count
  )

cat("步驟 4：資料集合併成功.\n")
cat("--- 合併後用於分析的資料 (內部名稱) ---
")
print(head(analysis_data_final)) # 印出合併後的原始資料以供參考
cat("-------------------------------
")


# 5. 相關性分析
# -------------------------
# 從合併後的資料中選取數值欄位進行相關性分析，並排除 'year'
cor_data <- analysis_data_renamed_for_display %>% # 使用為顯示目的重命名的資料框
  select_if(is.numeric) %>%
  select(-year) # 排除 'year' 欄位

# 計算相關係數矩陣 (欄位名稱已在 analysis_data_renamed_for_display 中文命名)
cor_matrix <- cor(cor_data, use = "complete.obs")

cat("\n步驟 5：相關性分析.\n")
cat("--- 相關係數矩陣 ---
")
print(round(cor_matrix, 2))
cat("------------------------
")

cat("\n--- 相關性檢定 (p-values) ---\n")
cor_test1 <- cor.test(analysis_data_renamed_for_display$`平均總薪資`, analysis_data_renamed_for_display$`總出生人口`)
cat("平均總薪資 vs 總出生人口: p-value =", cor_test1$p.value, "\n")

cor_test2 <- cor.test(analysis_data_renamed_for_display$`平均總薪資`, analysis_data_renamed_for_display$`總報名人數`)
cat("平均總薪資 vs 總報名人數: p-value =", cor_test2$p.value, "\n")

cor_test3 <- cor.test(analysis_data_renamed_for_display$`總出生人口`, analysis_data_renamed_for_display$`總報名人數`)
cat("總出生人口 vs 總報名人數: p-value =", cor_test3$p.value, "\n")
cat("-------------------------------\n")

# 視覺化矩陣並儲存為 SVG 檔案
output_path_cor <- "output_chapters/images/3_3_correlation_matrix.svg"
dir.create(dirname(output_path_cor), showWarnings = FALSE, recursive = TRUE) # 確保目錄存在
svglite(file = output_path_cor, width = 8, height = 8)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         title = "薪資、報名人數與出生人口之相關性分析",
         mar = c(0, 0, 1, 0))
dev.off()

cat(paste("\n相關係數矩陣圖已儲存至:", output_path_cor, "\n"))


# 6. 迴歸分析
# ------------------------
cat("\n步驟 6：迴歸分析.\n")
cat("--------------------------------------------------\n")
cat("線性模型：以總報名人數預測平均總薪資\n")
cat("--------------------------------------------------\n")

# 建立線性模型，使用為顯示目的重命名的資料框
lm_model <- lm(`平均總薪資` ~ `總報名人數`, data = analysis_data_renamed_for_display)

# 印出模型摘要
summary_lm <- summary(lm_model)
print(summary_lm)

cat("\n腳本執行完畢.\n")