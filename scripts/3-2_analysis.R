# 3-2_analysis.R
#
# 本腳本旨在比較不同行業別的薪資水平，並產生對應的視覺化圖表。
#
# 分析流程:
# 1. 載入必要的 R 套件。
# 2. 讀取薪資資料。
# 3. 資料前處理：選取特定年份 (113年) 的資料。
# 4. 產生視覺化圖表：為不同行業別的薪資繪製長條圖 (Bar Chart)。
#
# ---

# 1. 載入套件
# 如果尚未安裝，請執行 install.packages("套件名稱")
library(readr)
library(dplyr)
library(ggplot2)
library(svglite)

# ---

# 2. 讀取資料
# 假設 R 的工作目錄是專案根目錄 (final_report_project)
file_path <- "data/salary_data_109_113.csv"
salary_data <- read_csv(file_path, show_col_types = FALSE)

# ---

# 3. 資料前處理
# 我們將只專注於 113 年的資料來進行橫向比較
salary_113 <- salary_data %>%
  filter(年度 == 113) %>%
  select(industry = 行業別, salary = 總薪資) %>%
  na.omit()

# ---

# 4. 產生視覺化圖表 (長條圖)

# 為了讓圖表更易讀，我們根據薪資高低對行業別進行排序
barchart <- ggplot(salary_113, aes(x = reorder(industry, salary), y = salary)) + 
  geom_col(aes(fill = industry), show.legend = FALSE) + # 使用 geom_col 繪製長條圖
  coord_flip() + # 將 X Y 軸翻轉，讓行業標籤更清晰
  labs(
    title = "113年 各行業別薪資比較",
    x = "行業別",
    y = "總薪資 (元)"
  ) + 
  theme_minimal(base_family = "sans") + # 使用簡潔主題
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # 標題置中
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12)
  )

# 顯示圖表
print(barchart)

# 將圖表儲存到 output_chapters/images/
# 注意：檔名已更改
output_path_svg <- "output_chapters/images/3_2_salary_barchart.svg"
svglite(output_path_svg, width = 10, height = 8)
print(barchart)
dev.off()

cat("長條圖已成功儲存至:", output_path_svg, "\n")

# --- 腳本結束 ---