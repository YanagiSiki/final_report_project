# 「統計基礎及 R 語言應用」期末報告專案

本專案為「統計基礎及 R 語言應用」課程的期末報告。主要目標是利用提供的數據集進行統計分析，探討不同變數之間的關聯性。

## 目錄結構

- `data/`: 存放原始數據集 (CSV 檔案)。
- `output_chapters/`: 存放最終報告的各個章節輸出檔案。
- `prompt_master_outline.md`: 定義報告大綱及各章節分析方向的提示文件。
- `README.md`: 專案說明文件（本文件）。

## 數據集

本專案使用以下數據集：

- `salary_data_109_113.csv`: 109 年至 113 年薪資相關數據。
- `tcte_birth_cohort_statistics_109_113.csv`: 109 年至 113 年出生世代統計數據。
- `tcte_registration_109_114.csv`: 109 年至 114 年登記相關數據。

---

### 如何產生/更新所有圖表

本專案中的所有視覺化圖表均由 R 腳本自動產生。

**需求:**

- 已安裝 R 環境 (R version 4.0.0 或以上)。
- 已安裝 R 的相關套件。

**執行步驟:**

1.  **安裝套件:**
    開啟 R 環境，並執行以下指令來安裝所有必要的套件：

    ```R
    install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "svglite", "gghighlight", "GGally"))
    ```

2.  **執行腳本:**
    在專案的根目錄下，開啟終端機（Command Prompt, PowerShell, Terminal），並執行以下指令：

    ```bash
    Rscript scripts/3-1_analysis.R
    ```

    > **注意:** 如果系統提示 `Rscript` 不是可辨識的指令，代表 R 的安裝路徑尚未加入到系統的環境變數 `PATH` 中。請將 R 的 `bin` 資料夾路徑（例如 `C:\Program Files\R\R-4.x.x\bin`）新增至 `PATH`。

    執行成功後，所有最新的圖表將會被儲存至 `output_chapters/images/` 資料夾中。
