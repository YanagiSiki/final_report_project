# 「統計基礎及 R 語言應用」期末報告專案

本專案為「統計基礎及 R 語言應用」課程的期末報告。主要目標是利用提供的數據集進行統計分析，探討不同變數之間的關聯性。

## 目錄結構

- `chapters/`: 存放報告各章節的原始碼檔案 (.md)。
- `data/`: 存放原始數據集 (CSV 檔案)。
- `output/`: 存放所有由腳本自動產生的結果。
  - `figures/`: 存放所有視覺化圖表。
- `scripts/`: 存放所有執行的 R 分析腳本。
- `prompt_master_outline.md`: 定義報告大綱及各章節分析方向的提示文件。
- `README.md`: 專案說明文件（本文件）。

## 數據集

本專案使用以下數據集：

- `salary_data_109_113.csv`: 109 年至 113 年薪資相關數據。
- `tcte_birth_cohort_statistics_109_113.csv`: 109 年至 113 年出生世代統計數據。
- `tcte_registration_109_114.csv`: 109 年至 114 年登記相關數據。

---

### 如何產生/更新所有圖表與分析結果

本專案中的所有視覺化圖表與分析均由 R 腳本自動產生。

**需求:**

- 已安裝 R 環境 (R version 4.0.0 或以上)。
- 已安裝 R 的相關套件。

**執行步驟:**

1.  **安裝套件:**
    開啟 R 環境，並執行以下指令來安裝所有必要的套件：

    ```R
    install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "svglite", "gghighlight", "scales", "stringr"))
    ```

2.  **執行腳本:**
    在專案的根目錄下，開啟終端機（Command Prompt, PowerShell, Terminal），並依序執行以下所有指令以產生完整的分析結果：

    ```bash
    Rscript scripts/2_descriptive_analysis.R
    Rscript scripts/3-1_analysis.R
    Rscript scripts/3-2_analysis.R
    Rscript scripts/3-3_analysis.R
    Rscript scripts/3-4_analysis.R
    ```

    > **注意:** 如果系統提示 `Rscript` 不是可辨識的指令，代表 R 的安裝路徑尚未加入到系統的環境變數 `PATH` 中。請將 R 的 `bin` 資料夾路徑（例如 `C:\Program Files\R\R-4.x.x\bin`）新增至 `PATH`。

    執行成功後，所有最新的圖表將會被儲存至 `output/figures/` 資料夾中。
