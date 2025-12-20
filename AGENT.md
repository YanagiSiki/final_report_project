# AGENT.md - 專案規格與情境指引

## 1. 專案概述

- **標題**：少子化衝擊下統測報考趨勢與產業薪資結構之關聯分析 (Impact of Declining Birth Rate on TCTE Registration Trends and Industry Salary Structure)
- **角色**：Expert Statistical Analyst & Academic Writer
- **核心假說**：
  - **宏觀層面**：總考生人數的下降主要由出生率驅動（Push Factor）。
  - **微觀層面**：學生在各群類的分布由「經濟理性」(Economic Rationality, Pull Factor) 所驅動。高薪產業雖無法阻止總人數下滑，但其**市場佔有率 (Market Share, Proportion)**相對於低薪產業會提升。
- **目前狀態**：分析已完成，報告撰寫中。主要發現為 _Absolute Salary_ 會驅動 _Market Share_，而 _Salary Growth_ 對 _Share Growth_ 並無顯著影響。

## 2. 工作區結構

- **`chapters/`**：報告 Markdown 原始檔。
  - `01_introduction/`：背景與動機。
  - `02_data-methods/`：資料來源與變數定義。
  - `03_analysis-core/`：主要統計分析（Macro, ANOVA, Static/Dynamic Regression）。
  - `04_conclusion/`：結論與建議。
- **`data/`**：原始 CSV 資料集。
  - `tcte_birth_cohort_statistics_100_113.csv`：出生數與總報名人數。
  - `salary_data_100_113.csv`：產業薪資資料。
  - `tcte_registration_100_114.csv`：各群類報名人數。
- **`scripts/`**：R 語言資料處理與圖表產生腳本。
- **`output/`**：產生的圖表與文字結果。

## 3. 資料規格與邏輯

### 3.1 變數對應表（Group to Industry）

_所有分析皆依此對應表，不可更動。_

| TCTE Group (統測群類)  | Industry (行業別)            | Attribute               |
| :--------------------- | :--------------------------- | :---------------------- |
| **餐旅群**             | **住宿及餐飲業**             | Low Salary / High Churn |
| **電機與電子群資電類** | **出版影音及資通訊業**       | High Salary / Tech      |
| **商業與管理群**       | **金融及保險業**             | High Salary / Service   |
| **機械群**             | **製造業**                   | Traditional Ind.        |
| **動力機械群**         | **製造業**                   | Traditional Ind.        |
| **土木與建築群**       | **營建工程業**               | High Labor / High Pay   |
| **衛生與護理類**       | **醫療保健及社會工作服務業** | Stable / High Demand    |
| **藝術群影視類**       | **藝術娛樂及休閒服務業**     | Interest-driven         |
| **化工群**             | **製造業**                   | Traditional Ind.        |

### 3.2 主要指標

- **Market Share (報名佔比)**：$ \frac{\text{Group Registration}}{\text{Total Registration}} $（用以控制出生率下滑的影響）
- **Growth Rate (年增率)**：$ \frac{V*t - V*{t-1}}{V\_{t-1}} $

## 4. 分析流程與主要發現（The "Spec"）

### Phase 1: Macro Trend (Chapter 3.1)

- **目標**：驗證 Push Factor。
- **方法**：Pearson Correlation。
- **發現**：出生率（滯後 18 年）與總報名人數高度正相關。

### Phase 2: Industry Salary Structure (Chapter 3.2)

- **目標**：驗證 Economic Incentive。
- **方法**：產業薪資 One-way ANOVA。
- **發現**：產業間存在顯著差異，可分為三層級（Tier 1: Tech/Finance, Tier 2: Health/Construction, Tier 3: Hospitality）。

### Phase 3: Market Share Analysis (Chapter 3.3)

- **目標**：驗證 Pull Factor。
- **Model A (Static)**：$ Y*{share} = \beta_0 + \beta_1 X*{salary} $
  - **結果**：**顯著**。高絕對薪資對應高市場佔有率。
- **Model B (Dynamic)**：$ \Delta Y*{share} = \beta_0 + \beta_1 \Delta X*{salary} $
  - **結果**：**不顯著**。短期薪資成長對市場佔有率成長無顯著影響。
- **結論**：學生反應於「存量」(Stock, Current High Salary)，而非「流量」(Flow, Growth Rate)。

## 5. 撰寫規範

- **格式**：Markdown。
- **數學**：公式請用 LaTeX（如 $R^2$, $p < 0.05$）。
- **語氣**：學術、客觀、分析性。
- **圖表引用**：以相對路徑引用 `output/figures/` 內圖檔。
- **語言**：繁體中文。

## 6. AI 執行規則

1.  **優先閱讀**：執行任務前，務必先檢查 `AGENT.md`，理解變數對應與專案情境。
2.  **資料正確性**：不得憑空產生數據，僅能使用提供的 CSV 或 R script 輸出。
3.  **一致性**：`chapters/` 內容須與 `output/` 統計結果相符。
