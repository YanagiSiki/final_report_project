# Copilot 使用指引（final_report_project）

## 專案簡介

本專案為「統計基礎及 R 語言應用」課程之期末報告，聚焦於少子化、統測報名趨勢與產業薪資結構的關聯分析。所有分析皆以公開數據集與 R 腳本自動化產出，強調可重現性與學術嚴謹。

## 主要結構與資料流程

- **chapters/**：各章節 Markdown 原始檔，依主題分資料夾（如：緒論、資料方法、分析、結論等）。
- **data/**：原始 CSV 數據集（薪資、出生世代、報名人數），為所有分析基礎。
- **scripts/**：R 語言分析腳本，對應各分析步驟或章節，負責資料處理、統計分析與圖表產生。
- **output/**：所有自動產生的結果，包括：
  - `figures/`：所有圖表與視覺化（由腳本自動產生）
  - `final_report_integrated.md`：整合後的完整報告（由各章節串接而成，供檢閱/提交）
  - `statistical_methods.md`：統計方法與模型說明文件，隨分析進度同步更新

## 開發者與 AI 代理人協作規範

- **可重現性**：所有結果與圖表必須由 `scripts/` 內 R 腳本自動產生，嚴禁手動編輯 output 內容。
- **更新流程**：
  1. 依 README 安裝所需 R 套件
  2. 依 README 指定順序執行所有腳本
  3. 所有輸出自動寫入 `output/` 與 `output/figures/`
- **內容編輯**：
  - 僅能於 `chapters/` 內撰寫/修改報告內容，`final_report_integrated.md` 僅作自動整合，請勿直接編輯。
  - 若分析方法有新增或調整，需同步更新 `statistical_methods.md`。
- **專案慣例**：
  - 報告內容一律採 Markdown 格式。
  - 章節結構、檔名需與 `prompt_master_outline.md` 一致。
  - CSV 檔僅能依官方來源更新，嚴禁任意更動。
- **整合規則**：
  - `final_report_integrated.md` 由各章節檔案串接產生，整合前請確認章節內容已更新。
  - 報告引用圖表須與 `output/figures/` 內檔案對應。
- **重要參考**：
  - 請參閱 `README.md`、`AGENT.md` 以理解專案脈絡、流程與慣例。
  - `AGENT.md` 詳列群類與產業對應表、分析邏輯與專案背景。

## 標準作業流程範例

- 若需更新分析或數據：
  1. 於 `data/` 新增或更新數據
  2. 於 `scripts/` 編輯或新增分析腳本
  3. 依 README 執行腳本產生新結果
  4. 於 `chapters/` 撰寫/修正報告內容
  5. 重新整合產生 `final_report_integrated.md`
  6. 若有新統計方法，務必同步更新 `statistical_methods.md`

## 特別注意

- 未經說明請勿更動 `chapters/`、`data/`、`output/` 結構，若有調整需同步更新相關文件與整合腳本。
- 所有統計方法必須完整記錄於 `output/statistical_methods.md`。
- 任何自動化、整合作業，皆須遵循 `prompt_master_outline.md` 與 `AGENT.md` 所定規則與順序。

## Commit message 規範

- 所有 commit message 預設以中文撰寫，內容需簡明扼要反映本次修改重點。
  - 範例：
    - 新增薪資資料分析腳本
    - 修正統計方法說明文件
    - 整合最新章節內容產生報告
