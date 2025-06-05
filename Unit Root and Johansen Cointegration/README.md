# Macroeconomic Time Series Analysis: Unit Root and Johansen Cointegration Tests in R

This repository contains R scripts to perform stationarity and cointegration analysis on macroeconomic time series data. The main focus is on two variables — **FEDFUNDS** (Federal Funds Rate) and **EAI** (Economic Activity Index) — using Augmented Dickey-Fuller (ADF), Phillips-Perron (PP) tests, and Johansen cointegration tests.

---

## Project Overview

The analysis follows these steps:

1. **Data Import and Preparation**  
   Read macroeconomic data from Excel, convert date columns, and prepare monthly time series objects.

2. **Unit Root Testing**  
   Test stationarity of each variable using ADF and PP tests.  
   - Non-stationary variables are differenced once (I(1)).

3. **Cointegration Testing**  
   If both series are I(1), proceed with Johansen cointegration tests (trace and max eigenvalue tests) to identify long-run relationships.

4. **Output and Visualization**  
   Diagnostic output is printed in the console, including test statistics and p-values.  
   Differenced series are plotted for visual inspection.

---

## Files

- `00.01.ADF.PP.Cointegration.R`  
  Main R script executing the entire workflow: loading data, testing unit roots, and performing cointegration analysis.

- `Data_Macro.xlsx`  
  Sample dataset containing monthly data for `FEDFUNDS`, `EAI`, and other variables.

---

## Requirements

- R (>= 4.0 recommended)
- R packages:
  - `tseries`
  - `forecast`
  - `readxl`
  - `ggplot2`
  - `urca`

Packages will be installed automatically if missing.

---

## Usage

1. Clone or download this repository.
2. Place your data file (`Data_Macro.xlsx`) in the `inputs` folder or update the path in the script.
3. Open `00.01.ADF.PP.Cointegration.R` in RStudio or your preferred R environment.
4. Run the script to see unit root test results, plots of differenced series if necessary, and Johansen cointegration test summaries.
5. Review console output for detailed interpretation of stationarity and cointegration.

---

Feel free to contribute or raise issues!

---
