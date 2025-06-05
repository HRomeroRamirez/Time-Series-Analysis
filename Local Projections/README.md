# # Macroeconomic Time Series Analysis: Local Projections for Impulse Response Estimation

## Overview
This repository implements the Local Projections (LP) method to estimate Impulse Response Functions (IRFs). The LP approach provides flexibility in modeling dynamic causal effects without imposing structural assumptions inherent in vector autoregressions (VARs).

## Features
- **Data Handling**: Reads macroeconomic time series data from Excel.
- **Lagged Variable Construction**: Generates lags for explanatory variables.
- **Local Projection Estimation**: Runs regressions for varying horizons using linear models.
- **Robust Inference**: Computes Newey-West standard errors to account for serial correlation.
- **Visualization**: Produces impulse response plots with confidence intervals.

## Dependencies
Ensure the following R packages are installed:
```r
install.packages(c("sandwich", "lmtest", "readxl", "ggplot2", "dplyr"))
