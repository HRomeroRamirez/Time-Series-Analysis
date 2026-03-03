# Summary ----------------------------------------------------------------------
# Local Projections Project
# This script calculates IRFs for the relationship of SF Fed Proxy Funds Rate with
# Puerto Rico's Headline/Core Median CPI Inflation, Unemployment Rate and Economic Activity Index
# The series goes from Q1-1989 to Q4-2023

####### Comments on lpirfs package ########
# lp_lin is a System Estimator: The lpirfs package is designed to mirror VAR-style output.
# When you pass a data frame to lp_lin,it iterates the regression for every variable 
# in that data frame as a dependent variable for every horizon.
# The "Requirement" is Package-Specific:
# It is not a statistical requirement of Local Projections; it is a functional requirement of the lpirfs function syntax.

# Variable Dictorionary:

# HCPI - PR = Headline CPI Inflation - Puerto Rico
# CCPI - PR = Core Median CPI Inflation - Puerto Rico
# UR - PR   = Unemployment Rate - Puerto ico
# EAI - PR  = Economic Activity Index - Puerto Rico
# PFR - US = San Francisco Fed Proxy Funds Rate - United States
# MPS - US = San Francisco Fed Raw Monetary Policy Surprises - United States
# UR - US = Unemployment Rate - United States
# HCPI - US = Headline CPI Inflation - United States
# CCPI - US = Core Median CPI Inflation - United States
# EAI - US = Economic Activity Index - United States

# Install & Read Packages --------------------------------------------------------------

# Install packages (if necessary)
# install.packages("lpirfs", type = "binary")

# Read packages
library(readxl)
library(lpirfs)
library(tseries)
library(ggplot2)
library(gridExtra)

# Clear Environment -----------------------------------------------------------
rm(list = ls())
gc()

# Try-Catch Block for Sourcing Setup File -------------------------------------
tryCatch({
  dirSet <- getSrcDirectory(function(x) { x })
  sourceFile <- list.files(dirSet, recursive = TRUE, full.names = TRUE)
  source(sourceFile[grepl("00.Setup.", sourceFile)])
}, error = function(e) {
  dirSet <- dirname(rstudioapi::getActiveDocumentContext()$path)
  sourceFile <- list.files(dirSet, recursive = TRUE, full.names = TRUE)
  source(sourceFile[grepl("00.Setup.", sourceFile)])
})

if (!dir.exists(outputsFolder)) {
  dir.create(outputsFolder, recursive = TRUE)
}
# Inputs ------------------------------------------------------------------
# The Data was originally on a index form (base 100). The variables were seasonally adjusted.
# Then were transform into their Inverse Hyperbolic Sine function form, we used this method because the proxy funds rate
# has negative values. All these transformations are already implemented on the previous script (02.00).

# Read final data version
macroData <- read_excel(file.path(outputsFolder, "Quarterly_FinalData_IHS_SA.xlsx"))

# Sanity Checks ----------------------------------------------------------------
# Get column names
names(macroData)

# Print head and Tail
print(head(macroData))
print(tail(macroData))

# Data Transformations --------------------------------------------------------

# Define the lag length for the long difference (e.g., 4 for 1 year)
k <- 4

# Long Difference version
# 1. Create a copy of macroData excluding the first k rows
macroData2 <- macroData[-(1:k), ]

# 2. Function to calculate long difference: x[t] - x[t-k]
long_diff <- function(x, k) {
  return(x[(k + 1):length(x)] - x[1:(length(x) - k)])
}

# 3. Apply the function to all non-Date columns
cols_to_fix <- names(macroData) != "Date"
macroData2[, cols_to_fix] <- lapply(macroData[, cols_to_fix], long_diff, k = k)

# Create Fiscal Dummy Variable
# 1. Create the date sequence (assuming your quarterly data starts Q1 2006)
# Adjust the start date if your 'macroData_quarterly' starts elsewhere
dates_quarterly <- seq(from = as.Date("1989-01-01"), to = as.Date("2023-10-01"), by = "quarter")

# 2. Create the dummy variable: 0 before Q1 2018, 1 from Q1 2018 onwards
# This captures the post-Maria reconstruction + COVID fical stimulus era
Fiscal_Dummy_M <- ifelse(dates_quarterly >= as.Date("2018-01-01"), 1, 0)

# 3. Add it to your monthly dataset (assuming it's named macroData_M)
macroData2$Fiscal_Dummy <- Fiscal_Dummy_M

# Variables of interest ----------------------------------------------------

# Scenario 1: PFR_2, HCPI, and UR as Endogenous
df_hcpi <- macroData2[, c("PFR - US", "HCPI - PR", "UR - PR", "EAI - PR")]

# Scenario 1: HCPI - US, and UR - US as contemp
df_hcpi_contemp <- macroData2[, c("HCPI - US", "UR - US", "EAI - US", "Fiscal_Dummy")]

# Scenario 2: PFR_2, CCPI, and UR as Endogenous
df_ccpi <- macroData2[, c("PFR - US", "CCPI - PR", "UR - PR", "EAI - PR")]

# Scenario 2: CCPI - US, and UR - US as contemp
df_ccpi_contemp <- macroData2[, c("CCPI - US", "UR - US", "EAI - US", "Fiscal_Dummy")]

# Scenario 1: Lag-Augmented LPs-OLS: Scenario with Headline CPI -----------------------------------------
results_hcpi <- lp_lin(
  endog_data      = as.data.frame(df_hcpi),
  contemp_data    = as.data.frame(df_hcpi_contemp),
  lags_endog_lin  = NA,
  lags_criterion = "BIC",
  max_lags =        4,
  trend           = 0,
  shock_type      = 1,
  hor             = 12,
  confint         = 1.96,
  use_nw         = TRUE,
  nw_lag         = 13,
  adjust_se      = TRUE
)

# Scenario 2: Lag-Augmented LPs-OLS: Scenario with Core Median CPI  -----------------------------
results_ccpi <- lp_lin(
  endog_data      = as.data.frame(df_ccpi),
  contemp_data    = as.data.frame(df_ccpi_contemp),
  lags_endog_lin  = NA,
  lags_criterion = "BIC",
  max_lags =        4,
  trend           = 0,
  shock_type      = 1,
  hor             = 12,
  confint         = 1.96,
  use_nw         = TRUE,
  nw_lag         = 13,
  adjust_se      = TRUE
)

# --- Visualizing only the 6 required responses per scenario ---
# We use vars_response to exclude the rows where PFR_2 is the dependent variable
plots_hcpi <- plot(results_hcpi, vars_response = c("HCPI - PR", "UR - PR", "EAI - PR"))
plots_ccpi <- plot(results_ccpi, vars_response = c("CCPI - PR", "UR - PR", "EAI - PR"))

# Print Plots
print(plots_hcpi)
print(plots_ccpi)

# LP-IV Identification Setup ---------------------------------------------

# Define the endogenous variable you want to instrument (The Shock)
shock_pfr <- as.data.frame(macroData2[, "PFR - US"])

# Define the instrument (Monetary Policy Surprises)
instrument_mps <- as.data.frame(macroData2[, "MPS - US"])

# --- Scenario 1: Lag-Augmented LPs-IV: Scenario with Headline CPI 
endog_hcpi <- as.data.frame(macroData2[, c("HCPI - PR", "UR - PR", "EAI - PR")])

results_iv_hcpi <- lp_lin_iv(
  endog_data      = endog_hcpi,
  contemp_data    = as.data.frame(df_hcpi_contemp),
  lags_endog_lin  = NA,
  lags_criterion = "BIC",
  max_lags =        4,
  shock           = shock_pfr,      
  instrum         = instrument_mps, 
  use_twosls      = TRUE,           
  trend           = 0,              
  hor             = 12,
  confint         = 1.96,
  use_nw          = TRUE,
  nw_lag          = 13,
  adjust_se      = TRUE
)

# --- Scenario 2:  Lag-Augmented LPs-IV: Scenario with Core Median CPI 
endog_ccpi <- as.data.frame(macroData2[, c("CCPI - PR", "UR - PR", "EAI - PR")])

results_iv_ccpi <- lp_lin_iv(
  endog_data      = endog_ccpi,
  contemp_data    = as.data.frame(df_ccpi_contemp),
  lags_endog_lin  = NA,
  lags_criterion = "BIC",
  max_lags =        4,
  shock           = shock_pfr,
  instrum         = instrument_mps,
  use_twosls      = TRUE,
  trend           = 0,              
  hor             = 12,
  confint         = 1.96,
  use_nw          = TRUE,
  nw_lag          = 13,
  adjust_se      = TRUE
)

# Plotting
plot(results_iv_hcpi)
plot(results_iv_ccpi)

# Call back Results ---------------------------------------------

# Inspect the summary for the first scenario: Lag Augmented OLS-HCPI
summary(results_hcpi)

# Inspect the summary for the second scenario: Lag Augmented OLS-CCPI
summary(results_ccpi)

# Inspect the summary for the first scenario: Lag Augmented IV-HCPI
summary(results_iv_hcpi)

# Inspect the summary for the second scenario: Lag Augmented IV-CCPI
summary(results_iv_ccpi)




