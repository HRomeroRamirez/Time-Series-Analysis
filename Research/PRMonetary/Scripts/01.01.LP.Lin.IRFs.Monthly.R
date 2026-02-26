# Summary ----------------------------------------------------------------------
# Local Projections Project
# This script calculates IRFs for the relationship of SF Fed Proxy Funds Rate with
# Puerto Rico's Headline/Core CPI Inflation and Unemployment Rate
# The series goes from Oct-2008 to Dec-2025

####### Comments on lpirfs package ########
# lp_lin is a System Estimator: The lpirfs package is designed to mirror VAR-style output.
# When you pass a data frame to lp_lin,
# it iterates the regression for every variable in that data frame as a dependent variable for every horizon.

# The "Requirement" is Package-Specific:
# It is not a statistical requirement of Local Projections; it is a functional requirement of the lpirfs function syntax.

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
# The Data was originally on a index form (base 100). The CPI data was seasonally adjusted.
# Then were transform into their Inverse Hyperbolic Sine function form, we used this method because the proxy funds rate
# has negative values. All these transformations are already implemented on the previous script.

# Read final data version
macroData <- read_excel(file.path(outputsFolder, "Monthly_FinalData_IHS_SA.xlsx"))

# Sanity Checks & Transformations ---------------------------------------------
# Get column names
names(macroData)

# Print head and Tail
print(head(macroData))
print(tail(macroData))

# Calculate differences for all but "Date" and re-bind with the adjusted Date column
# 1. Create a copy of macroData excluding the first row (to match the length of diffs)
macroData2 <- macroData[-1, ]

# 2. Overwrite the non-Date columns with their differenced values
macroData2[, names(macroData) != "Date"] <- lapply(macroData[, names(macroData) != "Date"], diff)

# Variables of interest ----------------------------------------------------

# Scenario 1: PFR_2, HCPI, and UR as Endogenous
df_hcpi <- macroData2[, c("PFR - US", "HCPI - PR", "UR - PR")]

# Scenario 2: PFR_2, CCPI, and UR as Endogenous
df_ccpi <- macroData2[, c("PFR - US", "CCPI - PR", "UR - PR")]

# Scenario 1: Headline CPI Estimation (Monthly) -----------------------------------------
results_hcpi <- lp_lin(
  endog_data      = as.data.frame(df_hcpi),
  lags_endog_lin  = 12,
  trend           = 0,
  shock_type      = 1,
  hor             = 24,
  confint         = 1.96,
  use_nw         = TRUE,
  nw_lag         = 25,
  adjust_se      = TRUE
)

# Scenario 2: Core CPI Estimation (Monthly) --------------------------------------------
results_ccpi <- lp_lin(
  endog_data      = as.data.frame(df_ccpi),
  lags_endog_lin  = 12,
  trend           = 0,
  shock_type      = 1,
  hor             = 24,
  confint         = 1.96,
  use_nw         = TRUE,
  nw_lag         = 25,
  adjust_se      = TRUE
)

# Summary Results ----------------------------------------------
# Inspect the summary for the first scenario
summary(results_hcpi)

# Inspect the summary for the second scenario
summary(results_ccpi)

# --- Visualizing only the 6 required responses per scenario ---
# We use vars_response to exclude the rows where PFR_2 is the dependent variable
plots_hcpi <- plot(results_hcpi, vars_response = c("HCPI - PR", "UR - PR"))
plots_ccpi <- plot(results_ccpi, vars_response = c("CCPI - PR", "UR - PR"))

# Print Plots
print(plots_hcpi)
print(plots_ccpi)










