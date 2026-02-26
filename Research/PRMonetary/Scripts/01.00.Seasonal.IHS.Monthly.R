# Summary----------------------------------------------------------------:
# UR was download from FRED as seasonally adjusted
# Proxy rates are market-determined and do not contain seasonal "noise" 
# in the way consumer prices do
# Do analysis for HCPI and CCPI

# Install & Read Packages ---------------------------------------------------
# Read packages
library(readxl)
library(writexl)
library(seasonal)

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
# The Data was originally on a index form (base 100).

# Read final data version
macroData <- read_excel(file.path(inputsFolder, "RawDataPRMonthly.xlsx"))

# Sanity Checks --------------------------------------------------------------

# Get column names
names(macroData)

# Print head and Tail
print(head(macroData))
print(tail(macroData))

# Analysis ------------------------------------------------------------

# 1. Ensure BOTH series have the correct start date
ts_hcpi <- ts(macroData[["HCPI - PR"]], start = c(2008, 10), frequency = 12)
ts_ccpi <- ts(macroData[["CCPI - PR"]], start = c(2008, 10), frequency = 12)
ts_ie2 <- ts(macroData[["IE2 - US"]], start = c(2008, 10), frequency = 12)

# 2. Now perform Seasonal Adjustment
sa_hcpi <- seas(ts_hcpi)
sa_ccpi <- seas(ts_ccpi)
sa_ie2 <- seas(ts_ie2)

# 3. Extract the final seasonally adjusted series
macroData$HCPI_SA <- final(sa_hcpi)
macroData$CCPI_SA <- final(sa_ccpi)
macroData$IE2_SA <- final(sa_ie2)

# 4. Now apply the IHS transformation to the SA series
macroData$HCPI_IHS_SA <- log(macroData$HCPI_SA + sqrt(macroData$HCPI_SA^2 + 1))
macroData$CCPI_IHS_SA <- log(macroData$CCPI_SA + sqrt(macroData$CCPI_SA^2 + 1))
macroData$IE2_IHS_SA <- log(macroData$IE2_SA + sqrt(macroData$IE2_SA^2 + 1))

# Estimate HSI for variables dad do not requiere SA
macroData$UR_IHS_SA <- log(macroData$`UR - PR` + sqrt(macroData$`UR - PR`^2 + 1))
macroData$PFR_IHS_SA <- log(macroData$`PFR - US` + sqrt(macroData$`PFR - US`^2 + 1))

# Create the final system data frame
df_final <- data.frame(
  Date         = macroData$Date,
  HCPI_IHS_SA  = macroData$HCPI_IHS_SA,
  CCPI_IHS_SA  = macroData$CCPI_IHS_SA,
  UR_IHS_SA    = macroData$UR_IHS_SA,
  PFR_IHS_SA   = macroData$PFR_IHS_SA,
  IE2_IHS_SA   =  macroData$IE2_IHS_SA
)

# Rename Columns
names(df_final)[names(df_final) == "HCPI_IHS_SA"] <- "HCPI - PR"
names(df_final)[names(df_final) == "CCPI_IHS_SA"] <- "CCPI - PR"
names(df_final)[names(df_final) == "PFR_IHS_SA"] <- "PFR - US"
names(df_final)[names(df_final) == "UR_IHS_SA"] <- "UR - PR"
names(df_final)[names(df_final) == "IE2_IHS_SA"] <- "IE2 - US"

# Save final data frame
writexl::write_xlsx(df_final, file.path(outputsFolder, "Monthly_FinalData_IHS_SA.xlsx"))