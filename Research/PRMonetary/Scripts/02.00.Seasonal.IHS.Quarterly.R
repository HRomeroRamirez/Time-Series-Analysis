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
macroData <- read_excel(file.path(inputsFolder, "RawDataPRQuarterly_1988.xlsx"))

# Sanity Checks --------------------------------------------------------------

# Get column names
names(macroData)

# Print head and Tail
print(head(macroData))
print(tail(macroData))

# Analysis ------------------------------------------------------------

# 1. Ensure BOTH series have the correct start date
ts_hcpi <- ts(macroData[["HCPI - PR"]], start = c(1988, 01), frequency = 4)
ts_ccpi <- ts(macroData[["CCPI - PR"]], start = c(1988, 01), frequency = 4)
ts_wti <- ts(macroData[["WTI - US"]], start = c(1988, 01), frequency = 4)

# 2. Now perform Seasonal Adjustment
sa_hcpi <- seas(ts_hcpi)
sa_ccpi <- seas(ts_ccpi)
sa_wti <- seas(ts_wti)

# 3. Extract the final seasonally adjusted series
macroData$HCPI_SA <- final(sa_hcpi)
macroData$CCPI_SA <- final(sa_ccpi)
macroData$WTI_SA <- final(sa_wti)

# 4. Now apply the IHS transformation to the SA series
macroData$HCPI_IHS_SA <- log(macroData$HCPI_SA + sqrt(macroData$HCPI_SA^2 + 1))
macroData$CCPI_IHS_SA <- log(macroData$CCPI_SA + sqrt(macroData$CCPI_SA^2 + 1))
macroData$WTI_IHS_SA <- log(macroData$WTI_SA + sqrt(macroData$WTI_SA^2 + 1))

# Estimate HSI for variables dad do not requiere SA
macroData$UR_IHS_SA <- log(macroData$`UR - PR` + sqrt(macroData$`UR - PR`^2 + 1))
macroData$PFR_IHS_SA <- log(macroData$`PFR - US` + sqrt(macroData$`PFR - US`^2 + 1))
macroData$MPS_IHS_SA <- log(macroData$`MPS - US` + sqrt(macroData$`MPS - US`^2 + 1))
macroData$UR_US_IHS_SA <- log(macroData$`UR - US` + sqrt(macroData$`UR - US`^2 + 1))
macroData$HCPI_US_IHS_SA <- log(macroData$`HCPI - US` + sqrt(macroData$`HCPI - US`^2 + 1))
macroData$CCPI_US_IHS_SA <- log(macroData$`CCPI - US` + sqrt(macroData$`CCPI - US`^2 + 1))
macroData$EAI_US_IHS_SA <- log(macroData$`EAI - US` + sqrt(macroData$`EAI - US`^2 + 1))
macroData$EAI_PR_IHS_SA <- log(macroData$`EAI - PR` + sqrt(macroData$`EAI - PR`^2 + 1))

# Create the final system data frame
df_final <- data.frame(
  Date         = macroData$Date,
  HCPI_IHS_SA  = macroData$HCPI_IHS_SA,
  CCPI_IHS_SA  = macroData$CCPI_IHS_SA,
  UR_IHS_SA    = macroData$UR_IHS_SA,
  PFR_IHS_SA   = macroData$PFR_IHS_SA,
  MPS_IHS_SA   =  macroData$MPS_IHS_SA,
  UR_US_IHS_SA   =  macroData$UR_US_IHS_SA,
  HCPI_US_IHS_SA   =  macroData$HCPI_US_IHS_SA,
  CCPI_US_IHS_SA   =  macroData$CCPI_US_IHS_SA,
  WTI_IHS_SA   =  macroData$WTI_IHS_SA,
  EAI_US_IHS_SA   =  macroData$EAI_US_IHS_SA,
  EAI_PR_IHS_SA   =  macroData$EAI_PR_IHS_SA
)

# Rename Columns
names(df_final)[names(df_final) == "HCPI_IHS_SA"] <- "HCPI - PR"
names(df_final)[names(df_final) == "CCPI_IHS_SA"] <- "CCPI - PR"
names(df_final)[names(df_final) == "PFR_IHS_SA"] <- "PFR - US"
names(df_final)[names(df_final) == "UR_IHS_SA"] <- "UR - PR"
names(df_final)[names(df_final) == "MPS_IHS_SA"] <- "MPS - US"
names(df_final)[names(df_final) == "UR_US_IHS_SA"] <- "UR - US"
names(df_final)[names(df_final) == "HCPI_US_IHS_SA"] <- "HCPI - US"
names(df_final)[names(df_final) == "CCPI_US_IHS_SA"] <- "CCPI - US"
names(df_final)[names(df_final) == "WTI_IHS_SA"] <- "WTI - US"
names(df_final)[names(df_final) == "EAI_US_IHS_SA"] <- "EAI - US"
names(df_final)[names(df_final) == "EAI_PR_IHS_SA"] <- "EAI - PR"

# Save final data frame
writexl::write_xlsx(df_final, file.path(outputsFolder, "Quarterly_FinalData_IHS_SA.xlsx"))