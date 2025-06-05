# Install and Load Packages ------------------------------------------------------
if (!require(tseries)) install.packages("tseries")
if (!require(forecast)) install.packages("forecast")
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(urca)) install.packages("urca")

library(tseries)
library(forecast)
library(readxl)
library(ggplot2)
library(urca)

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

# Ensure the base output directory exists -------------------------------------
if (!dir.exists(outputsFolder)) {
  dir.create(outputsFolder, recursive = TRUE)
}

# Read Data --------------------------------------------------------------------
dataMacro <- read_excel(file.path(inputsFolder, "Data_Macro.xlsx"))
dataMacro$Date <- as.Date(dataMacro$Date)

# Variables to Analyze --------------------------------------------------------
seriesVars <- c("FEDFUNDS", "EAI")

# Unit Root Test Function -----------------------------------------------------
unit_root_test <- function(series, seriesName) {
  cat("\n------------------------------------------------------------\n")
  cat(paste("Analyzing:", seriesName))
  cat("\n------------------------------------------------------------\n")
  
  tsSeries <- ts(series, frequency = 12)
  
  adfResult <- adf.test(tsSeries)
  cat("\nADF Test:\n")
  print(adfResult)
  
  ppResult <- pp.test(tsSeries)
  cat("\nPP Test:\n")
  print(ppResult)
  
  d <- 0
  if (adfResult$p.value > 0.05 & ppResult$p.value > 0.05) {
    d <- 1
    tsDiff <- diff(tsSeries)
    plot(tsDiff, main = paste("Differenced", seriesName), ylab = paste("Δ", seriesName), xlab = "Time")
    cat(paste("\n", seriesName, "is non-stationary. First difference taken (d = 1).\n"))
  } else {
    tsDiff <- tsSeries
    cat(paste("\n", seriesName, "appears stationary (d = 0).\n"))
  }
  
  return(list(original = tsSeries, diffed = tsDiff, d = d,
              adf = adfResult$p.value, pp = ppResult$p.value))
}

# Run Unit Root Tests ---------------------------------------------------------
seriesResults <- list()
for (var in seriesVars) {
  seriesResults[[var]] <- unit_root_test(dataMacro[[var]], var)
}

# Summary of Integration Orders -----------------------------------------------
cat("\n\nSummary of Integration Orders:\n")
for (var in seriesVars) {
  cat(sprintf("%s: d = %d (ADF p = %.4f, PP p = %.4f)\n",
              var,
              seriesResults[[var]]$d,
              seriesResults[[var]]$adf,
              seriesResults[[var]]$pp))
}

# Johansen Cointegration Test (2 variables only) -------------------------------
all_I1 <- all(sapply(seriesResults, function(x) x$d == 1))

if (all_I1) {
  cat("\nBoth FEDFUNDS and EAI are I(1). Proceeding to Johansen cointegration test.\n")
  
  minLength <- min(sapply(seriesResults, function(x) length(x$original)))
  combinedTs <- ts(data.frame(
    FEDFUNDS = seriesResults$FEDFUNDS$original[1:minLength],
    EAI      = seriesResults$EAI$original[1:minLength]
  ), frequency = 12)
  
  # Johansen test
  johansen_trace <- ca.jo(combinedTs, type = "trace", ecdet = "const", K = 3)
  johansen_eigen <- ca.jo(combinedTs, type = "eigen", ecdet = "const", K = 3)
  
  cat("\n\n=== Johansen Trace Test ===\n")
  print(summary(johansen_trace))
  
  cat("\n\n=== Johansen Max Eigenvalue Test ===\n")
  print(summary(johansen_eigen))
  
} else {
  cat("\nAt least one series is not I(1). Johansen test not valid.\n")
}
