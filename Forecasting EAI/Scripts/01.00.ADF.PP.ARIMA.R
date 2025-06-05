# Install and Load Packages ------------------------------------------------------
if (!require(tseries)) install.packages("tseries")
if (!require(forecast)) install.packages("forecast")
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")

library(tseries)
library(forecast)
library(readxl)
library(ggplot2)

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

# Read Data ------------------------------------------------------------------
dataEai <- read_excel(file.path(inputsFolder, "EAIData.xlsx"))

# Make sure the Date column is in Date format
dataEai$Date <- as.Date(dataEai$Date)

# Create Time Series 
# Extract the EAI variable as a ts object (with frequency = 12 for monthly data)
eaiTs <- ts(dataEai$EAI, frequency = 12)

# Unit Root Tests ------------------------------------------------------------
adfResult <- adf.test(eaiTs)
cat("\nADF Test:\n")
print(adfResult)

ppResult <- pp.test(eaiTs)
cat("\nPP Test:\n")
print(ppResult)

# Differencing Logic --------------------------------------------------------
d <- 0
if (adfResult$p.value > 0.05 & ppResult$p.value > 0.05) {
  d <- 1
  eaiDiff <- diff(eaiTs)
  plot(eaiDiff, main = "Differenced EAI", ylab = "Differenced EAI", xlab = "Time")
} else {
  eaiDiff <- eaiTs
}

# Fit ARIMA Model ----------------------------------------------------------
fit <- auto.arima(eaiTs, d = d, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
cat("\nSelected ARIMA Model:\n")
print(fit)

# Optional: Try ARMA if no differencing
if (d == 0) {
  fitArma <- arima(eaiTs, order = c(2, 0, 1))
  cat("\nARMA(2,1) Model:\n")
  print(fitArma)
}

# Forecasting -------------------------------------------------------------
forecastHorizon <- 12
fc <- forecast(fit, h = forecastHorizon)

# Prepare Data for Plot ---------------------------------------------------
# Fitted values
fittedVals <- fitted(fit)
fittedDates <- tail(dataEai$Date, length(fittedVals))

# Future dates (assumes monthly frequency)
lastDate <- max(dataEai$Date)
futureDates <- seq(from = lastDate + 30, by = "month", length.out = forecastHorizon)

# Forecast dataframe
forecastDf <- data.frame(
  Date = futureDates,
  Forecast = as.numeric(fc$mean),
  Lower = as.numeric(fc$lower[, 2]),
  Upper = as.numeric(fc$upper[, 2])
)

# Actual fitted values
fittedDf <- data.frame(
  Date = fittedDates,
  Fitted = as.numeric(fittedVals)
)

# Plot with ggplot2 ------------------------------------------------------
eaiPlot <- ggplot() +
  geom_line(data = dataEai, aes(x = Date, y = EAI), color = "black", linewidth = 0.8, alpha = 0.6) +
  geom_line(data = fittedDf, aes(x = Date, y = Fitted), color = "blue", linewidth = 1) +
  geom_line(data = forecastDf, aes(x = Date, y = Forecast), color = "red", linewidth = 1) +
  geom_ribbon(data = forecastDf, aes(x = Date, ymin = Lower, ymax = Upper), 
              fill = "pink", alpha = 0.3) +
  labs(title = "EAI Fitted and Forecasted Values",
       x = "Date", y = "EAI",
       caption = "Black: Observed | Blue: Fitted | Red: Forecast | Pink Band: 95% CI") +
  theme_minimal()

# Export Outputs ----------------------------------------------------------
write.csv(forecastDf, file = file.path(outputsFolder, "ForecastEAIdata.csv"), row.names = FALSE)

ggsave(filename = file.path(outputsFolder, "EAI_Forecast_Plot.png"), plot = eaiPlot, width = 10, height = 6, dpi = 300)



