# Local Projections Project ------------------------------------------------------

# Install and Load Packages ------------------------------------------------------
if (!require(tseries)) install.packages("sandwich")
if (!require(forecast)) install.packages("lmtest")
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggplot2)) install.packages("dplyr")

library(sandwich)
library(lmtest)
library(readxl)
library(ggplot2)
library(dplyr)

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
macroData <- read_excel(file.path(inputsFolder, "Data_Macro.xlsx"))

# Operations -----------------------------------------------------------------

# Set Parameters 
H <- 24  # Maximum horizon for IRF
p <- 4   # Number of lags to control for

# Prepare Data 
# Assume macroData is already loaded and contains: Date, FEDFUNDS, EPUMONETARY
macroData <- macroData %>% arrange(Date)

# Create lags for FEDFUNDS and EPUMONETARY
for (i in 1:p) {
  macroData[[paste0("L", i, "_FEDFUNDS")]] <- dplyr::lag(macroData$FEDFUNDS, i)
  macroData[[paste0("L", i, "_EPUMONETARY")]] <- dplyr::lag(macroData$EPUMONETARY, i)
}

# Estimate Local Projections 
results <- data.frame(h = 0:H, beta = NA, se = NA)

for (h in 0:H) {
  # Generate the h-period ahead value of EPUMONETARY
  macroData[[paste0("EPUMONETARY_h", h)]] <- dplyr::lead(macroData$EPUMONETARY, h)
  
  # Define regression formula
  formula_lp <- as.formula(
    paste0("EPUMONETARY_h", h, " ~ FEDFUNDS + ",
           paste(c(paste0("L", 1:p, "_FEDFUNDS"),
                   paste0("L", 1:p, "_EPUMONETARY")), collapse = " + "))
  )
  
  # Run regression
  model <- lm(formula_lp, data = macroData)
  
  # Compute Newey-West standard errors
  nw_se <- sqrt(diag(sandwich::NeweyWest(model, lag = h, prewhite = FALSE)))
  
  # Store results
  results$beta[h + 1] <- coef(model)["FEDFUNDS"]
  results$se[h + 1] <- nw_se["FEDFUNDS"]
}

# Create Confidence Intervals 
results <- results %>%
  mutate(
    lower = beta - 1.96 * se,
    upper = beta + 1.96 * se
  )

# Plot IRF -------------------------------------------------------------------
irfPlot <- ggplot(results, aes(x = h, y = beta)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgreen", alpha = 0.4) +
  labs(
    title = "Impulse Response of EPUMONETARY to a Shock in FEDFUNDS",
    x = "Horizon (h)",
    y = "Response of EPUMONETARY"
  ) +
  theme_minimal(base_size = 14)


# Export Outputs ----------------------------------------------------------
write.csv(macroData, file = file.path(outputsFolder, "Outputs_Macro_Data.csv"), row.names = FALSE)

ggsave(file.path(outputsFolder, "Impulse_Response_EPUMONETARY.png"), plot = irfPlot, width = 8, height = 6, dpi = 300)
