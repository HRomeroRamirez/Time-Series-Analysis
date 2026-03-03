# Summary----------------------------------------------------------------:
# Create a times series chart of FEDFUNDS and the SF Fed Proxy Funds Rate

# Read Packages ---------------------------------------------------
# Read packages
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

if (!dir.exists(outputsFolder)) {
  dir.create(outputsFolder, recursive = TRUE)
}
# Inputs ------------------------------------------------------------------
# The Data was originally on a index form (base 100).

# Read final data version
data <- read_excel(file.path(inputsFolder, "Data_Interest_Rate.xlsx"))

# Sanity Checks --------------------------------------------------------------

# Get column names
names(data)

# Print head and Tail
print(head(data))
print(tail(data))

# Plotting the Time Series --------------------------------------------------

# Ensure the Date column is in the correct format
data$Date <- as.Date(data$Date)

ggplot(data, aes(x = Date)) +
  # First Series: Effective funds rate
  geom_line(aes(y = `Effective funds rate`, color = "Effective funds rate"), linewidth = 1) + 
  # Second Series: SF Fed Proxy funds rate
  geom_line(aes(y = `SF Fed Proxy funds rate`, color = "SF Fed Proxy funds rate"), linewidth = 1) +
  # Formatting
  scale_color_manual(values = c("Effective funds rate" = "red", "SF Fed Proxy funds rate" = "blue")) +
  labs(title = "Effective funds rate vs. SF Fed Proxy Funds Rate",
       x = "Date",
       y = "Rate (%)",
       color = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom")

