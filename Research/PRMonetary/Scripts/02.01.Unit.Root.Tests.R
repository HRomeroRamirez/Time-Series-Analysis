# # Install if necessary
# install.packages("tidyverse", type = "binary")
# install.packages("broom", type = "binary")
# install.packages("knitr", type = "binary")
# install.packages("kableExtra", type = "binary")

# Read packages
library(readxl)
library(tseries)
library(tidyverse)
library(broom)
library(urca)
library(knitr)
library(kableExtra)

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
macroData <- read_excel(file.path(outputsFolder, "Quarterly_FinalData_IHS_SA.xlsx"))

# Sanity Checks ----------------------------------------------------------------
# Get column names
names(macroData)

# Print head and Tail
print(head(macroData))
print(tail(macroData))

# Unit Root Tests: Main -------------------------------------------

# 1. Re-calculate the test results (Ensuring the object exists)
unit_root_triple_check <- macroData %>%
  select(-Date) %>%
  map_df(~ {
    clean_x <- na.omit(.x)
    suppressWarnings({
      data.frame(
        ADF_p  = adf.test(clean_x)$p.value,
        PP_p   = pp.test(clean_x)$p.value,
        KPSS_p = kpss.test(clean_x)$p.value
      )
    })
  }, .id = "Variable")

# 2. Format into a Formal Long-Form Table
formal_unit_root_table <- unit_root_triple_check %>%
  pivot_longer(cols = c(ADF_p, PP_p, KPSS_p), 
               names_to = "Test", 
               values_to = "p_value") %>%
  mutate(
    # Standardize Test Names
    Test = case_when(
      Test == "ADF_p"  ~ "ADF",
      Test == "PP_p"   ~ "Phillips-Perron",
      Test == "KPSS_p" ~ "KPSS"
    ),
    # Define Null Hypotheses (Crucial for clarity)
    Null_Hypothesis = case_when(
      Test == "KPSS" ~ "H0: Stationary",
      TRUE           ~ "H0: Unit Root"
    ),
    # Logic for Decision at 5% Significance
    Decision = case_when(
      Test == "KPSS" & p_value < 0.05 ~ "Reject H0 (Non-Stationary)",
      Test == "KPSS" & p_value >= 0.05 ~ "Fail to Reject (Stationary)",
      p_value < 0.05 ~ "Reject H0 (Stationary)",
      p_value >= 0.05 ~ "Fail to Reject (Non-Stationary)"
    ),
    # Formatting for professional presentation
    p_value_display = ifelse(p_value <= 0.01, "< 0.01", as.character(round(p_value, 4)))
  ) %>%
  # Reorder columns for a formal report look
  select(Variable, Test, Null_Hypothesis, p_value_display, Decision)

# 3. Print result
print(as.data.frame(formal_unit_root_table))


# Create a cleaner version for a document ------------------------
formal_unit_root_table %>%
  kable(caption = "Unit Root Test Results (IHS Transformed Data)", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  collapse_rows(columns = 1, valign = "top")



