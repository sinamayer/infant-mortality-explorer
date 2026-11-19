# =============================================================================
# Final Project – Infant Mortality & The Feminist Hypothesis
# GDA – Sina-Marie Mayer
#
# Research Question:
#   Are maternal empowerment indicators stronger predictors of infant mortality
#   than GDP per capita across countries?
#
# PART 1 - DATA COLLECTION, CLEANING & REGRESSION ANALYSIS
#
# Run from the repository folder. Writes clean_data.rds, which the app reads.
# =============================================================================
library(tidyverse)
library(WDI)

# -----------------------------------------------------------------------------
# 1. DATA
# -----------------------------------------------------------------------------

my_indicators <- c(
  infant_mortality = "SP.DYN.IMRT.IN",    # outcome: deaths per 1,000 live births
  gdp_per_cap      = "NY.GDP.PCAP.PP.KD", # baseline predictor
  female_lit       = "SE.ADT.LITR.FE.ZS", # feminist variable (adult female literacy 15+)
  adol_fertility   = "SP.ADO.TFRT",       # feminist variable
  female_labor     = "SL.TLF.CACT.FE.ZS", # feminist variable
  # Controls (Caldwell 1986; Vollmer et al. 2014 point to health systems)
  health_expend    = "SH.XPD.CHEX.PC.CD", # current health expenditure per capita (USD)
  sanitation       = "SH.STA.BASS.ZS",    # basic sanitation access (%)
  urban_pop        = "SP.URB.TOTL.IN.ZS"  # urban population (% of total)
)

# Pull 2015–2022 so gaps can be filled with the most recent available value.
# Many indicators (female literacy, sanitation) are only reported every few years.
raw_data <- WDI(
  indicator = my_indicators,
  country   = "all",
  start     = 2015,
  end       = 2022,
  extra     = TRUE
)

# For each country keep the most recent non-NA observation per variable
fill_latest <- function(df, vars) {
  df %>%
    arrange(iso3c, desc(year)) %>%
    group_by(iso3c) %>%
    summarise(
      across(all_of(vars), ~ first(na.omit(.x))),
      country = first(country),
      region  = first(region),
      income  = first(income),
      .groups = "drop"
    )
}

# Save
saveRDS(raw_data, "raw_data.rds")

# De-comment to load data
# raw_data <- readRDS("raw_data.rds")

all_vars <- c("infant_mortality", "gdp_per_cap", "female_lit", "adol_fertility",
              "female_labor", "health_expend", "sanitation", "urban_pop")

clean_data <- raw_data %>%
  filter(region != "Aggregates") %>%
  fill_latest(all_vars) %>%
  # High-income countries don't report female literacy because it's ~universal;
  # impute 99 so they aren't dropped (US, EU, Australia, Canada, etc.)
  mutate(female_lit = ifelse(is.na(female_lit) & income == "High income", 99, female_lit)) %>%
  drop_na(all_of(all_vars)) %>%
  filter(
    infant_mortality > 0,
    gdp_per_cap      > 0,
    adol_fertility   > 0,
    female_labor     >= 0,
    health_expend    > 0,
    sanitation       >= 0,  sanitation       <= 100,
    urban_pop        >= 0,  urban_pop        <= 100,
    female_lit       >= 0,  female_lit       <= 100
  )

cat("Countries in analysis:", nrow(clean_data), "\n")

# Round predictors to match slider precision so actual = predicted on first load
clean_data <- clean_data %>%
  mutate(
    gdp_per_cap    = round(gdp_per_cap / 100) * 100,
    female_lit     = round(female_lit),
    adol_fertility = round(adol_fertility),
    female_labor   = round(female_labor),
    health_expend  = round(health_expend / 10) * 10,
    sanitation     = round(sanitation),
    urban_pop      = round(urban_pop)
  )

# summary stats
summary(clean_data)

# To get the row with the lowest infant mortality
min_row <- clean_data[which.min(clean_data$infant_mortality), ]

# To get the row with the highest infant mortality
max_row <- clean_data[which.max(clean_data$infant_mortality), ]

# Save
saveRDS(clean_data, "clean_data.rds")

# Reload
# clean_data <- readRDS("clean_data.rds")

# -----------------------------------------------------------------------------
# 2. REGRESSION MODELS
# -----------------------------------------------------------------------------

# Model 1: Baseline — GDP alone
model_baseline <- lm(infant_mortality ~ gdp_per_cap, data = clean_data)
cat("\n--- Baseline Model (GDP per capita only) ---\n")
print(summary(model_baseline))

# Model 2: Full model — feminist variables + controls
# (Caldwell 1986 motivates female_lit; Vollmer et al. 2014 motivate health_expend & sanitation)
model_full <- lm(
  infant_mortality ~ gdp_per_cap + female_lit + adol_fertility +
    female_labor + health_expend + sanitation + urban_pop,
  data = clean_data
)
cat("\n--- Full Multiple Regression Model ---\n")
print(summary(model_full))

cat(sprintf(
  "\nAdjusted R² improvement: baseline = %.3f → full model = %.3f\n",
  summary(model_baseline)$adj.r.squared,
  summary(model_full)$adj.r.squared
))
