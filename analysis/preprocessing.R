# ------------------------------------------------------------
# Data loading
# ------------------------------------------------------------

# File paths
Sensortower_file_path <- 'Leveling_Up_in_Lockdown/data/Sensor_Tower_Gaming_Insights_Market_Analysis_By_Country_Unified_All_Countries_2017-01-01_2025-11-19_daily.csv'
Oxcgrt_file_path <- 'Leveling_Up_in_Lockdown/data/OxCGRT_compact_national_v1.csv'

# Read raw data
sensortower_raw_data <- read_csv(Sensortower_file_path, na = c("", "NA", "#DIV/0!"), show_col_types = FALSE)
oxcgrt_raw_data <- read_csv(Oxcgrt_file_path, na = c("", "NA", "#DIV/0!"), show_col_types = FALSE)


# ------------------------------------------------------------
# OxCGRT preprocessing
# ------------------------------------------------------------

clean_stringency_data <- oxcgrt_raw_data %>%
  filter(CountryCode %in% c('SWE','TWN', 'GBR', 'USA')) %>% # Filter for relevant countries
  select(Date = Date, CountryCode = CountryCode, StringencyIndex = StringencyIndex_Average) %>% # Select relevant columns
  mutate(Date = ymd(Date), # Parse Date
         CountryCode = as.factor(CountryCode), # Convert CountryCode to factor
         StringencyIndex = as.numeric(StringencyIndex)) # Convert StringencyIndex to numeric
# View(clean_stringency_data)


# ------------------------------------------------------------
# SensorTower preprocessing
# ------------------------------------------------------------

clean_sensortower_data <- sensortower_raw_data %>%
  mutate(Date = dmy(Date)) %>% # Parse Date
  mutate(Country = as.factor(Country)) %>% # Convert Country to factor
  mutate(across(contains("Downloads") | contains("Revenue") | contains("DAU") | contains("ARPDAU"), as.numeric)) %>% # Convert key metrics to numeric
  filter(Date < as.Date("2025-11-19")) %>% # Remove incomplete data at 2025-11-19 
  filter(Country %in% c("US", "GB", "SE", "TW")) %>% # Filter for relevant countries
  mutate(Country = fct_recode(Country, "USA" = "US", "GBR" = "GB", "TWN" = "TW", "SWE" = "SE")) %>% # Rename country codes to be consistent 
  rename_with(~ str_replace_all(., " ", "_")) # Clean column names
# View(clean_sensortower_data)


# ------------------------------------------------------------
# Merge datasets and define intervention indicator
# ------------------------------------------------------------

# Step 1: Compute country-specific intervention thresholds

# For each country, we calculate the median of the non-zero Stringency Index values. 
# This median is used as a country-specific threshold to define major intervention periods.
country_thresholds <- clean_stringency_data %>%
  group_by(CountryCode) %>%
  summarise(
    stringency_median_nonzero = median(StringencyIndex[StringencyIndex > 0], na.rm = TRUE),
    n_nonzero = sum(StringencyIndex > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Print the country-specific thresholds
print(country_thresholds$CountryCode)
print(country_thresholds)
# View(country_thresholds)


# The intervention period is defined as a continuous interval for each country, starting from the first date on which the Stringency Index exceeds the country-specific median (computed from non-zero values), and ending at the last date on which it remains above this threshold. All observations within this interval are classified as intervention periods.

# Step 2: Merge thresholds and define the intervention indicator

library(dplyr)

clean_data <- clean_sensortower_data %>%
  left_join(
    clean_stringency_data,
    by = c("Date" = "Date", "Country" = "CountryCode")
  ) %>%
  mutate(StringencyIndex = replace_na(StringencyIndex, 0)) %>%
  left_join(
    country_thresholds,
    by = c("Country" = "CountryCode")
  ) %>%
  group_by(Country) %>%
  mutate(
    # First time exceed median
    intervention_start = ifelse(
      any(StringencyIndex > stringency_median_nonzero),
      min(Date[StringencyIndex > stringency_median_nonzero]),
      as.Date(NA)
    ),
    # Last time exceed median
    intervention_end = ifelse(
      any(StringencyIndex > stringency_median_nonzero),
      max(Date[StringencyIndex > stringency_median_nonzero]),
      as.Date(NA)
    ),
    # mark as "1" (intervention) between first and last time
    Intervention_BinaryVar = ifelse(
      !is.na(intervention_start) &
        Date >= intervention_start &
        Date <= intervention_end,
      1, 0
    )
  ) %>%
  ungroup()

write_csv(clean_data, "Leveling_Up_in_Lockdown/data/clean_data.csv")


# Original
# clean_data <- clean_sensortower_data %>%
#   left_join(clean_stringency_data, by = c("Date" = "Date", "Country" = "CountryCode")) %>%
#   mutate(StringencyIndex = replace_na(StringencyIndex, 0)) %>% # Replace missing Stringency with 0 (no measures)
#   mutate(Intervention_BinaryVar = ifelse(StringencyIndex > 43, 1, 0)) # Flag for major interventions
# View(clean_data)
# # why 43?



# ------------------------------------------------------------
# Aggregate across platforms
# ------------------------------------------------------------

unified_data <- clean_data %>%
  mutate(
    Total_Downloads = iPad_Downloads + iPhone_Downloads + Android_Downloads,
    Total_Revenue = iPad_Revenue + iPhone_Revenue + Android_Revenue,
    Total_DAU = iPad_DAU + iPhone_DAU + Android_DAU, 
    Total_ARPDAU = ifelse(Total_DAU > 0, Total_Revenue / Total_DAU, 0),
    Intervention_BinaryVar = Intervention_BinaryVar
  ) %>%
  select(Date, Country, Total_Downloads, Total_Revenue, Total_DAU, Total_ARPDAU, StringencyIndex, Intervention_BinaryVar)
View(unified_data)


# ------------------------------------------------------------
# Output Data
# ------------------------------------------------------------
write_csv(unified_data, "Leveling_Up_in_Lockdown/data/unified_data.csv")

