## ----warning=FALSE, message=FALSE-----------------------------
library(dplyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(bsts)
library(CausalImpact)
library(ragg)
library(ggplot2)
library(knitr) # For nice table formatting

# Set seed for reproducibility
set.seed(10101)


## A) Generate synthetic data
## -------------------------------------------------------------
generate_synthetic_data <- function(
    n_months = 120,          # 10 years of data
    start_date = "2015-01-01",
    intervention_idx = 80,   # Month where intervention starts
    base_level = 1000,       # Intercept
    trend_slope = 5,         # Assumed growth rate
    seasonal_amp = 100,      # Strength of seasonality
    effect_size = 200,       # True causal impact (lift)
    effect_type = "constant",# "constant" or "decay"
    noise_sd = 20            # Random noise level
) {
  
  dates <- seq(from = as.Date(start_date), by = "month", length.out = n_months)
  
  # Construct data using tidyverse style
  df <- tibble(time_idx = 1:n_months) %>%
    mutate(
      Date = dates,
      Country = "SimLand", 
      
      # 1. Trend Component (Linear)
      trend = base_level + (trend_slope * time_idx),
      
      # 2. Seasonality Component (12-month sine wave)
      seasonal = seasonal_amp * sin(2 * pi * time_idx / 12),
      
      # 3. Causal Effect Component
      effect = case_when(
        time_idx < intervention_idx ~ 0,
        effect_type == "constant" ~ effect_size,
        effect_type == "decay" ~ effect_size * (0.95 ^ (time_idx - intervention_idx)), # 5% decay per month
        TRUE ~ 0
      ),
      
      # 4. Random Noise Component
      noise = rnorm(n_months, mean = 0, sd = noise_sd),
      
      # Combine to create observed 'y'
      y = trend + seasonal + effect + noise,
      
      # Flag for intervention
      Intervention_BinaryVar = ifelse(time_idx >= intervention_idx, 1, 0)
    )
  
  list(
    data = df,
    params = list(
      effect_size = effect_size,
      effect_type = effect_type,
      noise_sd = noise_sd,
      intervention_idx = intervention_idx
    )
  )
}


## B) Run Scenarios
## -------------------------------------------------------------
run_bsts_causalimpact <- function(
    data,
    target_col,
    # column names
    date_col = "Date",
    country_col = "Country",
    intervention_col = "Intervention_BinaryVar",
    country = NULL,
    analysis_start = NULL,
    post_start = NULL,                 # if NULL -> computed from intervention_col
    end_date   = NULL,                 # if NULL -> max(date)
    freq = "month",
    agg_fun = c("sum", "mean"),
    # model config
    nseasons = 12,
    use_local_linear_trend = TRUE,
    slope_sigma_mult = 0.001,
    slope_prior_sample_size = 10000,
    niter = 1000,
    seed  = 10101
) {
  agg_fun <- match.arg(agg_fun)
  
  # --- 0) country filter
  df <- data
  if (!is.null(country)) {
    df <- df %>% filter(.data[[country_col]] == country)
  }
  
  # --- 1) parse & basic checks
  df <- df %>% mutate(.date = as.Date(.data[[date_col]]))
  if (!target_col %in% names(df)) stop("target_col not found in data: ", target_col)
  
  # --- 1.5) cut by analysis_start
  if (!is.null(analysis_start)) {
    analysis_start <- as.Date(analysis_start)
    df <- df %>% filter(.date >= analysis_start)
  }
  
  # --- 2) compute post_start if needed
  if (is.null(post_start)) {
    if (!intervention_col %in% names(df)) stop("intervention_col not found in data: ", intervention_col)
    
    post_start <- df %>%
      filter(.data[[intervention_col]] == 1) %>%
      summarise(min_date = min(.date, na.rm = TRUE)) %>%
      pull(min_date)
    
    if (is.infinite(post_start) || is.na(post_start)) {
      stop("Cannot compute post_start: no rows where ", intervention_col, " == 1.")
    }
  } else {
    post_start <- as.Date(post_start)
  }
  
  # --- 3) compute end_date if needed
  if (is.null(end_date)) {
    end_date <- max(df$.date, na.rm = TRUE)
  } else {
    end_date <- as.Date(end_date)
  }
  
  # --- 4) aggregate to time series
  df_agg <- df %>%
    mutate(.period = floor_date(.date, unit = freq)) %>%
    group_by(.period) %>%
    summarise(
      y = if (agg_fun == "sum") sum(.data[[target_col]], na.rm = TRUE) else mean(.data[[target_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(.period)
  
  if (nrow(df_agg) < 5) stop("Too few aggregated time points after filtering/aggregation.")
  
  ts_zoo <- read.zoo(df_agg, index.column = 1)
  ts_zoo <- window(ts_zoo, end = end_date)
  
  # --- 5) training series: set post-period to NA
  y_train <- ts_zoo
  window(y_train, start = post_start) <- NA
  
  # --- 6) priors based on pre-period variability
  pre_end <- post_start - 1
  y_pre <- window(ts_zoo, end = pre_end)
  sd_pre <- sd(as.numeric(y_pre), na.rm = TRUE)
  if (is.na(sd_pre) || sd_pre == 0) sd_pre <- sd(as.numeric(ts_zoo), na.rm = TRUE)
  
  # --- 7) state specification of BSTS Model
  ss <- list()
  ss <- AddSeasonal(ss, y_train, nseasons = nseasons)
  
  if (use_local_linear_trend) {
    ss <- AddLocalLinearTrend(
      ss,
      y = y_train,
      slope.sigma.prior = SdPrior(
        sigma.guess = slope_sigma_mult * sd_pre,
        sample.size = slope_prior_sample_size
      )
    )
  } else {
    ss <- AddLocalLevel(ss, y_train)
  }
  
  # --- 8) fit bsts
  bsts_model <- bsts(
    y_train,
    state.specification = ss,
    niter = niter,
    seed  = seed,
    ping = niter / 2
  )
  
  # --- 9) causal impact
  y_post <- as.vector(window(ts_zoo, start = post_start, end = end_date))
  if (all(is.na(y_post))) stop("Post-period response is all NA. Check post_start/end_date.")
  
  impact <- CausalImpact(
    bsts.model = bsts_model,
    post.period.response = y_post,
  )
  
  list(
    country = country,
    target_col = target_col,
    analysis_start = analysis_start,
    post_start = post_start,
    end_date = end_date,
    freq = freq,
    agg_fun = agg_fun,
    sd_pre = sd_pre,
    ts_zoo = ts_zoo,
    y_train = y_train,
    model = bsts_model,
    impact = impact
  )
}


## Data loading
## -------------------------------------------------------------
unified_data <- read_csv("data/unified_data.csv")
# View(unified_data)


## -------------------------------------------------------------
# Initialize a results list to store data for the final table
all_results <- list()

# Helper to extract and store metrics
store_result <- function(scenario_name, res_obj, comment_expectation, time_taken = NA, n_iter = NA, true_val_for_check = NA) {
  summ <- res_obj$impact$summary
  
  # Absolute effect estimates (Average per time point)
  est_abs <- summ$AbsEffect[1]
  lower_abs <- summ$AbsEffect.lower[1]
  upper_abs <- summ$AbsEffect.upper[1]
  
  # Success is only calculated if we actually know the true value (Scenario 1)
  success <- if(!is.na(true_val_for_check)) {
      (true_val_for_check >= lower_abs) && (true_val_for_check <= upper_abs)
  } else {
      NA # Not applicable for real data
  }

  tibble(
    Scenario = scenario_name,
    Comments_Expectation = comment_expectation, # Flexible user comments
    Iterations = n_iter,
    Time_Sec = round(as.numeric(time_taken), 2),
    Est_Effect = round(est_abs, 2),
    CI_Lower = round(lower_abs, 2),
    CI_Upper = round(upper_abs, 2),
    Success = success
  )
}


## -------------------------------------------------------------
cat("\n", paste(rep("=", 50), collapse = ""), "\n",
    "BASELINE MODEL (USA)", "\n",
    paste(rep("=", 50), collapse = ""), "\n", sep = "")

t_start <- Sys.time()
# Using default parameters: niter=1000, slope_prior=10000
res_base <- run_bsts_causalimpact(
    data = unified_data[unified_data$Country == 'USA',], 
    target_col = "Total_Revenue", 
    intervention_col = "Intervention_BinaryVar",
    niter = 1000 # Default
)
t_end <- Sys.time()

# Store Baseline
all_results[[length(all_results) + 1]] <- store_result(
  "Baseline (Default)", 
  res_base, 
  "Params: Prior_Sigma=0.001, Prior_N=10000", 
  t_end - t_start, 
  1000
)


### 1. Ideal Model Verification
## **Goal: Verify that the package recovers the true effect size ($200)**
## -------------------------------------------------------------
cat("\n", paste(rep("=", 50), collapse = ""), "\n",
    "SCENARIO 1: Synthetic Data Model Verification", "\n",
    paste(rep("=", 50), collapse = ""), "\n", sep = "")

# 1. Generate Data
sim_obj_1 <- generate_synthetic_data(effect_size = 200, effect_type = "constant", noise_sd = 25)
simulated_data <- sim_obj_1$data
true_effect <- 200

# 2. Run Model
start_time <- Sys.time()
res_A <- run_bsts_causalimpact(
    data = simulated_data,
    target_col = "y",
    intervention_col = "Intervention_BinaryVar",
    niter = 1000
)
end_time <- Sys.time()

# 3. Plot & Store
plot(res_A$impact)
summary(res_A$impact)
all_results[[length(all_results) + 1]] <- store_result(
  "Scenario 1: Synthetic Data", 
  res_A, 
  "Expect: Recover True Effect (200)", 
  end_time - start_time, 
  2000,
  true_val_for_check = true_effect
)


### 2. MCMC Iteration Sensitivity
### **Goal: Compare stability and runtime between 500, 1000, and 10000 iterations**
## -------------------------------------------------------------
cat("\n", paste(rep("=", 50), collapse = ""), "\n",
    "SCENARIO 2: ITERATION SENSITIVITY", "\n",
    paste(rep("=", 50), collapse = ""), "\n", sep = "")

iter_counts <- c(500, 50000) 

for (n in iter_counts) {
  cat(paste0("\nRunning with N = ", n, "...\n"))
  
  t_start <- Sys.time()
  res_iter <- run_bsts_causalimpact(
      data = unified_data[unified_data$Country == 'USA',],
      target_col = "Total_Revenue",
      intervention_col = "Intervention_BinaryVar",
      niter = n
  )
  t_end <- Sys.time()
  
  # Define flexible comment based on N
  comment <- if(n == 500) "Fast, potential lower stability" else "Increased Time with little gain"
  
  # Store
  all_results[[length(all_results) + 1]] <- store_result(
    paste0("Scenario 2 (N=", n, ")"), 
    res_iter, 
    comment, 
    t_end - t_start, 
    n
  )
}


### 3. Prior Sensitivity (Standard Deviation)
### **Goal: Test impact of prior belief on posterior uncertainty**
## -------------------------------------------------------------
cat("\n", paste(rep("=", 50), collapse = ""), "\n",
    "SCENARIO 3: PRIOR SENSITIVITY", "\n",
    paste(rep("=", 50), collapse = ""), "\n", sep = "")

# A) Tight Prior (n=50)
t_start <- Sys.time()
res_tight <- run_bsts_causalimpact(
    data = unified_data[unified_data$Country == 'USA',], target_col = "Total_Revenue", intervention_col = "Intervention_BinaryVar",
    niter = 1000, slope_sigma_mult = 1e-6, slope_prior_sample_size = 50
)
t_end <- Sys.time()
all_results[[length(all_results) + 1]] <- store_result(
  "Scenario 3: Tight Prior (Prior_Sigma = 1e-6)", 
  res_tight, 
  "Strong belief, narrow CI", 
  t_end - t_start, 
  1000
)

# B) Loose Prior (n=1)
t_start <- Sys.time()
res_loose <- run_bsts_causalimpact(
    data = unified_data[unified_data$Country == 'USA',], target_col = "Total_Revenue", intervention_col = "Intervention_BinaryVar",
    niter = 1000, slope_sigma_mult = 1.0, slope_prior_sample_size = 1
)
t_end <- Sys.time()
all_results[[length(all_results) + 1]] <- store_result(
  "Scenario 3: Loose Prior (Prior_Sigma = 1)", 
  res_loose, 
  "Weak belief, wide CI", 
  t_end - t_start, 
  1000
)


## -------------------------------------------------------------
cat("\n", paste(rep("=", 50), collapse = ""), "\n",
    "SCENARIO 4: FLEXIBLE TREND (n=32)", "\n",
    paste(rep("=", 50), collapse = ""), "\n", sep = "")

t_start <- Sys.time()
res_flex <- run_bsts_causalimpact(
    data = unified_data[unified_data$Country == 'USA',], 
    target_col = "Total_Revenue", 
    intervention_col = "Intervention_BinaryVar",
    niter = 1000,
    slope_prior_sample_size = 32
)
t_end <- Sys.time()

# Plot to visualize the flexible trend
print(plot(res_flex$impact) + ggtitle("Scenario 4: Flexible Trend (n=32)"))

all_results[[length(all_results) + 1]] <- store_result(
  "Scenario 4: Flexible Trend (Prior_N=32)", 
  res_flex, 
  "High flexibility", 
  t_end - t_start, 
  1000
)


## Final Results Comparison
## -------------------------------------------------------------
final_table <- bind_rows(all_results)
print(kable(final_table, caption = "Simulation & Sensitivity Results Summary"))



## -------------------------------------------------------------
# knitr::purl("simulation.Rmd", output = "simulation.R")


