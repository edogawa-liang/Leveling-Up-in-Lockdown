library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(scales)
library(tidyverse)


# ------------------------------------------------------------
# Data loading
# ------------------------------------------------------------

clean_data <- read_csv("Leveling_Up_in_Lockdown/data/clean_data.csv")
unified_data <- read_csv("Leveling_Up_in_Lockdown/data/unified_data.csv")
# View(unified_data)

use_countries <- c('SWE','TWN', 'GBR', 'USA')
use_goals <- c("Total_ARPDAU", "Total_DAU", "Total_Downloads", "Total_Revenue")

# ------------------------------------------------------------
# Create output directory for EDA plots
# ------------------------------------------------------------
eda_dir <- "eda"
if (!dir.exists(eda_dir)) {
  dir.create(eda_dir, recursive = TRUE)
}


# ------------------------------------------------------------
# Descriptive Plots
# ------------------------------------------------------------

# ------------------------------------------------------------
# Correlation Plot
# ------------------------------------------------------------

plot_correlation <- function(
    data,
    vars,
    title = "Correlation Matrix",
    method = "pearson",
    digits = 2,
    label_size = 3
) {
  library(ggplot2)
  
  # corr matrux
  cor_mat <- cor(
    data[, vars, drop = FALSE],
    use = "pairwise.complete.obs",
    method = method
  )
  
  cor_df <- as.data.frame(as.table(cor_mat))
  colnames(cor_df) <- c("Var1", "Var2", "Correlation")
  
  ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, digits)), size = label_size) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, limits = c(-1, 1)
    ) +
    coord_fixed() +
    labs(
      title = title,
      x = "Features",
      y = "Features",
      fill = "Correlation"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

vars <- c(
  "Android_DAU", "iPhone_DAU", "iPad_DAU",
  "Android_ARPDAU", "iPhone_ARPDAU", "iPad_ARPDAU"
)


downloads_vars <- c("Android_Downloads", "iPhone_Downloads", "iPad_Downloads")
revenue_vars   <- c("Android_Revenue",   "iPhone_Revenue",   "iPad_Revenue")
plot_correlation(clean_data, downloads_vars, title = "Correlation Matrix: Downloads")
plot_correlation(clean_data, revenue_vars,   title = "Correlation Matrix: Revenue")
plot_correlation(clean_data, vars, title = "Correlation Matrix: DAU & ARPDAU")

# ------------------------------------------------------------
# boxplot
# ------------------------------------------------------------

vars <- c(
  "Total_ARPDAU",
  "Total_DAU",
  "Total_Downloads",
  "Total_Revenue"
)

long_df <- unified_data %>%
  select(Country, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "Metric",
    values_to = "Value"
  )
ggplot(long_df, aes(x = Value, y = Country)) +
  geom_boxplot(
    outlier.size = 1,
    fill = "white",
    color = "black"
  ) +
  facet_wrap(~ Metric, scales = "free_x", nrow = 1) +
  labs(
    x = "value",
    y = "Country"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey85", color = "grey60"),
    strip.text = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# Histogram
# ------------------------------------------------------------

plot_country_hist <- function(data, country_code, vars, bins = 30) {
  long_df <- data %>%
    filter(Country == country_code) %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
  
  long_df$Metric <- factor(long_df$Metric, levels = vars)
  
  ggplot(long_df, aes(x = Value)) +
    geom_histogram(bins = bins, fill = "grey40", color = "black") +
    facet_wrap(~ Metric, scales = "free", nrow = 1) +
    labs(
      title = paste("Distributions for", country_code),
      x = "Value",
      y = "Frequency"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      strip.background = element_rect(fill = "grey85", color = "grey60"),
      strip.text = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8)
    )
}

plot_country_hist(unified_data, "USA", vars)
plot_country_hist(unified_data, "GBR", vars)
plot_country_hist(unified_data, "SWE", vars)
plot_country_hist(unified_data, "TWN", vars)


# ------------------------------------------------------------
# Advanced EDA
# ------------------------------------------------------------

# 1. Raw time series overlay:
  # Left axis = business metrics
  # Right axis = Stringency Index

plot_overlay_ts_facet_left <- function(
    data,
    country_name,
    y_left_vars = c("Total_Revenue", "Total_DAU"),
    y_right_var = "StringencyIndex",
    log_vars = NULL,
    smooth_k = 7,
    show_intervention = TRUE,
    period = c("all", "intervention") 
) {
  
  metric_colors <- c(
    "Total_ARPDAU"    = "#F8766D", 
    "Total_DAU"       = "#7CAE00", 
    "Total_Downloads" = "#00BFC4", 
    "Total_Revenue"   = "#C77CFF"  
  )
  metric_order <- c("Total_ARPDAU", "Total_DAU", "Total_Downloads", "Total_Revenue")
  
  period <- match.arg(period)
  
  # Filter country and analysis period
  country_data <- data %>%
    filter(Country == country_name) %>%
    {
      if (period == "intervention") {
        filter(., Intervention_BinaryVar == 1)
      } else {
        .
      }
    }
  has_intervention <- show_intervention && ("Intervention_BinaryVar" %in% names(country_data))
  
  # Check required variables
  need_cols <- c("Date", y_left_vars, y_right_var, if (has_intervention) "Intervention_BinaryVar")
  miss <- setdiff(need_cols, names(country_data))
  if (length(miss) > 0) stop(paste("Missing columns:", paste(miss, collapse = ", ")))
  
  # Left-axis variables (long format)
  left_long <- country_data %>%
    select(Date, all_of(y_left_vars)) %>%
    pivot_longer(cols = -Date, names_to = "LeftVar", values_to = "LeftValue") %>%
    mutate(
      LeftVar = factor(LeftVar, levels = metric_order),
      LeftValue = if (is.null(log_vars)) LeftValue else if_else(LeftVar %in% log_vars, log1p(LeftValue), LeftValue)
    ) %>%
    group_by(LeftVar) %>%
    arrange(Date) %>%
    mutate(
      LeftSmooth = if (is.null(smooth_k) || smooth_k <= 1) LeftValue
      else zoo::rollmean(LeftValue, k = smooth_k, fill = NA, align = "right")
    ) %>%
    ungroup()
  
  # Right-axis variable
  right_ts <- country_data %>%
    select(Date, RightValue = all_of(y_right_var)) %>%
    arrange(Date) %>%
    mutate(
      RightSmooth = if (is.null(smooth_k) || smooth_k <= 1) RightValue
      else zoo::rollmean(RightValue, k = smooth_k, fill = NA, align = "right")
    )
  
  # Align left and right series
  plot_long <- left_long %>%
    left_join(right_ts, by = "Date") %>%
    filter(!is.na(LeftSmooth), !is.na(RightSmooth))
  
  # Panel-specific rescaling of right axis
  scale_tbl <- plot_long %>%
    group_by(LeftVar) %>%
    summarise(
      left_min = min(LeftSmooth, na.rm = TRUE),
      left_max = max(LeftSmooth, na.rm = TRUE),
      right_min = min(RightSmooth, na.rm = TRUE),
      right_max = max(RightSmooth, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      scale = (left_max - left_min) / (right_max - right_min),
      shift = left_min - right_min * scale
    )
  
  plot_long <- plot_long %>%
    left_join(scale_tbl, by = "LeftVar") %>%
    mutate(RightPlot = RightSmooth * scale + shift)
  
  # intervention blocks
  intervention_blocks <- NULL
  if (has_intervention) {
    intervention_blocks <- country_data %>%
      filter(Intervention_BinaryVar == 1) %>%
      arrange(Date) %>%
      mutate(gap = c(FALSE, diff(Date) > 1),
             block_id = cumsum(gap)) %>%
      group_by(block_id) %>%
      summarise(xmin = min(Date), xmax = max(Date), .groups = "drop")
  }
  
  # Plot
  p <- ggplot(plot_long, aes(x = Date)) +
    facet_wrap(~ LeftVar, ncol = 1, scales = "free_y") +
    labs(
      title = paste("Overlay:", country_name),
      subtitle = paste0(
        ifelse(is.null(smooth_k) || smooth_k <= 1, "Raw", paste0(smooth_k, "-Day Rolling Avg")),
        " | Dashed = ", y_right_var,
        if (has_intervention) " | Red Zones = Intervention" else ""
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  if (!is.null(intervention_blocks) && nrow(intervention_blocks) > 0) {
    p <- p +
      geom_rect(
        data = intervention_blocks,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "red", alpha = 0.1
      )
  }
  
  p <- p +
    geom_line(aes(y = LeftSmooth, color = LeftVar), linewidth = 0.7) +
    geom_line(aes(y = RightPlot), linewidth = 0.7, linetype = "dashed", color = "black") +
    scale_color_manual(values = metric_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()))
  
  return(p)
}

# Plot every country
for (country in use_countries){
  p<- plot_overlay_ts_facet_left(
      unified_data, country,
      y_left_vars = c("Total_ARPDAU", "Total_DAU", "Total_Downloads", "Total_Revenue"),
      y_right_var = "StringencyIndex",
      smooth_k = 7,
      # log_vars = c("Total_Revenue")  
      period = "all"
    )
  print(p)
  
  fname <- file.path(
    eda_dir,
    paste0("overlay_ts_", country, ".png")
  )
  
  ggsave(
    filename = fname,
    plot = p,
    width = 10,
    height = 12,
    dpi = 300
  )
}



# ---------------------------------------------------------------
# 2. Cross-Correlation Function (CCF)
  # To determine the optimal lag structure

plot_ccf_multi <- function(
    data,
    country_name,
    y_vars = c("Total_Revenue", "Total_DAU"),
    x_var = "StringencyIndex",
    max_lag = 30,
    log_vars = NULL,
    facet = FALSE,
    period = c("all", "intervention")   # Analysis window
) {
  
  metric_colors <- c(
    "Total_ARPDAU"    = "#F8766D",
    "Total_DAU"       = "#7CAE00",
    "Total_Downloads" = "#00BFC4",
    "Total_Revenue"   = "#C77CFF"
  )
  
  # Filter country and time window
  period <- match.arg(period)
  
  country_data <- data %>%
    filter(Country == country_name) %>%
    {
      if (period == "intervention") {
        filter(., Intervention_BinaryVar == 1)
      } else {
        .
      }
    }
  
  # Check required variables
  miss <- setdiff(c(y_vars, x_var), names(country_data))
  if (length(miss) > 0) stop(paste("Missing columns:", paste(miss, collapse = ", ")))
  
  # Compute CCF for each y_var against x_var
  ccf_all <- list()
  for (y_var in y_vars) {
    
    tmp <- country_data %>%
      select(Date, y = all_of(y_var), x = all_of(x_var)) %>%
      arrange(Date)
    
    y <- tmp$y
    x <- tmp$x
    
    # optional log
    if (!is.null(log_vars) && y_var %in% log_vars) {
      y <- log1p(y)
    }
    
    # Drop missing pairs
    keep <- complete.cases(y, x)
    y <- y[keep]
    x <- x[keep]
    
    # CCF computation (no base plot)
    ccf_res <- ccf(x, y, lag.max = max_lag, plot = FALSE)
    
    ccf_all[[y_var]] <- data.frame(
      lag = ccf_res$lag,
      ccf = ccf_res$acf,
      Metric = y_var
    )
  }
  
  ccf_df <- bind_rows(ccf_all)
  
  # Approx. significance bounds (white-noise assumption)
  n <- nrow(country_data)
  ci <- 1.96 / sqrt(n)
  
  p <- ggplot(ccf_df, aes(x = lag, y = ccf, color = Metric)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "grey60") +
    geom_segment(aes(xend = lag, yend = 0), linewidth = 0.9) +
    scale_color_manual(values = metric_colors) +
    labs(
      title = paste("Cross-Correlation Function:", country_name),
      subtitle = paste0(
        paste(y_vars, collapse = ", "),
        " vs ", x_var,
        if (!is.null(log_vars)) paste0(" | log1p: ", paste(log_vars, collapse = ", ")) else ""
      ),
      x = "Lag (days)",
      y = "Cross-correlation"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = if (facet) "none" else "right"
    )
  
  if (facet) {
    p <- p + facet_wrap(~ Metric, ncol = 1)
  }
  
  return(p)
}


# Plot every country
for (country in use_countries){
  p<- plot_ccf_multi(
      unified_data,
      country_name = country,
      y_vars = c("Total_ARPDAU", "Total_DAU", "Total_Downloads", "Total_Revenue"),
      x_var = "StringencyIndex",
      max_lag = 30,
      log_vars = c("Total_Revenue"),
      facet = TRUE,    
      period = "intervention" # only use the intervention period; otherwise, the effect might be diluted.
      # period = "all" # all
    )
  print(p)
  
  fname <- file.path(
    eda_dir,
    paste0("ccf_", country, "_intervention.png")
  )
  
  ggsave(
    filename = fname,
    plot = p,
    width = 9,
    height = 12,
    dpi = 300
  )
}


# ---------------------------------------------------------------

# 3. Seasonal Sub-Series Plots
  # Decomposes monthly series into Trend / Seasonal / Remainder to assess whether a 12-month seasonal component is needed.

plot_monthly_stl_decomp <- function(
    data,
    country_name,
    y_vars = c("Total_Revenue", "Total_DAU"),
    date_var = "Date",
    country_var = "Country",
    log_vars = NULL,              # Variables to log1p before STL
    robust = TRUE                 # Robust STL (downweights outliers)
) {
  
  metric_colors <- c(
    "Total_ARPDAU"    = "#F8766D", 
    "Total_DAU"       = "#7CAE00", 
    "Total_Downloads" = "#00BFC4", 
    "Total_Revenue"   = "#C77CFF"  
  )
  metric_order <- c("Total_ARPDAU", "Total_DAU", "Total_Downloads", "Total_Revenue")
  
  # Filter country
  df0 <- data %>%
    filter(.data[[country_var]] == country_name) %>%
    mutate(Date = as.Date(.data[[date_var]]))
  
  # Check required variables
  miss <- setdiff(y_vars, names(df0))
  if (length(miss) > 0) stop(paste("Missing columns:", paste(miss, collapse = ", ")))
  
  # Monthly aggregation + STL per variable
  out_list <- list()
  
  for (v in y_vars) {
    
    monthly <- df0 %>%
      mutate(month = lubridate::floor_date(Date, "month")) %>%
      group_by(month) %>%
      summarise(Value = sum(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
      arrange(month)
    
    if (!is.null(log_vars) && v %in% log_vars) {
      monthly <- monthly %>% mutate(Value = log1p(Value))
    }
    
    # STL on monthly ts (frequency = 12)
    ts_y <- ts(monthly$Value, frequency = 12)
    
    stl_fit <- stl(ts_y, s.window = "periodic", robust = robust)
    comp <- stl_fit$time.series
    
    out_list[[v]] <- data.frame(
      Date = monthly$month,
      Metric = v,
      Trend = as.numeric(comp[, "trend"]),
      Seasonal = as.numeric(comp[, "seasonal"]),
      Remainder = as.numeric(comp[, "remainder"])
    )
  }
  
  plot_df <- bind_rows(out_list) %>%
    mutate(
      Metric = factor(Metric, levels = metric_order),
    ) %>%
    pivot_longer(cols = c("Trend", "Seasonal", "Remainder"),
                 names_to = "Component", values_to = "Value") %>%
    mutate(Component = factor(Component, levels = c("Trend", "Seasonal", "Remainder")))
  
  # Plot: rows = components, columns = metrics
  ggplot(plot_df, aes(x = Date, y = Value, color = Metric)) +
    facet_grid(Component ~ Metric, scales = "free_y") +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = metric_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    labs(
      title = paste("Monthly STL Decomposition:", country_name),
      subtitle = paste0(
        "Trend / Seasonal / Remainder (monthly aggregation, frequency = 12)",
        if (!is.null(log_vars)) paste0(" | log1p: ", paste(log_vars, collapse = ", ")) else ""
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(size = 11),
      strip.text.y = element_text(size = 11)
    )
}

# Plot every country
for (country in use_countries){
  for (goal in use_goals){
    p <- plot_monthly_stl_decomp(
        unified_data,
        country_name = country,
        y_vars = c(goal)#, "Total_Downloads", "Total_DAU", "Total_ARPDAU"),
        # log_vars = c("Total_Revenue")  
      )
    print(p)
    
    fname <- file.path(
      eda_dir,
      paste0("stl_", country, "_", goal, ".png")
    )
    
    ggsave(
      filename = fname,
      plot = p,
      width = 12,
      height = 6,
      dpi = 300
  }
}









