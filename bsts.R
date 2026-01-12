library(dplyr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(lubridate)
library(zoo)
library(bsts)
library(CausalImpact)
# install.packages("ragg")
library(ragg)


## Data loading
## -------------------------------------------------------------
raw_unified_data <- read_csv("data/unified_data.csv")
unified_data <- raw_unified_data[raw_unified_data$Date<"2025-11-01",] # Removing incomplete data
# View(unified_data)


## A) Detect pandemic window
## -------------------------------------------------------------

get_pandemic_window <- function(
    data,
    method = c("binary", "stringency"), # detection method
    date_col = "Date",
    country_col = NULL,
    country = NULL,
    intervention_col = "Intervention_BinaryVar",
    stringency_col = "StringencyIndex",
    stringency_nonzero_threshold = 0
) {
  method <- match.arg(method)
  
  df <- data
  if (!is.null(country_col) && !is.null(country)) {
    df <- df %>% filter(.data[[country_col]] == country)
  }
  
  df <- df %>%
    mutate(.date = as.Date(.data[[date_col]])) %>%
    arrange(.date)
  
  first_date <- min(df$.date, na.rm = TRUE)
  last_date  <- max(df$.date, na.rm = TRUE)
  
  if (method == "binary") {
    start_date <- df %>%
      filter(.data[[intervention_col]] == 1) %>%
      summarise(d = min(.date, na.rm = TRUE)) %>%
      pull(d)
    if (is.na(start_date) || is.infinite(start_date)) {
      stop("No start_date found: ", intervention_col, " never equals 1.")
    }
    
    end_date <- df %>%
      filter(.date > start_date) %>%
      filter(.data[[intervention_col]] == 0) %>%
      summarise(d = min(.date, na.rm = TRUE)) %>%
      pull(d)
    
  } else { # stringency
    df <- df %>% mutate(.s = as.numeric(.data[[stringency_col]]))
    
    start_date <- df %>%
      filter(!is.na(.s) & (.s != stringency_nonzero_threshold)) %>%
      summarise(d = min(.date, na.rm = TRUE)) %>%
      pull(d)
    if (is.na(start_date) || is.infinite(start_date)) {
      stop("No start_date found: ", stringency_col, " never becomes non-zero.")
    }
    
    end_date <- df %>%
      filter(.date > start_date) %>%
      filter(!is.na(.s) & (.s == stringency_nonzero_threshold)) %>%
      summarise(d = min(.date, na.rm = TRUE)) %>%
      pull(d)
  }
  
  list(
    method = method,
    country = country,
    first_date = first_date,
    last_date = last_date,
    start_date = start_date,
    end_date = end_date
  )
}


## B) Build pre- and post-intervention periods
## -------------------------------------------------------------
build_causal_periods <- function(
    data,
    method = c("binary", "stringency"),
    date_col = "Date",
    country_col = NULL,
    country = NULL,
    intervention_col = "Intervention_BinaryVar",
    stringency_col = "StringencyIndex",
    stringency_nonzero_threshold = 0,
    end_date_setting = c("max_date", "intervention_end"),
    analysis_start_manual = NULL,
    post_start_manual = NULL,
    end_date_manual = NULL
) {
  method <- match.arg(method)
  end_date_setting <- match.arg(end_date_setting)
  
  df <- data
  if (!is.null(country_col) && !is.null(country)) {
    df <- df %>% filter(.data[[country_col]] == country)
  }
  df <- df %>% mutate(.date = as.Date(.data[[date_col]])) %>% arrange(.date)
  first_date <- min(df$.date, na.rm = TRUE)
  last_date  <- max(df$.date, na.rm = TRUE)
  
  
  if (!is.null(post_start_manual)) {
    post_start_manual <- as.Date(post_start_manual)
    
    # analysis_start and end date
    analysis_start <- if (!is.null(analysis_start_manual)) as.Date(analysis_start_manual) else first_date
    end_date <- if (!is.null(end_date_manual)) as.Date(end_date_manual) else last_date
    
    return(list(
      mode = "manual",
      method = NA,
      analysis_start = analysis_start,
      post_start = post_start_manual,
      end_date = end_date,
      first_date = first_date,
      last_date = last_date
    ))
  }
  
  # Automatic detection
  w <- get_pandemic_window(
    data = data,
    method = method,
    date_col = date_col,
    country_col = country_col,
    country = country,
    intervention_col = intervention_col,
    stringency_col = stringency_col,
    stringency_nonzero_threshold = stringency_nonzero_threshold
  )
  
  # Determine analysis end date based on setting
  calc_end_date <- w$last_date
  
  if (end_date_setting == "intervention_end") {
    if (!is.na(w$end_date)) {
       calc_end_date <- w$end_date - 1
    } 
    # If is.na(w$end_date), it means intervention never ended, so we keep w$last_date
  }

  return(list(
    mode = "auto",
    method = method,
    analysis_start = w$first_date,
    post_start = w$start_date,
    end_date = calc_end_date,
    first_date = w$first_date,
    last_date = w$last_date,
    start_date = w$start_date,
    end_date_detected = w$end_date
  ))
}



## C) Run BSTS + CausalImpact for one country and target
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


## D) Batch runner (countries × targets)
## -------------------------------------------------------------
run_bsts_batch <- function(
    data,
    countries,
    targets,
    method = c("binary", "stringency"),
    date_col = "Date",
    country_col = "Country",
    intervention_col = "Intervention_BinaryVar",
    stringency_col = "StringencyIndex",
    stringency_nonzero_threshold = 0,
    end_date_setting = c("max_date", "intervention_end"),
    analysis_start_manual = NULL,
    post_start_manual = NULL,
    end_date_manual = NULL,
    ... # passed into run_bsts_causalimpact
) {
  method <- match.arg(method)
  end_date_setting <- match.arg(end_date_setting)
  
  results <- list()
  
  for (cty in countries) {
    
    # 1) periods for this country
    p <- tryCatch(
      build_causal_periods(
        data = data,
        method = method,
        date_col = date_col,
        country_col = country_col,
        country = cty,
        intervention_col = intervention_col,
        stringency_col = stringency_col,
        stringency_nonzero_threshold = stringency_nonzero_threshold,
        end_date_setting = end_date_setting,
        analysis_start_manual = analysis_start_manual,
        post_start_manual = post_start_manual,
        end_date_manual = end_date_manual
      ),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )
    
    # If period detection fails, all targets for that country return an error.
    if (!is.null(p$error)) {
      for (tgt in targets) {
        key <- paste(cty, tgt, "|", method)
        results[[key]] <- list(country = cty, target_col = tgt, error = p$error)
      }
      next
    }
    
    # 2) run each target
    for (tgt in targets) {
      key <- paste(cty, tgt, "|", method)
      
      results[[key]] <- tryCatch(
        run_bsts_causalimpact(
          data = data,
          country = cty,
          target_col = tgt,
          date_col = date_col,
          country_col = country_col,
          intervention_col = intervention_col,
          analysis_start = p$analysis_start,
          post_start = p$post_start,
          end_date = p$end_date,
          ...
        ),
        error = function(e) {
          list(country = cty, target_col = tgt, error = conditionMessage(e))
        }
      )
    }
  }
  
  results
}


## Run
## -------------------------------------------------------------
countries <- c('SWE', 'TWN', 'GBR', 'USA')
targets   <- c("Total_Downloads", "Total_Revenue", "Total_DAU", "Total_ARPDAU")

safe_name <- function(x) {
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("\\|", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("[^A-Za-z0-9_\\-\\.]", "", x)
  if (nchar(x) == 0) x <- "unnamed"
  x
}

clean_name <- function(x) {
    x <- gsub("\\| (binary|stringency)", "", x)
    x <- gsub("_", " ", x)
    x
}


## -------------------------------------------------------------
run_and_save <- function(method = "binary", end_date_setting = "max_date") {
  
  # 1) run
  res <- run_bsts_batch(
    data = unified_data,
    countries = countries,
    targets = targets,
    method = method,
    end_date_setting = end_date_setting,
    date_col = "Date",
    country_col = "Country",
    intervention_col = "Intervention_BinaryVar",
    freq = "month",
    agg_fun = "sum",
    niter = 1000,
    seed  = 10101
  )
  
  # 2) output dirs (separate by method)
  out_dir  <- file.path("outputs_TDTest", method)
  res_dir  <- file.path(out_dir, "results")
  plot_dir <- file.path(out_dir, "plots")
  sum_dir  <- file.path(out_dir, "summaries")
  
  dir.create(res_dir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(sum_dir,  showWarnings = FALSE, recursive = TRUE)
  
  # 3) save
  for (nm in names(res)) {
    item <- res[[nm]]
    stem <- safe_name(nm)
    clean <- clean_name(nm)
    
    saveRDS(item, file.path(res_dir, paste0(stem, ".rds")))
    
    cat("\n", paste(rep("=", 90), collapse = ""), "\n",
        toupper(paste0("[", method, "] ", nm)), "\n",
        paste(rep("=", 90), collapse = ""), "\n", sep = "")
      
    if (!is.null(item$impact)) {
      img_path <- file.path(plot_dir, paste0(stem, ".png"))
      
      p <-  plot(item$impact) + ggplot2::ggtitle(clean) + ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"))
      
      tryCatch({
        ragg::agg_png(filename = img_path, width = 2400, height = 1600, res = 300)
        print(p)
      }, finally = {
        if (dev.cur() > 1) dev.off()
      })
      
      # show on screen
      suppressWarnings(print(p))
      
      # summary (save + print)
      txt_path <- file.path(sum_dir, paste0(stem, ".txt"))
      sum_text <- capture.output(summary(item$impact))  

      # save to txt
      writeLines(sum_text, con = txt_path)

      cat("\n----- SUMMARY: ", toupper(paste0("[", method, "] ", nm)), " -----\n", sep = "")
      cat(paste(sum_text, collapse = "\n"), "\n")
      cat("----- END SUMMARY -----\n\n")
    }
  }
  
  # Return the results without printing the consolidated table
  invisible(res)
}


### Overall Impact of the COVID-19 Interventions
## -------------------------------------------------------------
cat(">>> Running Overall Impact Analysis (End Date = Max Date) <<<\n")
res_overall <- run_and_save(method = "binary", end_date_setting = "max_date")


### Immediate Impact during COVID-19 Interventions
## -------------------------------------------------------------
cat(">>> Running Intervention Impact Analysis (End Date = Intervention End) <<<\n")
res_intervention <- run_and_save(method = "binary", end_date_setting = "intervention_end")


## -------------------------------------------------------------

### Combined Summary Function
combined_summary_save <- function(res_overall, res_intervention, method = "binary") {
  
  # Helper function to extract stats from a results list (internal)
  extract_stats <- function(res_list, period_label) {
    summary_stats <- list()
    
    for (nm in names(res_list)) {
      item <- res_list[[nm]]
      if (!is.null(item$impact) && !is.null(item$impact$summary)) {
        s <- item$impact$summary
        
        # Extract metrics
        rel_val   <- s[1, "RelEffect"]
        rel_lower <- s[1, "RelEffect.lower"]
        rel_upper <- s[1, "RelEffect.upper"]
        
        p_val     <- s[1, "p"]
        
        val   <- s[1, "AbsEffect"]
        lower <- s[1, "AbsEffect.lower"]
        upper <- s[1, "AbsEffect.upper"]
        
        if (item$target_col == "Total_ARPDAU") {
          # Small currency/value, keep decimals
          val_str <- sprintf("%+.3f [%+.3f, %+.3f]", val, lower, upper)
        } else {
          # Users/Downloads/Revenue (Large numbers)- format in Millions
          val_str <- sprintf("%+.2fM [%+.2fM, %+.2fM]", val/1e6, lower/1e6, upper/1e6)
        }
        
        summary_stats[[nm]] <- data.frame(
          Country      = item$country,
          Target       = item$target_col,
          Period       = period_label,
          RelEffect    = sprintf("%+.1f%% [%+.1f%%, %+.1f%%]", rel_val*100, rel_lower*100, rel_upper*100),
          AvgMonthlyEffect = val_str,
          p_value      = sprintf("%.3f", p_val),
          Sig          = ifelse(p_val < 0.05, "*", ""),
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(summary_stats) > 0) {
      return(dplyr::bind_rows(summary_stats))
    } else {
      return(NULL)
    }
  }
  
  # 1) Extract from both runs
  df_overall      <- extract_stats(res_overall, "Overall Effect")
  df_intervention <- extract_stats(res_intervention, "Intervention Period Only")
  
  # 2) Combine and Arrange
  final_combined_table <- dplyr::bind_rows(df_overall, df_intervention) %>%
    dplyr::arrange(Country, Target, desc(Period))
  
  # 3) Output dirs (separate by method)
  out_dir  <- file.path("outputs_TDTest", method)
  sum_dir  <- file.path(out_dir, "summaries")
  dir.create(sum_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 4) Save individual country summary files as CSV
  unique_countries <- unique(final_combined_table$Country)
  
  for (cty in unique_countries) {
    sub_table <- final_combined_table %>% 
      dplyr::filter(Country == cty)
    
    fname <- paste0(cty, "_Combined_Summary.csv")
    csv_path <- file.path(sum_dir, fname)
    
    write.csv(sub_table, csv_path, row.names = FALSE)
    
    cat("\n", paste(rep("=", 90), collapse = ""), "\n",
        toupper(paste0("[", method, "] COMBINED SUMMARY: ", cty)), "\n",
        paste(rep("=", 90), collapse = ""), "\n", sep = "")
    
    print(as.data.frame(sub_table), row.names = FALSE, right = FALSE)
    cat("----- END SUMMARY -----\n\n")
  }

  write.csv(final_combined_table, file.path(sum_dir, "All_Countries_Combined_Summary.csv"), row.names = FALSE)
  
  invisible(final_combined_table)
}


## -------------------------------------------------------------
combined_summary_save(res_overall, res_intervention, method = "binary")

