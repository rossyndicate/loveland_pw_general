#' Generate Model Predictions for Time Series Data
#'
#' This function takes sensor data, aggregates it to a specified time interval,
#' and computes predictions across an ensemble of XGBoost models. It supports
#' filtering by site and date window, with graceful fallbacks if parameters are missing.
#'
#' @param model_input_df A data frame or tibble containing the input sensor data. Must include `site` and `DT_round`.
#' @param ensemble_model A named list of trained XGBoost model objects.
#' @param summary_interval Character string indicating the rounding/summarization interval (e.g., "1 hour", "1 day"). Default is "1 hour".
#' @param site_sel Optional character string specifying the site to filter. If missing, `NULL`, or `NA`, all sites are included.
#' @param start_DT Optional character/POSIXct start date-time string. If missing, defaults to the earliest date in the data.
#' @param end_DT Optional character/POSIXct end date-time string. If missing, defaults to the latest date in the data.
#' @param target_col Character string representing the prefix for prediction columns. Default is "TOC".
#'
#' @return A tibble with summarized feature data, fold-specific predictions, min/max prediction limits, and ensemble means.
#'
#' @examples
#' sensor_ts_w_preds <- predict_model_ts(
#' model_input_df = sensor_ts,
#' ensemble_model = map(rt_model, "model"),
#' target_col = "TOC",summary_interval = "3 hours")
#'
predict_model_ts <- function(model_input_df,
                             ensemble_model,
                             summary_interval = "1 hour",
                             site_sel = NULL,
                             start_DT = NULL,
                             end_DT = NULL,
                             target_col = "TOC") {

  # 1. Process and Validate Date Inputs
  has_start <- !missing(start_DT) && !is.null(start_DT) && !is.na(start_DT)
  has_end   <- !missing(end_DT)   && !is.null(end_DT)   && !is.na(end_DT)

  if (has_start) start_DT <- ymd_hm(start_DT, tz = "MST")
  if (has_end)   end_DT   <- ymd_hm(end_DT, tz = "MST")

  # 2. Process and Summarize Data
  summarized_data <- model_input_df

  # Filter by site only if site_sel is valid and provided
  has_site <- !missing(site_sel) && !is.null(site_sel) && !is.na(site_sel)
  if (has_site) {
    summarized_data <- summarized_data %>% filter(site == site_sel)
  }

  # Summarize data to desired interval
  summarized_data <- summarized_data %>%
    mutate(DT_round = round_date(DT_round, unit = summary_interval)) %>%
    group_by(site, DT_round) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

  # Extract features layout from the first fold
  features <- xgb.importance(model = ensemble_model[[1]]) %>% pull(Feature)

  # 3. Generate Ensemble Fold Predictions
  predictions_df <- imap_dfc(ensemble_model, ~{
    features_fold <- xgb.importance(model = .x) %>% pull(Feature)
    best_iter <- as.numeric(xgb.attr(.x, "best_iteration"))

    feature_data <- summarized_data %>%
      select(all_of(features_fold)) %>%
      mutate(across(everything(), as.numeric))

    # Check for missing values in features
    has_na <- rowSums(is.na(feature_data)) > 0

    # Make predictions
    raw_preds <- feature_data %>%
      as.matrix() %>%
      predict(.x, ., iteration_range = c(1, best_iter), validate_features = TRUE) %>%
      round(2)

    # Make predictions NA where features had NA
    final_preds <- if_else(has_na, NA_real_, raw_preds)

    # Return predictions as tibble
    tibble(!!glue("{target_col}_guess_fold{.y}") := final_preds)
  })

  # Combine predictions with summarized features and calculate ensemble mean
  summarized_data <- summarized_data %>%
    bind_cols(predictions_df) %>%
    mutate(
      !!glue("{target_col}_guess_ensemble") := if_else(
        if_any(all_of(features), is.na),
        NA_real_,
        round(rowMeans(across(matches(glue("{target_col}_guess_fold")))), 2)
      )
    )

  # 4. Generate Time Series Plot-Ready Outputs (Slicing & Padding)
  fold_cols <- grep(glue("{target_col}_guess_fold"), colnames(summarized_data), value = TRUE)

  plot_ts_data <- summarized_data

  # Apply date window filters if specified
  if (has_start && has_end) {
    plot_ts_data <- plot_ts_data %>% filter(between(DT_round, start_DT, end_DT))
  }

  # Establish start/end bounds for the padding step (fallback to data extremes if missing)
  pad_start <- if (has_start) start_DT else min(plot_ts_data$DT_round, na.rm = TRUE)
  pad_end   <- if (has_end)   end_DT   else max(plot_ts_data$DT_round, na.rm = TRUE)

  # Handle Multi-Site or Single-Site Padding securely using group = "site"
  plot_ts_data <- plot_ts_data %>%
    pad(
      by = "DT_round",
      interval = summary_interval,
      start_val = pad_start,
      end_val = pad_end,
      group = "site"
    )

  # Compute min/max boundaries and group consecutive non-NA prediction sequences
  plot_ts_data <- plot_ts_data %>%
    group_by(site) %>%
    mutate(
      !!glue("{target_col}_guess_min") := pmin(!!!syms(fold_cols), na.rm = TRUE),
      !!glue("{target_col}_guess_max") := pmax(!!!syms(fold_cols), na.rm = TRUE),
      !!glue("{target_col}_guess_ensemble") := pmax(0, !!sym(glue("{target_col}_guess_ensemble"))),
      group = with(rle(!is.na(.data[[glue("{target_col}_guess_ensemble")]])), rep(seq_along(values), lengths))
    ) %>%
    ungroup()

  return(plot_ts_data)
}
