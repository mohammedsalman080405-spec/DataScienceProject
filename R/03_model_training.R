# ==============================================================================
# Script: 03_model_training.R
# Purpose: Train and compare multiple ML models to predict API latency and SLA breaches
# ==============================================================================

suppressPackageStartupMessages({
  library(rpart)
  library(randomForest)
  library(ggplot2)
})

train_and_evaluate_models <- function(data_path = "data/api_metrics_data.csv",
                                      models_dir = "models",
                                      plots_dir = "plots",
                                      seed = 42) {
  set.seed(seed)
  if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE)
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  
  df <- read.csv(data_path, stringsAsFactors = TRUE)
  
  # Select predictive features and target
  feature_cols <- c("endpoint", "http_method", "concurrent_requests", 
                    "payload_size_kb", "cpu_usage_pct", "mem_usage_pct", 
                    "db_query_count", "network_latency_ms", "cache_hit")
  
  cat("====================================================================\n")
  cat("            TRAINING API PERFORMANCE PREDICTION MODELS              \n")
  cat("====================================================================\n\n")
  
  # 1. Train / Test Split (80% / 20%)
  train_idx <- sample(seq_len(nrow(df)), size = floor(0.80 * nrow(df)))
  train_data <- df[train_idx, ]
  test_data  <- df[-train_idx, ]
  
  cat(sprintf("Training samples: %d | Test samples: %d\n\n", nrow(train_data), nrow(test_data)))
  
  formula_reg <- as.formula(paste("response_time_ms ~", paste(feature_cols, collapse = " + ")))
  
  # 2. Train Models
  cat("--> 1. Training Baseline Multiple Linear Regression...\n")
  t0 <- Sys.time()
  model_lm <- lm(formula_reg, data = train_data)
  t_lm <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  cat("--> 2. Training Decision Tree (rpart)...\n")
  t0 <- Sys.time()
  model_rpart <- rpart(formula_reg, data = train_data, method = "anova",
                       control = rpart.control(cp = 0.005, maxdepth = 8))
  t_rpart <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  cat("--> 3. Training Random Forest Regressor (150 trees)...\n")
  t0 <- Sys.time()
  model_rf <- randomForest(formula_reg, data = train_data, 
                           ntree = 150, mtry = 4, importance = TRUE)
  t_rf <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  # 3. Model Evaluation on Independent Test Set
  eval_metrics <- function(actual, pred) {
    rmse <- sqrt(mean((actual - pred)^2))
    mae  <- mean(abs(actual - pred))
    mape <- mean(abs((actual - pred) / actual)) * 100
    ss_tot <- sum((actual - mean(actual))^2)
    ss_res <- sum((actual - pred)^2)
    r2   <- 1 - (ss_res / ss_tot)
    return(c(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2))
  }
  
  pred_lm    <- predict(model_lm, newdata = test_data)
  pred_rpart <- predict(model_rpart, newdata = test_data)
  pred_rf    <- predict(model_rf, newdata = test_data)
  
  m_lm    <- eval_metrics(test_data$response_time_ms, pred_lm)
  m_rpart <- eval_metrics(test_data$response_time_ms, pred_rpart)
  m_rf    <- eval_metrics(test_data$response_time_ms, pred_rf)
  
  comparison_df <- data.frame(
    Model = c("Linear Regression", "Decision Tree", "Random Forest"),
    RMSE_ms = c(m_lm["RMSE"], m_rpart["RMSE"], m_rf["RMSE"]),
    MAE_ms  = c(m_lm["MAE"], m_rpart["MAE"], m_rf["MAE"]),
    MAPE_pct = c(m_lm["MAPE"], m_rpart["MAPE"], m_rf["MAPE"]),
    R_Squared = c(m_lm["R2"], m_rpart["R2"], m_rf["R2"]),
    TrainTime_s = c(t_lm, t_rpart, t_rf)
  )
  rownames(comparison_df) <- NULL
  
  cat("\n====================================================================\n")
  cat("                     MODEL BENCHMARK RESULTS                        \n")
  cat("====================================================================\n")
  print(round(comparison_df[, -1], 3))
  cat("\n")
  
  # 4. SLA Breach Classification Evaluation
  # If predicted latency > endpoint SLA threshold, predicted breach = 1
  actual_breach <- test_data$sla_breach == 1
  
  eval_sla <- function(pred_latency, thresholds) {
    pred_breach <- pred_latency > thresholds
    acc <- mean(pred_breach == actual_breach)
    tp <- sum(pred_breach & actual_breach)
    fp <- sum(pred_breach & !actual_breach)
    fn <- sum(!pred_breach & actual_breach)
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall    <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    f1        <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
    c(Accuracy = acc * 100, Precision = precision * 100, Recall = recall * 100, F1_Score = f1 * 100)
  }
  
  sla_metrics <- rbind(
    "Linear Regression" = eval_sla(pred_lm, test_data$sla_threshold_ms),
    "Decision Tree"     = eval_sla(pred_rpart, test_data$sla_threshold_ms),
    "Random Forest"     = eval_sla(pred_rf, test_data$sla_threshold_ms)
  )
  
  cat("--- SLA Breach Detection Capability on Test Set ---\n")
  print(round(sla_metrics, 2))
  cat("\n")
  
  # 5. Visualizations
  theme_custom <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#555555"),
      panel.grid.minor = element_blank()
    )
  
  # Plot 1: Model Comparison Bar Chart
  comp_melt <- data.frame(
    Model = rep(comparison_df$Model, 2),
    Metric = rep(c("MAE (ms)", "RMSE (ms)"), each = 3),
    Value = c(comparison_df$MAE_ms, comparison_df$RMSE_ms)
  )
  
  p1 <- ggplot(comp_melt, aes(x = Model, y = Value, fill = Metric)) +
    geom_col(position = "dodge", width = 0.6) +
    scale_fill_manual(values = c("MAE (ms)" = "#4575b4", "RMSE (ms)" = "#d73027")) +
    labs(title = "Model Error Benchmark (Lower is Better)",
         subtitle = "Comparison across Linear Regression, Decision Tree, and Random Forest",
         x = "Model", y = "Error in Milliseconds (ms)") +
    theme_custom
  
  ggsave(file.path(plots_dir, "05_model_benchmark_bar.png"), plot = p1, width = 8, height = 5, dpi = 150)
  
  # Plot 2: Actual vs Predicted Latency (Random Forest)
  plot_df <- data.frame(Actual = test_data$response_time_ms, Predicted = pred_rf, Endpoint = test_data$endpoint)
  p2 <- ggplot(plot_df, aes(x = Actual, y = Predicted, color = Endpoint)) +
    geom_point(alpha = 0.5, size = 1.8) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
    labs(title = "Actual vs. Predicted Response Time (Random Forest)",
         subtitle = paste0("R² = ", round(m_rf["R2"], 3), " | MAE = ", round(m_rf["MAE"], 1), " ms"),
         x = "Actual Latency (ms)", y = "Predicted Latency (ms)") +
    theme_custom
  
  ggsave(file.path(plots_dir, "06_actual_vs_predicted.png"), plot = p2, width = 8, height = 5, dpi = 150)
  
  # Plot 3: Feature Importance (Random Forest)
  imp <- importance(model_rf)
  imp_df <- data.frame(
    Feature = rownames(imp),
    IncMSE = imp[, "%IncMSE"]
  )
  imp_df <- imp_df[order(imp_df$IncMSE, decreasing = TRUE), ]
  imp_df$Feature <- factor(imp_df$Feature, levels = rev(imp_df$Feature))
  
  p3 <- ggplot(imp_df, aes(x = Feature, y = IncMSE, fill = IncMSE)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_gradient(low = "#91bfdb", high = "#fc8d59") +
    labs(title = "Feature Importance: Driver of API Response Time",
         subtitle = "% Increase in Mean Squared Error when feature is permuted",
         x = "Feature", y = "% Increase in MSE") +
    guides(fill = "none") +
    theme_custom
  
  ggsave(file.path(plots_dir, "07_feature_importance.png"), plot = p3, width = 8, height = 5, dpi = 150)
  
  # 6. Save Trained Models & Metadata
  saveRDS(model_rf, file = file.path(models_dir, "api_rf_model.rds"))
  saveRDS(model_lm, file = file.path(models_dir, "api_lm_model.rds"))
  saveRDS(model_rpart, file = file.path(models_dir, "api_rpart_model.rds"))
  
  # Extract factor levels from training data
  factor_levels <- list(
    endpoint = levels(train_data$endpoint),
    http_method = levels(train_data$http_method)
  )
  
  metadata <- list(
    feature_cols = feature_cols,
    factor_levels = factor_levels,
    benchmarks = comparison_df,
    sla_metrics = sla_metrics,
    best_model = "Random Forest",
    trained_at = Sys.time()
  )
  saveRDS(metadata, file = file.path(models_dir, "model_metadata.rds"))
  
  cat(sprintf("[SUCCESS] Models saved to '%s/'. Benchmark plots saved to '%s/'.\n", models_dir, plots_dir))
  return(invisible(list(models = list(rf = model_rf, lm = model_lm, rpart = model_rpart), 
                        metrics = comparison_df)))
}

if (sys.nframe() == 0) {
  train_and_evaluate_models()
}
