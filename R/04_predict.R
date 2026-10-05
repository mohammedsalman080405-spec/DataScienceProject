# ==============================================================================
# Script: 04_predict.R
# Purpose: Inference engine for predicting API performance & diagnosing bottlenecks
# ==============================================================================

suppressPackageStartupMessages({
  library(randomForest)
})

load_api_model <- function(model_type = "rf", models_dir = "models") {
  model_file <- switch(model_type,
    "rf"    = file.path(models_dir, "api_rf_model.rds"),
    "lm"    = file.path(models_dir, "api_lm_model.rds"),
    "rpart" = file.path(models_dir, "api_rpart_model.rds"),
    stop("Unknown model_type. Choose 'rf', 'lm', or 'rpart'.")
  )
  
  if (!file.exists(model_file)) {
    stop(sprintf("Model file '%s' not found. Please train models first using R/03_model_training.R", model_file))
  }
  
  readRDS(model_file)
}

load_api_metadata <- function(models_dir = "models") {
  meta_file <- file.path(models_dir, "model_metadata.rds")
  if (file.exists(meta_file)) {
    return(readRDS(meta_file))
  }
  return(NULL)
}

predict_api_performance <- function(endpoint = "/api/v1/checkout",
                                    http_method = "POST",
                                    concurrent_requests = 120,
                                    payload_size_kb = 15.5,
                                    cpu_usage_pct = 65.0,
                                    mem_usage_pct = 70.0,
                                    db_query_count = 5,
                                    network_latency_ms = 28.0,
                                    cache_hit = 0,
                                    model = NULL,
                                    model_type = "rf",
                                    models_dir = "models") {
  
  if (is.null(model)) {
    model <- load_api_model(model_type, models_dir = models_dir)
  }
  
  meta <- load_api_metadata(models_dir = models_dir)
  
  endpoint_levels <- if (!is.null(meta$factor_levels$endpoint)) {
    meta$factor_levels$endpoint
  } else {
    c("/api/v1/checkout", "/api/v1/products", "/api/v1/reports", "/api/v1/search", "/api/v1/users")
  }
  
  method_levels <- if (!is.null(meta$factor_levels$http_method)) {
    meta$factor_levels$http_method
  } else {
    c("GET", "POST", "PUT")
  }
  
  endpoint_sla <- c(
    "/api/v1/users"    = 150,
    "/api/v1/products" = 200,
    "/api/v1/search"   = 300,
    "/api/v1/checkout" = 500,
    "/api/v1/reports"  = 800
  )
  
  # Format input dataframe with proper factors and integer/numeric types matching training scheme
  input_df <- data.frame(
    endpoint = factor(endpoint, levels = endpoint_levels),
    http_method = factor(http_method, levels = method_levels),
    concurrent_requests = as.integer(concurrent_requests),
    payload_size_kb = as.numeric(payload_size_kb),
    cpu_usage_pct = as.numeric(cpu_usage_pct),
    mem_usage_pct = as.numeric(mem_usage_pct),
    db_query_count = as.integer(db_query_count),
    network_latency_ms = as.numeric(network_latency_ms),
    cache_hit = as.integer(cache_hit),
    stringsAsFactors = FALSE
  )
  
  # Predict latency
  pred_latency <- round(as.numeric(predict(model, newdata = input_df)), 2)
  sla_limit    <- if (endpoint %in% names(endpoint_sla)) endpoint_sla[[endpoint]] else 300
  sla_ratio    <- pred_latency / sla_limit
  
  # SLA Health Status
  status <- if (sla_ratio > 1.0) {
    "CRITICAL_SLA_BREACH"
  } else if (sla_ratio > 0.8) {
    "WARNING_NEAR_THRESHOLD"
  } else {
    "OPTIMAL_HEALTH"
  }
  
  # Diagnostic Bottleneck Analysis
  bottlenecks <- c()
  if (cpu_usage_pct >= 75) {
    bottlenecks <- c(bottlenecks, sprintf("High CPU (%.1f%%): queue saturation causes exponential latency spike.", cpu_usage_pct))
  }
  if (db_query_count >= 6) {
    bottlenecks <- c(bottlenecks, sprintf("Heavy DB usage (%d queries): database I/O is primary overhead.", db_query_count))
  }
  if (concurrent_requests >= 200) {
    bottlenecks <- c(bottlenecks, sprintf("High concurrent load (%d requests): queue backlog building up.", concurrent_requests))
  }
  if (as.integer(cache_hit) == 0 && endpoint %in% c("/api/v1/products", "/api/v1/users", "/api/v1/search")) {
    bottlenecks <- c(bottlenecks, "Cache miss: caching this endpoint could reduce latency by ~70%.")
  }
  if (length(bottlenecks) == 0) {
    bottlenecks <- c("Operating within healthy performance boundaries.")
  }
  
  result <- list(
    endpoint = endpoint,
    http_method = http_method,
    predicted_response_time_ms = pred_latency,
    sla_threshold_ms = sla_limit,
    sla_utilization_pct = round(sla_ratio * 100, 1),
    status = status,
    bottlenecks = bottlenecks
  )
  
  return(result)
}

# Standalone execution for interactive demonstration
if (sys.nframe() == 0) {
  cat("====================================================================\n")
  cat("              API PERFORMANCE PREDICTION PROTOTYPE DEMO            \n")
  cat("====================================================================\n\n")
  
  scenarios <- list(
    "Scenario A: Normal User Profile Query (Cached)" = list(
      endpoint = "/api/v1/users", http_method = "GET", concurrent = 30,
      payload = 2.0, cpu = 25.0, mem = 40.0, db = 0, net = 15.0, cache = 1
    ),
    "Scenario B: Search Query under Moderate Load" = list(
      endpoint = "/api/v1/search", http_method = "GET", concurrent = 90,
      payload = 8.5, cpu = 52.0, mem = 55.0, db = 4, net = 22.0, cache = 0
    ),
    "Scenario C: Flash Sale Peak Load Checkout (High Stress)" = list(
      endpoint = "/api/v1/checkout", http_method = "POST", concurrent = 380,
      payload = 24.0, cpu = 88.5, mem = 82.0, db = 8, net = 35.0, cache = 0
    ),
    "Scenario D: Heavy Analytical Report Export" = list(
      endpoint = "/api/v1/reports", http_method = "GET", concurrent = 40,
      payload = 65.0, cpu = 45.0, mem = 60.0, db = 12, net = 30.0, cache = 0
    )
  )
  
  rf_model <- load_api_model("rf")
  
  for (name in names(scenarios)) {
    sc <- scenarios[[name]]
    res <- predict_api_performance(
      endpoint = sc$endpoint,
      http_method = sc$http_method,
      concurrent_requests = sc$concurrent,
      payload_size_kb = sc$payload,
      cpu_usage_pct = sc$cpu,
      mem_usage_pct = sc$mem,
      db_query_count = sc$db,
      network_latency_ms = sc$net,
      cache_hit = sc$cache,
      model = rf_model
    )
    
    cat(sprintf(">>> %s <<<\n", name))
    cat(sprintf("   Predicted Latency: %s ms (SLA Limit: %s ms | SLA Utilization: %.1f%%)\n", 
                res$predicted_response_time_ms, res$sla_threshold_ms, res$sla_utilization_pct))
    cat(sprintf("   Status:            [%s]\n", res$status))
    cat("   Root Cause / Advice:\n")
    for (b in res$bottlenecks) {
      cat(sprintf("     * %s\n", b))
    }
    cat("\n")
  }
}
