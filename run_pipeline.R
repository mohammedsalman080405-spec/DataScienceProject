# ==============================================================================
# Script: run_pipeline.R
# Purpose: Master execution script for API Performance Prediction Prototype
# ==============================================================================

cat("\n==============================================================================\n")
cat("          STARTING END-TO-END API PERFORMANCE PREDICTION PIPELINE             \n")
cat("==============================================================================\n\n")

# Step 1: Telemetry Data Generation
cat("[STEP 1/4] Generating Synthetic API Telemetry Dataset...\n")
source("R/01_data_generation.R")
df <- generate_api_data(n_samples = 5000, seed = 42)
write.csv(df, "data/api_metrics_data.csv", row.names = FALSE)
cat(sprintf("✔ Telemetry dataset saved: %d transactions in 'data/api_metrics_data.csv'\n\n", nrow(df)))

# Step 2: Exploratory Data Analysis
cat("[STEP 2/4] Executing Exploratory Data Analysis (EDA)...\n")
source("R/02_eda.R")
run_eda(data_path = "data/api_metrics_data.csv", plots_dir = "plots")
cat("✔ Exploratory visualizations generated in 'plots/'\n\n")

# Step 3: Model Training & Benchmarking
cat("[STEP 3/4] Training and Benchmarking Machine Learning Models...\n")
source("R/03_model_training.R")
train_res <- train_and_evaluate_models(
  data_path = "data/api_metrics_data.csv",
  models_dir = "models",
  plots_dir = "plots",
  seed = 42
)
cat("✔ Models trained, benchmarked, and serialized in 'models/'\n\n")

# Step 4: Sample Scenario Inference & Diagnostics
cat("[STEP 4/4] Running Inference Engine on Production Scenarios...\n")
source("R/04_predict.R")

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
  cat(sprintf("   Predicted Latency: %s ms (SLA Limit: %s ms | Headroom/Usage: %.1f%%)\n", 
              res$predicted_response_time_ms, res$sla_threshold_ms, res$sla_utilization_pct))
  cat(sprintf("   Status:            [%s]\n", res$status))
  cat("   Diagnostics:\n")
  for (b in res$bottlenecks) {
    cat(sprintf("     * %s\n", b))
  }
  cat("\n")
}

cat("==============================================================================\n")
cat("                       PIPELINE EXECUTION COMPLETE!                           \n")
cat("==============================================================================\n")
cat("To launch the interactive Shiny Dashboard prototype, run:\n")
cat("    Rscript -e \"shiny::runApp('.', port=3838, host='127.0.0.1')\"\n")
cat("Or open R / RStudio and run `shiny::runApp()`\n\n")
