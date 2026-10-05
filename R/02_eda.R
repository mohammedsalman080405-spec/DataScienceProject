# ==============================================================================
# Script: 02_eda.R
# Purpose: Exploratory Data Analysis for API Performance Telemetry
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

run_eda <- function(data_path = "data/api_metrics_data.csv", plots_dir = "plots") {
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  
  if (!file.exists(data_path)) {
    stop(sprintf("Data file not found at %s. Please run 01_data_generation.R first.", data_path))
  }
  
  df <- read.csv(data_path, stringsAsFactors = FALSE)
  df$endpoint <- as.factor(df$endpoint)
  df$http_method <- as.factor(df$http_method)
  df$cache_hit <- as.factor(df$cache_hit)
  df$sla_breach <- as.factor(df$sla_breach)
  
  cat("====================================================================\n")
  cat("             API PERFORMANCE TELEMETRY - EXPLORATORY ANALYSIS        \n")
  cat("====================================================================\n\n")
  
  cat(sprintf("Total API Transactions: %d\n", nrow(df)))
  cat(sprintf("Time Period: %s to %s\n\n", min(df$timestamp), max(df$timestamp)))
  
  # 1. Summary Statistics of Response Time
  resp_summary <- c(
    Min = min(df$response_time_ms),
    Q1 = quantile(df$response_time_ms, 0.25),
    Median = median(df$response_time_ms),
    Mean = mean(df$response_time_ms),
    P95 = quantile(df$response_time_ms, 0.95),
    P99 = quantile(df$response_time_ms, 0.99),
    Max = max(df$response_time_ms)
  )
  cat("--- Latency Metrics (Response Time in ms) ---\n")
  print(round(resp_summary, 2))
  cat("\n")
  
  # 2. SLA Breach Analysis
  total_breaches <- sum(as.numeric(as.character(df$sla_breach)) == 1)
  cat(sprintf("Total SLA Breaches: %d (%.2f%% of all requests)\n\n", 
              total_breaches, (total_breaches / nrow(df)) * 100))
  
  cat("--- SLA Breach Rate by Endpoint ---\n")
  sla_by_ep <- aggregate(as.numeric(as.character(sla_breach)) ~ endpoint, data = df, function(x) {
    c(Count = length(x), Breaches = sum(x), 
      Pct = round(mean(x) * 100, 2))
  })
  print(sla_by_ep)
  cat("\n")
  
  # 3. Numeric Correlations with Response Time
  num_cols <- c("concurrent_requests", "payload_size_kb", "cpu_usage_pct", 
                "mem_usage_pct", "db_query_count", "network_latency_ms", "response_time_ms")
  cor_matrix <- cor(df[, num_cols], use = "complete.obs")
  
  cat("--- Correlation with response_time_ms ---\n")
  print(round(cor_matrix["response_time_ms", ], 3))
  cat("\n")
  
  # ==========================================================================
  # Visualizations
  # ==========================================================================
  theme_custom <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#555555"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Plot 1: Latency Distribution
  p1 <- ggplot(df, aes(x = response_time_ms)) +
    geom_histogram(bins = 60, fill = "#2b5c8f", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = median(response_time_ms)), color = "orange", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = quantile(response_time_ms, 0.95)), color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text", x = median(df$response_time_ms) + 60, y = 300, 
             label = paste("Median:", round(median(df$response_time_ms), 1), "ms"), color = "orange") +
    annotate("text", x = quantile(df$response_time_ms, 0.95) + 120, y = 200, 
             label = paste("P95:", round(quantile(df$response_time_ms, 0.95), 1), "ms"), color = "red") +
    labs(title = "API Response Time Distribution",
         subtitle = "Overall latency profile across 5,000 requests",
         x = "Response Time (ms)", y = "Frequency") +
    theme_custom
  
  ggsave(file.path(plots_dir, "01_latency_distribution.png"), plot = p1, width = 8, height = 5, dpi = 150)
  
  # Plot 2: Latency by Endpoint Boxplot
  p2 <- ggplot(df, aes(x = endpoint, y = response_time_ms, fill = endpoint)) +
    geom_boxplot(alpha = 0.75, outlier.color = "red", outlier.size = 1.2) +
    coord_flip() +
    labs(title = "API Response Time by Endpoint",
         subtitle = "Heavy transactions (/reports, /checkout) exhibit higher baseline and variance",
         x = "API Endpoint", y = "Response Time (ms)") +
    guides(fill = "none") +
    theme_custom
  
  ggsave(file.path(plots_dir, "02_latency_by_endpoint.png"), plot = p2, width = 8, height = 5, dpi = 150)
  
  # Plot 3: Non-linear CPU vs Latency (Queuing bottleneck)
  p3 <- ggplot(df, aes(x = cpu_usage_pct, y = response_time_ms, color = endpoint)) +
    geom_point(alpha = 0.4, size = 1.6) +
    geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1) +
    geom_vline(xintercept = 75, linetype = "dotted", color = "darkred", linewidth = 1) +
    annotate("text", x = 77, y = max(df$response_time_ms) * 0.9, 
             label = "Critical CPU Threshold (>75%)", color = "darkred", hjust = 0) +
    labs(title = "Server CPU Utilization vs. API Latency",
         subtitle = "Demonstrates non-linear queue explosion under heavy CPU load",
         x = "Server CPU Usage (%)", y = "Response Time (ms)",
         color = "Endpoint") +
    theme_custom
  
  ggsave(file.path(plots_dir, "03_cpu_vs_latency_scatter.png"), plot = p3, width = 9, height = 6, dpi = 150)
  
  # Plot 4: Concurrent Requests vs SLA Breach
  df$concurrent_bin <- cut(df$concurrent_requests, breaks = c(0, 50, 100, 200, 350, 500))
  sla_load <- aggregate(as.numeric(as.character(sla_breach)) ~ concurrent_bin, data = df, mean)
  colnames(sla_load) <- c("Load_Tier", "Breach_Rate")
  
  p4 <- ggplot(sla_load, aes(x = Load_Tier, y = Breach_Rate * 100, fill = Breach_Rate)) +
    geom_col(width = 0.6) +
    scale_fill_gradient(low = "#4caf50", high = "#f44336") +
    labs(title = "SLA Breach Rate by Concurrent Load Tier",
         subtitle = "Percentage of requests violating SLA under increasing traffic pressure",
         x = "Concurrent Requests Tier", y = "SLA Breach Rate (%)") +
    guides(fill = "none") +
    theme_custom
  
  ggsave(file.path(plots_dir, "04_concurrent_load_sla.png"), plot = p4, width = 8, height = 5, dpi = 150)
  
  cat(sprintf("[SUCCESS] EDA completed. Plots saved to '%s/'.\n", plots_dir))
  return(invisible(df))
}

if (sys.nframe() == 0) {
  run_eda()
}
