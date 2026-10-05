# ==============================================================================
# Script: 01_data_generation.R
# Purpose: Generate realistic synthetic API performance telemetry dataset
# ==============================================================================

generate_api_data <- function(n_samples = 5000, seed = 42) {
  set.seed(seed)
  message(sprintf("Generating %d synthetic API telemetry records...", n_samples))
  
  # 1. API Endpoints and their inherent characteristics
  endpoints <- c("/api/v1/users", "/api/v1/products", "/api/v1/search", 
                 "/api/v1/checkout", "/api/v1/reports")
  endpoint_probs <- c(0.25, 0.30, 0.20, 0.15, 0.10)
  
  endpoint_base_latency <- c(
    "/api/v1/users" = 35,
    "/api/v1/products" = 45,
    "/api/v1/search" = 90,
    "/api/v1/checkout" = 175,
    "/api/v1/reports" = 340
  )
  
  endpoint_sla <- c(
    "/api/v1/users" = 150,
    "/api/v1/products" = 200,
    "/api/v1/search" = 300,
    "/api/v1/checkout" = 500,
    "/api/v1/reports" = 800
  )
  
  endpoint_db_queries_mean <- c(
    "/api/v1/users" = 1.2,
    "/api/v1/products" = 2.0,
    "/api/v1/search" = 4.5,
    "/api/v1/checkout" = 6.0,
    "/api/v1/reports" = 12.0
  )
  
  # Sample categorical attributes
  sampled_endpoints <- sample(endpoints, size = n_samples, replace = TRUE, prob = endpoint_probs)
  
  http_methods <- sapply(sampled_endpoints, function(ep) {
    if (ep %in% c("/api/v1/users", "/api/v1/products", "/api/v1/search")) {
      sample(c("GET", "POST"), 1, prob = c(0.85, 0.15))
    } else if (ep == "/api/v1/checkout") {
      sample(c("POST", "PUT"), 1, prob = c(0.8, 0.2))
    } else {
      sample(c("GET", "POST"), 1, prob = c(0.9, 0.1))
    }
  })
  
  # 2. Server & Workload Features
  # Concurrent requests (Poisson / log-normal mix with bursty spikes)
  concurrent_requests <- pmax(1, round(rlnorm(n_samples, meanlog = 3.8, sdlog = 0.75)))
  concurrent_requests <- pmin(concurrent_requests, 500) # Cap at 500
  
  # Payload size in KB
  payload_size_kb <- round(rlnorm(n_samples, meanlog = 1.5, sdlog = 0.9), 2)
  payload_size_kb <- pmin(payload_size_kb, 250)
  
  # CPU & Memory Utilization (%)
  # CPU is correlated with concurrent load plus background noise
  cpu_usage_pct <- pmin(99.5, pmax(5.0, 15 + 0.15 * concurrent_requests + rnorm(n_samples, mean = 10, sd = 8)))
  mem_usage_pct <- pmin(98.0, pmax(20.0, 30 + 0.08 * concurrent_requests + rnorm(n_samples, mean = 12, sd = 6)))
  
  # Network latency in ms (normal ping distribution + occasional jitter)
  network_latency_ms <- round(pmax(2.0, rnorm(n_samples, mean = 25, sd = 10) + rexp(n_samples, rate = 0.1)), 2)
  
  # Cache hit (prob depends on endpoint and method)
  cache_hit <- mapply(function(ep, m) {
    if (m != "GET") return(0)
    if (ep %in% c("/api/v1/products", "/api/v1/users")) {
      sample(c(0, 1), 1, prob = c(0.35, 0.65))
    } else if (ep == "/api/v1/search") {
      sample(c(0, 1), 1, prob = c(0.60, 0.40))
    } else {
      0
    }
  }, sampled_endpoints, http_methods)
  
  # Database queries count
  db_query_count <- mapply(function(ep, ch) {
    if (ch == 1) return(0)
    mean_q <- endpoint_db_queries_mean[[ep]]
    pmax(1, rpois(1, lambda = mean_q))
  }, sampled_endpoints, cache_hit)
  
  # 3. Target: Response Time (ms)
  # Simulated based on queueing theory:
  # - Base endpoint processing
  # - Database overhead (10-18ms per query)
  # - Payload transfer overhead (0.4ms per KB)
  # - Network latency component
  # - Non-linear queuing penalty when CPU > 70% (exponential queue buildup)
  # - Cache hit reduction
  base_latency <- endpoint_base_latency[sampled_endpoints]
  db_latency <- db_query_count * rnorm(n_samples, mean = 14, sd = 3)
  payload_overhead <- payload_size_kb * 0.45
  
  # Queueing factor: M/M/1 queue model behavior rho / (1 - rho)
  utilization_ratio <- cpu_usage_pct / 100
  queue_multiplier <- ifelse(utilization_ratio < 0.70,
                             1.0 + (utilization_ratio * 0.4),
                             1.0 + 0.28 + exp((utilization_ratio - 0.70) * 8.5))
  
  raw_latency <- (base_latency + db_latency + payload_overhead + (concurrent_requests * 0.15)) * queue_multiplier
  
  # Apply cache discount if hit
  raw_latency[cache_hit == 1] <- raw_latency[cache_hit == 1] * 0.25
  
  # Add network latency and random noise
  noise <- rnorm(n_samples, mean = 0, sd = 6)
  response_time_ms <- round(pmax(5.0, raw_latency + network_latency_ms + noise), 2)
  
  # 4. Target 2: SLA Breach
  sla_thresholds <- endpoint_sla[sampled_endpoints]
  sla_breach <- as.integer(response_time_ms > sla_thresholds)
  
  # Timestamps spanning 7 days
  start_time <- as.POSIXct("2026-10-01 00:00:00", tz = "UTC")
  timestamps <- start_time + sort(runif(n_samples, min = 0, max = 7 * 86400))
  
  # Build DataFrame
  df <- data.frame(
    request_id = paste0("req_", sprintf("%06d", 1:n_samples)),
    timestamp = format(timestamps, "%Y-%m-%d %H:%M:%S"),
    hour_of_day = as.integer(format(timestamps, "%H")),
    endpoint = as.factor(sampled_endpoints),
    http_method = as.factor(http_methods),
    concurrent_requests = as.integer(concurrent_requests),
    payload_size_kb = payload_size_kb,
    cpu_usage_pct = round(cpu_usage_pct, 2),
    mem_usage_pct = round(mem_usage_pct, 2),
    db_query_count = as.integer(db_query_count),
    network_latency_ms = network_latency_ms,
    cache_hit = as.factor(cache_hit),
    sla_threshold_ms = sla_thresholds,
    response_time_ms = response_time_ms,
    sla_breach = as.factor(sla_breach),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# Standalone execution
if (sys.nframe() == 0) {
  df <- generate_api_data(n_samples = 5000, seed = 42)
  out_path <- "data/api_metrics_data.csv"
  write.csv(df, out_path, row.names = FALSE)
  cat(sprintf("[SUCCESS] Generated %d rows. Saved to '%s'.\n", nrow(df), out_path))
  cat("Preview:\n")
  print(head(df[, c("endpoint", "concurrent_requests", "cpu_usage_pct", "db_query_count", "response_time_ms", "sla_breach")], 5))
}
