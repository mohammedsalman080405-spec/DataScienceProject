# ==============================================================================
# Script: generate_sample_projects.R
# Purpose: Generate realistic telemetry datasets for two different real-world projects:
#          1. E-Commerce API (Cart, Payment, Search, Inventory)
#          2. FinTech / Banking API (Account Balance, Wire Transfer, KYC, Fraud Check)
# ==============================================================================

set.seed(101)

# Project 1: E-Commerce Platform
n1 <- 1200
ecom_endpoints <- c("/cart/add", "/checkout/pay", "/catalog/search", "/inventory/check")
ecom_methods <- c("POST", "POST", "GET", "GET")
ep_sample1 <- sample(ecom_endpoints, n1, replace = TRUE, prob = c(0.3, 0.2, 0.35, 0.15))

ecom_df <- data.frame(
  request_id = paste0("ecom_", 1:n1),
  endpoint = ep_sample1,
  http_method = ifelse(ep_sample1 %in% c("/cart/add", "/checkout/pay"), "POST", "GET"),
  concurrent_requests = round(rnorm(n1, mean = 150, sd = 40)),
  payload_size_kb = round(rexp(n1, rate = 0.1) + 2, 2),
  cpu_usage_pct = round(pmin(99, pmax(10, rnorm(n1, mean = 60, sd = 15))), 1),
  mem_usage_pct = round(pmin(98, pmax(20, rnorm(n1, mean = 65, sd = 10))), 1),
  db_query_count = ifelse(ep_sample1 == "/checkout/pay", sample(6:12, n1, replace = TRUE),
                   ifelse(ep_sample1 == "/catalog/search", sample(3:7, n1, replace = TRUE), sample(1:4, n1, replace = TRUE))),
  network_latency_ms = round(rnorm(n1, mean = 30, sd = 8), 1),
  cache_hit = ifelse(ep_sample1 == "/catalog/search", sample(c(0, 1), n1, replace = TRUE, prob = c(0.4, 0.6)), 0),
  stringsAsFactors = FALSE
)

# E-commerce response time calculation
ecom_base <- c("/cart/add" = 60, "/checkout/pay" = 280, "/catalog/search" = 85, "/inventory/check" = 40)
ecom_df$response_time_ms <- round(
  (ecom_base[ecom_df$endpoint] + 
   ecom_df$db_query_count * 15 + 
   ecom_df$payload_size_kb * 0.4 + 
   ecom_df$concurrent_requests * 0.2) * 
  ifelse(ecom_df$cpu_usage_pct > 75, 1 + exp((ecom_df$cpu_usage_pct - 75)/8), 1.0) *
  ifelse(ecom_df$cache_hit == 1, 0.3, 1.0) +
  rnorm(n1, 0, 5), 2
)
ecom_df$response_time_ms <- pmax(10, ecom_df$response_time_ms)
write.csv(ecom_df, "data/ecommerce_api_sample.csv", row.names = FALSE)


# Project 2: FinTech / Banking Core API
n2 <- 1000
fin_endpoints <- c("/account/balance", "/transfer/wire", "/fraud/evaluate", "/kyc/document")
ep_sample2 <- sample(fin_endpoints, n2, replace = TRUE, prob = c(0.4, 0.25, 0.25, 0.10))

fin_df <- data.frame(
  request_id = paste0("fin_", 1:n2),
  endpoint = ep_sample2,
  http_method = ifelse(ep_sample2 == "/account/balance", "GET", "POST"),
  concurrent_requests = round(rnorm(n2, mean = 80, sd = 25)),
  payload_size_kb = round(ifelse(ep_sample2 == "/kyc/document", rnorm(n2, 80, 20), rnorm(n2, 5, 2)), 2),
  cpu_usage_pct = round(pmin(99, pmax(10, rnorm(n2, mean = 48, sd = 12))), 1),
  mem_usage_pct = round(pmin(98, pmax(20, rnorm(n2, mean = 55, sd = 8))), 1),
  db_query_count = ifelse(ep_sample2 == "/transfer/wire", sample(5:9, n2, replace = TRUE),
                   ifelse(ep_sample2 == "/fraud/evaluate", sample(8:15, n2, replace = TRUE), sample(1:3, n2, replace = TRUE))),
  network_latency_ms = round(rnorm(n2, mean = 18, sd = 4), 1),
  cache_hit = ifelse(ep_sample2 == "/account/balance", sample(c(0, 1), n2, replace = TRUE, prob = c(0.5, 0.5)), 0),
  stringsAsFactors = FALSE
)

# FinTech response time calculation (heavy compliance & cryptographic checks)
fin_base <- c("/account/balance" = 25, "/transfer/wire" = 220, "/fraud/evaluate" = 350, "/kyc/document" = 420)
fin_df$response_time_ms <- round(
  (fin_base[fin_df$endpoint] + 
   fin_df$db_query_count * 18 + 
   fin_df$payload_size_kb * 0.6 + 
   fin_df$concurrent_requests * 0.12) * 
  ifelse(fin_df$cpu_usage_pct > 80, 1 + exp((fin_df$cpu_usage_pct - 80)/7), 1.0) *
  ifelse(fin_df$cache_hit == 1, 0.25, 1.0) +
  rnorm(n2, 0, 8), 2
)
fin_df$response_time_ms <- pmax(12, fin_df$response_time_ms)
write.csv(fin_df, "data/fintech_banking_sample.csv", row.names = FALSE)

cat("[SUCCESS] Generated sample projects:\n - data/ecommerce_api_sample.csv\n - data/fintech_banking_sample.csv\n")
