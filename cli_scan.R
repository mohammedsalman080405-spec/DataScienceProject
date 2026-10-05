#!/usr/bin/env Rscript
# ==============================================================================
# Script: cli_scan.R
# Purpose: Command-Line Scanner to predict API performance for ANY project path
# Usage:   Rscript cli_scan.R /path/to/your/project
# ==============================================================================

source("R/05_code_scanner.R")

args <- commandArgs(trailingOnly = TRUE)
target_path <- if (length(args) > 0) args[1] else "projects/express_ecommerce_api"

cat("\n==============================================================================\n")
cat("          SCANNING PROJECT PATH FOR API PERFORMANCE PREDICTION                \n")
cat(sprintf("          Target: %s\n", target_path))
cat("==============================================================================\n\n")

res <- scan_project_folder(target_path)

if (res$status != "SUCCESS") {
  cat(sprintf("[ERROR] %s\n\n", res$message))
  quit(status = 1)
}

cat(sprintf("✔ Detected Framework: %s\n", res$framework))
cat(sprintf("✔ Total Source Files Scanned: %d\n", res$total_files))
cat(sprintf("✔ Discovered API Endpoints: %d\n\n", nrow(res$df)))

df <- res$df
format_table <- data.frame(
  Endpoint = df$Endpoint,
  Method = df$Method,
  DB_Calls = df$DB_Queries,
  CPU_Pct = paste0(df$Est_CPU_Pct, "%"),
  Cache = df$Cache_Layer,
  Latency_ms = paste0(df$Predicted_Latency_ms, " ms"),
  SLA_Limit = paste0(df$SLA_Limit_ms, " ms"),
  Status = df$SLA_Status
)

print(format_table, row.names = FALSE)

cat("\n--- Detailed Endpoint Bottleneck Summaries ---\n")
for (i in seq_len(nrow(df))) {
  cat(sprintf("[%s %s] (%s)\n", df$Method[i], df$Endpoint[i], df$Source_File[i]))
  cat(sprintf("  -> Predicted Latency: %s ms | Status: [%s]\n", df$Predicted_Latency_ms[i], df$SLA_Status[i]))
  cat(sprintf("  -> Advice: %s\n\n", df$Bottleneck_Summary[i]))
}
