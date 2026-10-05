# ⚡ API Performance Prediction Prototype (R)

A Machine Learning prototype developed in **R** to predict API response times (latency), detect potential **SLA (Service Level Agreement) breaches**, and isolate server bottlenecks before production impact.

---

## 🎯 Project Overview

In microservices and web architectures, API latency degradation often goes unnoticed until SLA violations occur. This prototype addresses that challenge through proactive modeling:
- **Continuous Latency Regression**: Predicts API latency (`response_time_ms`) given endpoint characteristics, workload intensity, and infrastructure telemetry.
- **SLA Breach Risk Forecasting**: Determines whether incoming request traffic will exceed predefined latency limits (e.g., 150ms for profile reads vs. 500ms for checkout operations).
- **Automated Root-Cause Diagnostics**: Isolates whether latency spikes stem from **CPU queuing saturation**, **database query load**, **payload size**, or **cache misses**.
- **Interactive Prototyping UI**: Features an interactive **R Shiny** dashboard for real-time what-if scenario testing and capacity planning.

---

## 📁 Project Structure

```text
DataScience/
├── R/
│   ├── 01_data_generation.R   # Simulates realistic API telemetry based on queueing theory
│   ├── 02_eda.R               # Exploratory Data Analysis & visual distribution plots
│   ├── 03_model_training.R    # Trains Linear Regression, Decision Tree, and Random Forest
│   └── 04_predict.R           # Reusable inference engine with bottleneck root-cause advisor
├── data/
│   └── api_metrics_data.csv   # Synthetic benchmark dataset (5,000 transactions)
├── models/
│   ├── api_lm_model.rds       # Serialized Linear Regression model
│   ├── api_rpart_model.rds    # Serialized Decision Tree model
│   ├── api_rf_model.rds       # Serialized Random Forest model
│   └── model_metadata.rds     # Evaluation metrics, factor levels, and training metadata
├── plots/
│   ├── 01_latency_distribution.png
│   ├── 02_latency_by_endpoint.png
│   ├── 03_cpu_vs_latency_scatter.png
│   ├── 04_concurrent_load_sla.png
│   ├── 05_model_benchmark_bar.png
│   ├── 06_actual_vs_predicted.png
│   └── 07_feature_importance.png
├── app.R                      # Interactive R Shiny web dashboard prototype
├── run_pipeline.R             # Master end-to-end execution runner
└── README.md                  # Project documentation
```

---

## 🧪 Telemetry Features & Schema

| Feature | Type | Description |
| :--- | :--- | :--- |
| `endpoint` | Categorical | Target API route (`/users`, `/products`, `/search`, `/checkout`, `/reports`) |
| `http_method` | Categorical | HTTP verb (`GET`, `POST`, `PUT`) |
| `concurrent_requests` | Integer | Number of simultaneous active requests (1 – 500) |
| `payload_size_kb` | Numeric | Size of the request payload in Kilobytes |
| `cpu_usage_pct` | Numeric | Server host CPU utilization percentage (0 – 100%) |
| `mem_usage_pct` | Numeric | Server host RAM utilization percentage (0 – 100%) |
| `db_query_count` | Integer | Downstream database queries executed by endpoint |
| `network_latency_ms`| Numeric | External network round-trip ping time (ms) |
| `cache_hit` | Binary | `1` if cached in Redis/Memcached; `0` if cache miss |
| **`response_time_ms`** | **Target (Numeric)** | **Total API turnaround latency in milliseconds** |
| **`sla_breach`** | **Target (Binary)** | **`1` if response_time > SLA threshold, else `0`** |

---

## 📊 Model Benchmark Results (Holdout Test Set $N = 1,000$)

| Model Algorithm | RMSE (ms) | MAE (ms) | MAPE (%) | $R^2$ Score | SLA Breach Accuracy | SLA Precision | SLA Recall |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **Random Forest (150 trees)** | **149.37** | **17.22** | **8.02%** | **0.669** | **98.10%** | **78.57%** | **40.74%** |
| Decision Tree (`rpart`) | 179.13 | 33.65 | 20.75% | 0.524 | 97.40% | 100.00% | 3.70% |
| Multiple Linear Regression | 164.98 | 37.39 | 34.59% | 0.596 | 91.90% | 17.07% | 51.85% |

### Key Findings
1. **Queueing Saturation is Non-Linear**: When server CPU exceeds **75–80%**, response times surge exponentially due to thread pool starvation and request queuing. Tree ensembles (Random Forest) accurately capture this threshold behavior compared to linear models.
2. **Database Queries are Primary Bottlenecks**: `db_query_count` exhibits a strong linear correlation ($r \approx 0.795$) with response time, making database indexing and query optimization the highest ROI fix for slow endpoints.
3. **Caching Impact**: Cache hits reduce latency by approximately 70–75% by bypassing downstream database serialization.

---

## 🚀 How to Run the Prototype

### 1. Run the Entire Pipeline in One Command
From your terminal in `DataScience/`:
```bash
Rscript run_pipeline.R
```
This automatically:
1. Generates 5,000 synthetic telemetry records.
2. Runs EDA and saves high-resolution distribution graphs to `plots/`.
3. Trains and benchmarks the 3 machine learning models.
4. Serializes models to `models/`.
5. Executes sample scenario predictions with automated bottleneck diagnoses.

### 2. Launch the Interactive Shiny Dashboard
```bash
Rscript -e "shiny::runApp('.', port=3838, host='127.0.0.1')"
```
Then open your browser at:
👉 **[http://127.0.0.1:3838](http://127.0.0.1:3838)**

**Features of the Dashboard**:
- Real-time sliders for load, CPU, RAM, payload size, DB queries, and endpoint.
- Live predicted latency with SLA status badges (`HEALTHY`, `WARNING`, `CRITICAL BREACH`).
- Dynamic capacity planning curve showing exactly at what concurrency level the endpoint breaches SLA.
- Automated bottleneck alert recommendations.

### 3. Programmatic Inference in R
```r
source("R/04_predict.R")

# Predict latency for a checkout request under heavy load
result <- predict_api_performance(
  endpoint = "/api/v1/checkout",
  http_method = "POST",
  concurrent_requests = 350,
  cpu_usage_pct = 85.0,
  db_query_count = 7,
  payload_size_kb = 25.0,
  cache_hit = 0
)

print(result$predicted_response_time_ms) # e.g. 1540 ms
print(result$status)                     # "CRITICAL_SLA_BREACH"
print(result$bottlenecks)
```
