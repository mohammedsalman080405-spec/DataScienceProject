# ==============================================================================
# Script: R/05_code_scanner.R
# Purpose: Static Code Analysis Engine to inspect ANY project folder by PATH.
#          Extracts endpoints, DB queries, CPU load, and calculates performance.
# ==============================================================================

scan_project_folder <- function(project_dir) {
  project_dir <- path.expand(project_dir)
  if (!dir.exists(project_dir)) {
    return(list(
      df = data.frame(),
      status = "ERROR",
      message = sprintf("Directory does not exist: '%s'. Please verify the path.", project_dir),
      framework = "Unknown",
      project_name = basename(project_dir),
      total_files = 0
    ))
  }
  
  norm_path <- normalizePath(project_dir)
  project_name <- basename(norm_path)
  
  # Try reading project name from package.json or pyproject.toml
  pkg_json <- file.path(norm_path, "package.json")
  if (file.exists(pkg_json)) {
    txt <- paste(readLines(pkg_json, warn = FALSE), collapse = " ")
    m <- regmatches(txt, regexec('"name"\\s*:\\s*"([^"]+)"', txt))[[1]]
    if (length(m) >= 2) project_name <- m[2]
  }
  
  # Find all source code files
  extensions <- c("\\.py$", "\\.js$", "\\.ts$", "\\.java$", "\\.go$", "\\.php$")
  all_files <- list.files(norm_path, recursive = TRUE, full.names = TRUE)
  source_files <- all_files[grepl(paste(extensions, collapse = "|"), all_files, ignore.case = TRUE)]
  
  # Exclude build and dependency directories
  source_files <- source_files[!grepl("node_modules|venv|\\.git|build|dist|target|vendor|\\.next", source_files)]
  
  # Detect project framework
  framework <- "Generic REST API"
  if (file.exists(file.path(norm_path, "package.json")) || any(grepl("\\.(js|ts)$", source_files))) {
    framework <- "Node.js (Express / Fastify)"
  } else if (file.exists(file.path(norm_path, "requirements.txt")) || any(grepl("\\.py$", source_files))) {
    framework <- "Python (FastAPI / Flask)"
  } else if (file.exists(file.path(norm_path, "pom.xml")) || any(grepl("\\.java$", source_files))) {
    framework <- "Java (Spring Boot)"
  } else if (file.exists(file.path(norm_path, "go.mod")) || any(grepl("\\.go$", source_files))) {
    framework <- "Go (Gin / Fiber)"
  }
  
  if (length(source_files) == 0) {
    return(list(
      df = data.frame(),
      status = "NO_SOURCE_FILES",
      message = sprintf("No source code files found in '%s'.", project_dir),
      framework = framework,
      project_name = project_name,
      total_files = 0
    ))
  }
  
  endpoints_list <- list()
  
  for (f in source_files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    if (length(lines) == 0) next
    rel_path <- sub(paste0("^", norm_path, "/?"), "", normalizePath(f))
    
    # Identify line indices of all routes in this file
    route_indices <- c()
    for (idx in seq_along(lines)) {
      l <- lines[idx]
      is_express <- grepl("(app|router)[.](get|post|put|delete|patch)[[:space:]]*[(][[:space:]]*['\"`]/", l, ignore.case = TRUE)
      is_python  <- grepl("@(app|router)[.](get|post|put|delete|route)[[:space:]]*[(][[:space:]]*['\"`]/", l, ignore.case = TRUE)
      is_spring  <- grepl("@(Get|Post|Put|Delete)Mapping[[:space:]]*[(]", l, ignore.case = TRUE)
      is_go      <- grepl("[.](GET|POST|PUT|DELETE)[[:space:]]*[(][[:space:]]*['\"`]/", l)
      
      if (is_express || is_python || is_spring || is_go) {
        route_indices <- c(route_indices, idx)
      }
    }
    
    for (k in seq_along(route_indices)) {
      i <- route_indices[k]
      line <- lines[i]
      
      next_route_line <- if (k < length(route_indices)) route_indices[k + 1] - 1 else length(lines)
      end_idx <- min(next_route_line, i + 50)
      
      method <- NULL
      endpoint <- NULL
      
      # Match Node.js / Express
      m_exp <- regexec("(app|router)[.](get|post|put|delete|patch)[[:space:]]*[(][[:space:]]*['\"`](/[^'\"` ]*)['\"`]", line, ignore.case = TRUE)
      res_exp <- regmatches(line, m_exp)[[1]]
      
      # Match Python FastAPI / Flask
      m_py <- regexec("@(app|router)[.](get|post|put|delete|route)[[:space:]]*[(][[:space:]]*['\"`](/[^'\"` ]*)['\"`]", line, ignore.case = TRUE)
      res_py <- regmatches(line, m_py)[[1]]
      
      # Match Java Spring Boot
      m_sp <- regexec("@(Get|Post|Put|Delete)Mapping[[:space:]]*[(][^'\"`]*['\"`](/[^'\"` ]*)['\"`]", line, ignore.case = TRUE)
      res_sp <- regmatches(line, m_sp)[[1]]
      
      # Match Go Gin
      m_go <- regexec("[.](GET|POST|PUT|DELETE)[[:space:]]*[(][[:space:]]*['\"`](/[^'\"` ]*)['\"`]", line)
      res_go <- regmatches(line, m_go)[[1]]
      
      if (length(res_exp) >= 4) {
        method <- toupper(res_exp[3])
        endpoint <- res_exp[4]
      } else if (length(res_py) >= 4) {
        raw_m <- tolower(res_py[3])
        method <- if (raw_m == "route") "GET" else toupper(raw_m)
        endpoint <- res_py[4]
      } else if (length(res_sp) >= 3) {
        method <- toupper(sub("Mapping", "", res_sp[2]))
        endpoint <- res_sp[3]
      } else if (length(res_go) >= 3) {
        method <- toupper(res_go[2])
        endpoint <- res_go[3]
      }
      
      if (!is.null(endpoint)) {
        body_lines <- lines[i:end_idx]
        body_text <- paste(body_lines, collapse = "\n")
        
        # Database Queries Count
        db_matches <- gregexpr("(\\.find|\\.findOne|\\.findMany|\\.create|\\.save|\\.delete|\\.update|SELECT|INSERT|UPDATE|\\.execute|prisma\\.|mongoose\\.|db\\.query|db\\.execute)", body_text, ignore.case = TRUE)[[1]]
        db_queries <- if (db_matches[1] == -1) 0 else length(db_matches)
        
        # Caching
        has_cache <- grepl("(redis|cache|memcached|cacheable|@cache)", body_text, ignore.case = TRUE)
        
        # CPU Complexity
        heavy_cpu <- grepl("(bcrypt|hash|crypto|jwt|sort|while|nested|transform|worker|spawn|sha256)", body_text, ignore.case = TRUE)
        
        # File / Upload Handling
        file_upload <- grepl("(multer|upload|multipart|file|blob|stream|buffer|UploadFile)", body_text, ignore.case = TRUE)
        
        # Estimated features
        est_payload_kb <- if (file_upload) 45.0 else if (method %in% c("POST", "PUT")) 12.0 else 1.5
        est_cpu <- pmin(95, 18 + (db_queries * 7) + (if (heavy_cpu) 25 else 0) + (if (method == "POST") 10 else 0))
        est_mem <- pmin(90, 22 + (db_queries * 4.5) + (if (file_upload) 35 else 0))
        
        base_latency <- switch(method,
          "GET" = 20,
          "POST" = 45,
          "PUT" = 40,
          "DELETE" = 30,
          25
        )
        
        db_time <- db_queries * 16.0
        payload_overhead <- est_payload_kb * 0.35
        cpu_mult <- if (est_cpu > 75) 1.0 + exp((est_cpu - 75) / 8.0) else 1.0 + (est_cpu / 160)
        net_latency <- 20.0
        
        pred_latency <- round((base_latency + db_time + payload_overhead + (if (heavy_cpu) 35 else 0)) * 
                              cpu_mult * (if (has_cache) 0.35 else 1.0) + net_latency, 1)
        
        sla_limit <- switch(endpoint,
          "/health" = 50,
          "/api/v1/ping" = 50,
          if (method == "GET") 180 else 350
        )
        
        status <- if (pred_latency > sla_limit) {
          "CRITICAL BREACH"
        } else if (pred_latency > (0.8 * sla_limit)) {
          "WARNING"
        } else {
          "OPTIMAL HEALTH"
        }
        
        advice <- c()
        if (db_queries >= 4) advice <- c(advice, sprintf("%d DB queries detected - consider query batching or indexes.", db_queries))
        if (heavy_cpu) advice <- c(advice, "Cryptographic/CPU hashing in event loop.")
        if (has_cache) advice <- c(advice, "Redis caching implemented.")
        if (file_upload) advice <- c(advice, "Multipart file upload (memory & I/O heavy).")
        if (length(advice) == 0) advice <- c("Clean, optimal execution path.")
        
        endpoints_list[[length(endpoints_list) + 1]] <- data.frame(
          Endpoint = endpoint,
          Method = method,
          Source_File = paste0(rel_path, ":L", i),
          DB_Queries = as.integer(db_queries),
          Payload_KB = est_payload_kb,
          Est_CPU_Pct = round(est_cpu, 1),
          Est_RAM_Pct = round(est_mem, 1),
          Cache_Layer = ifelse(has_cache, "YES", "NO"),
          Predicted_Latency_ms = pred_latency,
          SLA_Limit_ms = sla_limit,
          SLA_Status = status,
          Bottleneck_Summary = paste(advice, collapse = " | "),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(endpoints_list) == 0) {
    return(list(
      df = data.frame(),
      status = "NO_ENDPOINTS",
      message = sprintf("Scanned %d source files, but no REST routes were found in '%s'.", length(source_files), project_dir),
      framework = framework,
      project_name = project_name,
      total_files = length(source_files)
    ))
  }
  
  res_df <- do.call(rbind, endpoints_list)
  res_df <- res_df[!duplicated(paste(res_df$Endpoint, res_df$Method)), ]
  rownames(res_df) <- NULL
  
  return(list(
    df = res_df,
    status = "SUCCESS",
    message = sprintf("Scanned %d files. Discovered %d endpoints.", length(source_files), nrow(res_df)),
    framework = framework,
    project_name = project_name,
    total_files = length(source_files)
  ))
}
