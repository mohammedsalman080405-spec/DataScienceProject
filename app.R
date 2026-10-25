# ==============================================================================
# File: app.R
# Project: API Performance Prediction & SLA Intelligence Platform
# Architecture: Production-Grade AI/API Observability Platform Server
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
})

# Serve the modern frontend dashboard
ui <- function(req) {
  html_file <- if (file.exists("index.html")) "index.html" else "www/index.html"
  shiny:::httpResponse(
    status = 200L,
    content_type = "text/html; charset=UTF-8",
    content = readBin(html_file, "raw", file.info(html_file)$size)
  )
}

server <- function(input, output, session) {
  # Server-side background support
}

shinyApp(ui = ui, server = server)
