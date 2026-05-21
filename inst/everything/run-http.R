#!/usr/bin/env Rscript
## Launch the mcpserver everything-demo over Streamable HTTP.

log_path <- Sys.getenv("MCPSERVER_LOG", "")
if (nzchar(log_path)) {
  log_con <- file(log_path, open = "a")
  sink(log_con, type = "message")
}

tryCatch({
  suppressPackageStartupMessages(library(mcpserver))
  args <- commandArgs(trailingOnly = TRUE)
  port <- 3000L
  require_origin <- TRUE
  allowed_origins <- c("http://localhost", "http://127.0.0.1")
  i <- match("--port", args)
  if (!is.na(i) && i < length(args)) port <- as.integer(args[[i + 1L]])
  if ("--no-origin-check" %in% args) require_origin <- FALSE
  i <- match("--allowed-origin", args)
  while (!is.na(i) && i < length(args)) {
    allowed_origins <- c(allowed_origins, args[[i + 1L]])
    args <- args[-c(i, i + 1L)]
    i <- match("--allowed-origin", args)
  }
  source(system.file("everything", "server.R", package = "mcpserver"))
  mcp <- build_everything_server()
  mcpserver::serve_http(mcp, port = port,
                        allowed_origins = allowed_origins,
                        require_origin = require_origin)
}, error = function(e) {
  message("run-http.R fatal: ", conditionMessage(e))
  quit(status = 1L, save = "no")
})
