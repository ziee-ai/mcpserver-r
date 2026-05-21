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
  if (length(args) > 0L) {
    i <- match("--port", args)
    if (!is.na(i) && i < length(args)) {
      port <- as.integer(args[[i + 1L]])
    }
  }
  source(system.file("everything", "server.R", package = "mcpserver"))
  mcp <- build_everything_server()
  mcpserver::serve_http(mcp, port = port)
}, error = function(e) {
  message("run-http.R fatal: ", conditionMessage(e))
  quit(status = 1L, save = "no")
})
