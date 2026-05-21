#!/usr/bin/env Rscript
## Launch the mcpserver conformance fixture over Streamable HTTP.

tryCatch({
  suppressPackageStartupMessages(library(mcpserver))
  args <- commandArgs(trailingOnly = TRUE)
  port <- 3001L
  i <- match("--port", args)
  if (!is.na(i) && i < length(args)) port <- as.integer(args[[i + 1L]])
  source(system.file("conformance", "server.R", package = "mcpserver"))
  mcp <- build_conformance_server()
  mcpserver::serve_http(mcp, port = port,
                        require_origin = FALSE)
}, error = function(e) {
  message("conformance run.R fatal: ", conditionMessage(e))
  quit(status = 1L, save = "no")
})
