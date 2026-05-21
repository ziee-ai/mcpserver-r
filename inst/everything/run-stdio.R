#!/usr/bin/env Rscript
## Launch the mcpserver everything-demo over stdio.

suppressPackageStartupMessages(library(mcpserver))
source(system.file("everything", "server.R", package = "mcpserver"))
mcp <- build_everything_server()
mcpserver::serve_io(mcp)
