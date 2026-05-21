# Runs the official `@modelcontextprotocol/conformance server` suite
# against our HTTP server. The suite is invoked as a subprocess; we
# spawn the conformance fixture server, then exec the conformance CLI
# and assert exit code 0 (all scenarios pass).
#
# Skipped when node or the conformance suite isn't installed.

skip_if_not_installed("processx")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

node_path <- Sys.which("node")
skip_if(!nzchar(node_path), "node not on PATH")

parity_dir <- system.file("parity", package = "mcpserver")
conformance_bin <- file.path(parity_dir, "node_modules", ".bin",
                             "conformance")
skip_if(!file.exists(conformance_bin),
        paste("@modelcontextprotocol/conformance not installed under",
              parity_dir, "- run `npm install` there"))

test_that("official @modelcontextprotocol/conformance suite passes for 2025-06-18", {
  runner <- system.file("conformance", "run.R", package = "mcpserver")
  skip_if(!file.exists(runner), "conformance fixture not installed")

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  port <- 42600L + sample.int(200L, 1L)
  srv <- processx::process$new(
    "Rscript", c(runner, "--port", as.character(port)),
    stdout = "|", stderr = "|", env = child_env)
  withr::defer(srv$kill())
  Sys.sleep(3)

  proc <- processx::run(
    conformance_bin,
    c("server", "--url",
      sprintf("http://127.0.0.1:%d/mcp", port),
      "--spec-version", "2025-06-18"),
    error_on_status = FALSE,
    timeout = 180)
  # Conformance CLI exits 0 when every scenario passes. Surface a
  # snippet of stderr in the failure message for diagnostics.
  expect_equal(proc$status, 0L,
    info = paste("conformance failed; stderr:",
                 substr(proc$stderr, 1L, 600L),
                 "stdout:",
                 substr(proc$stdout, 1L, 600L)))
})
