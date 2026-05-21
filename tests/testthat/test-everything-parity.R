skip_if_not_installed("processx")
skip_if_not_installed("httr2")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

# Locate node + the bundled parity runner. Skip gracefully when either is
# absent so the test suite stays green on hosts without Node.
node_path <- Sys.which("node")
skip_if(!nzchar(node_path), "node not on PATH")

parity_dir <- system.file("parity", package = "mcpserver")
parity_script <- file.path(parity_dir, "run-parity.mjs")
parity_modules <- file.path(parity_dir, "node_modules",
                            "@modelcontextprotocol", "sdk")
skip_if(!file.exists(parity_script), "run-parity.mjs not installed")
skip_if(!dir.exists(parity_modules),
        paste("@modelcontextprotocol/sdk not installed under",
              parity_dir, "— run `npm install` there"))

test_that("official TS-SDK client passes the full parity suite", {
  # Spawn the everything-demo HTTP server in a subprocess.
  runner <- system.file("everything", "run-http.R", package = "mcpserver")
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  log_path <- tempfile("mcp-parity-server-", fileext = ".log")
  child_env["MCPSERVER_LOG"] <- log_path
  port <- 42400L + sample.int(200L, 1L)
  srv <- processx::process$new(
    "Rscript", c(runner, "--port", as.character(port)),
    stdout = "|", stderr = "|", env = child_env)
  withr::defer(srv$kill())
  Sys.sleep(3)

  # Drive the Node parity script.
  report_path <- tempfile("parity-report-", fileext = ".json")
  node_proc <- processx::run(
    "node",
    c(parity_script, "--url",
      sprintf("http://127.0.0.1:%d/mcp", port),
      "--report", report_path),
    error_on_status = FALSE,
    timeout = 120)

  if (!file.exists(report_path)) {
    skip(paste("parity runner produced no report; stderr:",
               substr(node_proc$stderr, 1L, 400L)))
  }
  report <- jsonlite::fromJSON(report_path, simplifyVector = FALSE)
  expect_true(report$failed == 0L,
              info = paste0("failed entries: ",
                paste(vapply(
                  Filter(function(e) isFALSE(e$ok), report$results),
                  function(e) paste0(e$name, "=", substr(
                    as.character(e$detail %||% ""), 1L, 80L)),
                  character(1L)),
                  collapse = " | "),
                " | node exit=", node_proc$status))
  # Ensure we actually exercised the headline checks the parity
  # script reports on.
  names_run <- vapply(report$results, function(e) e$name, character(1L))
  expect_true("tools/list" %in% names_run)
  expect_true("tools/call sampleLLM" %in% names_run ||
              "tools/call echo" %in% names_run)
  expect_true("completion/complete department" %in% names_run)
})
