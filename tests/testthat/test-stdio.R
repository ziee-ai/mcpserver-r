skip_if_not_installed("processx")
skip_on_cran()
# NB: this test deliberately runs under R CMD check (no _R_CHECK_PACKAGE_NAME_
# skip). It exercises the stdio daemon-backed tools/call path, which hung on
# Windows; keeping it under check is the regression guard.

test_that("stdio transport replies to initialize and tools/list", {
  runner <- system.file("everything", "run-stdio.R",
                        package = "mcpserver")
  skip_if(!nzchar(runner) || !file.exists(runner),
          "run-stdio.R not installed")

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new(
    "Rscript", c(runner),
    stdin = "|", stdout = "|", stderr = "|",
    env = child_env)
  withr::defer(p$kill())

  send_msg <- function(p, msg) {
    line <- paste0(jsonlite::toJSON(msg, auto_unbox = TRUE), "\n")
    p$write_input(line)
  }
  # Persistent line buffer — subsequent calls consume queued lines without
  # losing data when the subprocess flushes multiple replies at once.
  buf <- new.env(parent = emptyenv())
  buf$lines <- character(0L)
  buf$partial <- ""
  read_line <- function(p, timeout_ms = 20000) {
    if (length(buf$lines) > 0L) {
      out <- buf$lines[[1L]]
      buf$lines <- buf$lines[-1L]
      return(out)
    }
    t0 <- Sys.time()
    while (difftime(Sys.time(), t0, units = "secs") < timeout_ms/1000) {
      p$poll_io(200)
      chunk <- p$read_output()
      if (nchar(chunk) > 0L) {
        buf$partial <- paste0(buf$partial, chunk)
        if (grepl("\n", buf$partial, fixed = TRUE)) {
          parts <- strsplit(buf$partial, "\n", fixed = TRUE)[[1L]]
          if (endsWith(buf$partial, "\n")) {
            buf$lines <- c(buf$lines, parts)
            buf$partial <- ""
          } else {
            buf$lines <- c(buf$lines, parts[-length(parts)])
            buf$partial <- parts[[length(parts)]]
          }
          # Drop blank lines.
          buf$lines <- buf$lines[nzchar(buf$lines)]
          if (length(buf$lines) > 0L) {
            out <- buf$lines[[1L]]
            buf$lines <- buf$lines[-1L]
            return(out)
          }
        }
      }
      if (!p$is_alive()) break
    }
    NA_character_
  }

  send_msg(p, list(jsonrpc = "2.0", id = 1, method = "initialize",
                   params = list(protocolVersion = "2025-06-18",
                                 capabilities = list())))
  resp_line <- read_line(p)
  expect_false(is.na(resp_line),
               info = paste("stderr:",
                            paste(p$read_error_lines(), collapse = " | ")))
  init <- jsonlite::fromJSON(resp_line, simplifyVector = FALSE)
  expect_equal(init$result$protocolVersion, "2025-06-18")

  send_msg(p, list(jsonrpc = "2.0",
                   method = "notifications/initialized"))

  send_msg(p, list(jsonrpc = "2.0", id = 2, method = "tools/list"))
  resp_line <- read_line(p)
  expect_false(is.na(resp_line))
  tl <- jsonlite::fromJSON(resp_line, simplifyVector = FALSE)
  names <- vapply(tl$result$tools, function(t) t$name, character(1L))
  expect_true("echo" %in% names)

  send_msg(p, list(jsonrpc = "2.0", id = 3, method = "tools/call",
                   params = list(name = "echo",
                                 arguments = list(message = "stdio works"))))
  resp_line <- read_line(p, timeout_ms = 60000)
  expect_false(is.na(resp_line),
               info = paste("stderr:",
                            paste(p$read_error_lines(), collapse = " | ")))
  tc <- jsonlite::fromJSON(resp_line, simplifyVector = FALSE)
  expect_equal(tc$result$content[[1L]]$text, "Echo: stdio works")
})
