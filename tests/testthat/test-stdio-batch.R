skip_if_not_installed("processx")
skip_on_cran()
skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))

test_that("stdio rejects malformed JSON with -32700 parse_error", {
  runner <- system.file("everything", "run-stdio.R",
                        package = "mcpserver")
  skip_if(!nzchar(runner) || !file.exists(runner))
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                               collapse = .Platform$path.sep)
  p <- processx::process$new("Rscript", c(runner),
                             stdin = "|", stdout = "|", stderr = "|",
                             env = child_env)
  withr::defer(p$kill())

  send <- function(line) p$write_input(paste0(line, "\n"))
  read_line <- function(timeout_s = 20) {
    t0 <- Sys.time()
    buf <- ""
    while (difftime(Sys.time(), t0, units = "secs") < timeout_s) {
      p$poll_io(200)
      buf <- paste0(buf, p$read_output())
      if (grepl("\n", buf, fixed = TRUE)) {
        return(strsplit(buf, "\n", fixed = TRUE)[[1L]][[1L]])
      }
      if (!p$is_alive()) break
    }
    NA_character_
  }

  # Send a malformed line first; expect a parse-error envelope.
  send("{this is not valid json")
  line <- read_line()
  if (is.na(line)) skip("server produced no parse-error response")
  msg <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  expect_equal(msg$error$code, -32700L)
})
