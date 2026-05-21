# Pick a free TCP port for HTTP transport tests.
free_port <- function() {
  sock <- nanonext::socket("rep")
  on.exit(close(sock), add = TRUE)
  # Bind to an ephemeral port via the OS by trying a random one in a
  # high range. Re-roll on conflict.
  for (i in seq_len(50L)) {
    p <- sample(40000:50000, 1L)
    if (is.null(tryCatch(nanonext::listen(sock,
                                          sprintf("tcp://127.0.0.1:%d", p)),
                         error = function(e) NULL))) next
    return(p)
  }
  40123L
}
