make_block <-function (block, t = NULL, max_lag = 3, tau = 1, lib = NULL) 
{
  num_vars <- NCOL(block)
  num_rows <- NROW(block)
  if (!is.null(lib)) {
    if (is.vector(lib)) 
      lib <- matrix(lib, ncol = 2, byrow = TRUE)
  }
  output <- matrix(NA, nrow = num_rows, ncol = 1 + num_vars * 
                     max_lag)
  col_names <- character(1 + num_vars * max_lag)
  if (is.null(t)) 
    output[, 1] <- 1:num_rows
  else output[, 1] <- t
  col_names[1] <- "time"
  col_index <- 2
  if (is.null(colnames(block))) 
    colnames(block) <- paste0("col", seq_len(num_vars))
  for (j in 1:num_vars) {
    ts <- block[, j]
    output[, col_index] <- ts
    col_names[col_index] <- colnames(block)[j]
    col_index <- col_index + 1
    if (max_lag > 1) {
      for (i in 1:(max_lag - 1)) {
        ts <- c(rep_len(NA, tau), ts[1:(num_rows - tau)])
        if (!is.null(lib)) {
          for (k in 1:NROW(lib)) {
            ts[lib[k, 1] - 1 + (1:tau)] <- NA
          }
        }
        output[, col_index] <- ts
        col_names[col_index] <- paste0(colnames(block)[j], 
                                       "_", i * tau)
        col_index <- col_index + 1
      }
    }
  }
  if (!is.null(lib)) {
    row_idx <- sort(unique(do.call(c, mapply(seq, lib[, 1], 
                                             lib[, 2], SIMPLIFY = FALSE))))
    output <- output[row_idx, ]
  }
  output <- data.frame(output)
  names(output) <- col_names
  return(output)
}