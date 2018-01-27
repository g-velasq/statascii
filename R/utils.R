wrap_tbl <- function(tbl, M = M, M1 = M1, width = getOption("width")) {
  stopifnot(is.matrix(tbl))
  if (max(nchar(tbl)) <= width) {
    cat(tbl, sep = "\n")
  }
  if (max(nchar(tbl)) > width) {
    M_rest <- M[-1] + 3L
    M_rest[1] <- M_rest[1] - 1L
    M_start <- M[-1]
    M_start[seq_along(M_start)] <- 0L
    M_start[1] <- 1L
    M_end <- M[-1]
    M_end[seq_along(M_end)] <- 0L
    M_end[1] <- M_rest[1]
    if (length(M_rest) > 1L) {
      for (i in 2L:length(M_rest)) {
        M_end[i] <- M_end[i - 1L] + M_rest[i]
        M_start[i] <- M_end[i - 1L] + 1L
      }
    }
    col_one <- as.matrix(str_sub(tbl, start = 1L, end = M1[1] + 4L))
    col_rest <- as.matrix(str_sub(tbl, start = M1[1] + 5L, end = -1L))
    col_position <- matrix(c(M_start, M_end), ncol = 2L)
    all_cols <- list()
    if (length(M_rest) > 1L) {
      for (i in 1L:length(M_rest)) {
        all_cols[[i + 1L]] <-
          as.matrix(stringr::str_sub(col_rest, col_position[i, 1], col_position[i, 2]))
      }
    }
    all_cols[[1]] <- col_one
    col_widths <- vector(mode = "integer", length = length(all_cols))
    col_sums <- vector(mode = "integer", length = length(all_cols))
    col_index <- vector(mode = "integer", length = length(all_cols))
    wrap_count <- 1L
    for (i in 1L:length(all_cols)) {
      col_widths[i] <- max(nchar(all_cols[[i]]))
      col_index[i] <- wrap_count
    }
    col_sums[1] <- col_widths[1]
    for (i in 2L:length(all_cols)) {
      col_sums[i] <- col_widths[i] + col_sums[i - 1L]
      if (col_sums[i] > width) {
        wrap_count <- wrap_count + 1L
        col_sums[i] <- col_widths[1L] + col_widths[i]
      }
      if (wrap_count > 1L) {
        col_index[i] <- wrap_count
      }
    }
    tbl_wrapped <- vector(mode = "list", length = wrap_count)
    for (i in 1L:length(tbl_wrapped)) {
      tbl_wrapped[i] <- all_cols[1]
    }
    for (i in 2L:length(col_index)) {
      current_list <- col_index[i]
      tbl_wrapped[[current_list]] <- as.matrix(
        paste0(as.matrix(unlist(tbl_wrapped[current_list])),
               as.matrix(unlist(all_cols[i])))
      )
    }
    for (i in 1L:length(tbl_wrapped)) {
      cat(tbl_wrapped[[i]], sep = "\n")
      if (i < length(tbl_wrapped)) {
        cat("\n")
      }
    }
  }
}