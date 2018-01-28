#' Create Stata-like tables in the R console
#' 
#' @name statascii
#' 
#' @importFrom rlang %|%
#' 
#' @usage statascii(df, ..., flavor = "oneway", padding = "stata", separators = FALSE)
#' @param df A data.frame or tibble.
#' @param ... A comma separated list of unquoted variable names. Use `desc()` to sort a variable in descending order.
#' @param flavor Choose 'oneway', 'twoway', or 'summary'.
#' @param padding Choose 'stata', 'summary', or 'none'.
#' @param separators Tabbed row separators when tabulating more than one variable.
#'
#' @return A Stata-like formatted table is displayed in the console. An unformatted tibble is invisibly returned.
#' 
#' @examples
#' # setup
#' library(dplyr)
#' library(stringr)
#' 
#' # a. demonstrate 'oneway' flavor for one-way tables of frequencies
#' a <- mtcars %>% count(gear) %>% rename(Freq. = n)
#' a <- a %>% add_row(gear = "Total", Freq. = sum(a[, 2]))
#' statascii(a, flavor = "oneway")
#' 
#' # b. demonstrate 'oneway' flavor with no padding
#' b <- mtcars %>% count(gear) %>% rename(Freq. = n)
#' b <- b %>% add_row(gear = "Total", Freq. = sum(b[, 2]))
#' statascii(b, flavor = "oneway", padding = "none")
#' 
#' # c. demonstrate 'twoway' flavor for n-way tables of frequencies
#' c <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
#' c <- c %>% ungroup() %>% add_row(gear = "Total", carb = "", am = "", Freq. = sum(c[, 4]))
#' statascii(c, flavor = "twoway")
#' 
#' # d. demonstrate 'twoway' flavor with dashed group separator
#' d <- mtcars %>% count(gear, carb, am) %>% rename(Freq. = n)
#' d <- d %>% ungroup() %>% add_row(gear = "Total", carb = "", am = "", Freq. = sum(d[, 4]))
#' statascii(d, flavor = "twoway", separators = TRUE)
#' 
#' # e. demonstrate 'summary' flavor for summary statistics
#' e <- mtcars %>% group_by(gear) %>% summarize(
#'   Obs = n(),
#'   Mean = mean(gear),
#'   "Std. Dev." = sd(gear),
#'   Min = min(gear),
#'   Max = max(gear)
#' )
#' statascii(e, flavor = "summary")
#' 
#' # f. demonstrate wrapping feature for wide tables
#' f <- mtcars %>%
#'   mutate(cyl2 = cyl, vs2 = vs, am2 = am, carb2 = carb) %>%
#'   filter(gear != 5) %>%
#'   count(gear, carb, am, vs, cyl, carb2, am2, vs2, cyl2) %>%
#'   rename(Freq. = n) %>%
#'   ungroup()
#' f <- f %>% add_row(gear = "Total", Freq. = sum(f[, 10]))
#' f[is.na(f)] <- ""
#' statascii(f, flavor = "oneway", separators = TRUE)

#' @export
statascii <- function(df, ..., flavor = "oneway", padding = "stata", pad = 1L, separators = FALSE) {
  stopifnot(is.data.frame(df))
  if (ncol(df) <= 2L & flavor == "twoway") {
    stop("data frame must have at least three columns for 'twoway' flavor",
         call. = FALSE)
  }
  if (ncol(df) <= 1L) {
    stop("data frame must have at least two columns", call. = FALSE)
  }
  df <- as.matrix(sapply(df, as.character))
  if (ncol(df) == 1L) {
    df <- t(df)
  }
  if (padding == "stata") {
    colnames(df) <- stringr::str_pad(colnames(df), 9L, pad = " ")
  }
  if (padding == "summary") {
    colnames(df) <- stringr::str_pad(colnames(df), 5L, pad = " ")
  }
  else if (padding == "none") {
  }
  add_line <- function(n, pad = 1L) {
    tmp <- lapply(n, function(x, pad)
      paste0(rep("\u2500", x + (2L * pad)),
             collapse = ""),
      pad = pad)
    paste0("\u2500", paste0(tmp, collapse = "\u253c"))
  }
  add_dash <- function(n, pad = 1L) {
    tmp <- lapply(n, function(x, pad)
      paste0(rep("-", x + (2L * pad)),
             collapse = ""),
      pad = pad)
    paste0("-", paste0(tmp, collapse = "\u253c"))
  }
  add_row_oneway <- function(x, n, pad = 1L) {
    reformat <- function(i, x, n) {
      fmt <- paste0("%", n[i], "s")
      sprintf(fmt, as.character(x[i]))
    }
    row_content <- sapply(seq_along(x), reformat, x = x, n = n)
    paste0(" ",
           paste0(paste0(rep(" ", pad), row_content[1], rep(" ", pad)), collapse = ""),
           "\u2502",
           paste0(paste0(rep(" ", pad), row_content[-1], rep(" ", pad)), collapse = " ")
    )
  }
  add_row_twoway <- function(x, n, pad = 1L) {
    reformat <- function(i, x, n) {
      fmt <- paste0("%", n[i], "s")
      sprintf(fmt, as.character(x[i]))
    }
    row_content <- sapply(seq_along(x), reformat, x = x, n = n)
    paste0(
      paste0(paste0(rep(" ", pad), row_content[1], rep(" ", pad)), collapse = ""),
      "\u2502",
      paste0(paste0(rep(" ", pad), row_content[2:(length(row_content) - 1L)], rep(" ", pad)), collapse = ""),
      "\u2502",
      paste0(paste0(rep(" ", pad), row_content[length(row_content)], rep(" ", pad)), collapse = " ")
    )
  }
  nchar_content <- apply(df, 2, function(x) {
    max(nchar(x, keepNA = FALSE))
  })
  nchar_names <- nchar(colnames(df), keepNA = FALSE)
  M <- pmax(nchar_content, nchar_names)
  M1 <- as.integer(c(M[1],
                     sum(M[2:(length(M))]) + (3L * ncol(df)) - 6L))
  M2 <- as.integer(c(M[1] - 1L,
                     sum(M[2:(length(M) - 1L)],
                     (2L * ncol(df)) - 6L),
                     M[length(M)] - 1L))
  if (flavor == "oneway") {
    table_line <- add_line(M1, pad = pad)
    group_dashes <- add_dash(M1, pad = pad)
    table_captured <-
      capture.output(writeLines(add_row_oneway(colnames(df), M, pad = pad)))
    table_captured <-
      as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
    total_line <- nrow(df) - 1L
    for (i in seq_len(nrow(df))) {
      table_captured <-
        as.matrix(rbind(table_captured, capture.output(writeLines(add_row_oneway(df[i, ], M, pad = pad)))))
      if (i > 0L & i < total_line) {
        if (separators) {
          df <- df %|% "NA"
          if (df[i, 1] != df[i + 1L, 1]) {
            table_captured <-
              as.matrix(rbind(table_captured, capture.output(writeLines(group_dashes))))
          }
        }
      }
      if (i == total_line) {
        table_captured <-
          as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
      }
    }
    wrap_tbl(table_captured, M = M, M1 = M1)
  }
  else if (flavor == "twoway") {
    table_line <- add_line(M2, pad = pad)
    group_dashes <- add_dash(M2, pad = pad)
    table_captured <-
      capture.output(writeLines(add_row_twoway(colnames(df), M, pad = pad)))
    table_captured <-
      as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
    total_line <- nrow(df) - 1L
    for (i in seq_len(nrow(df))) {
      table_captured <-
        as.matrix(rbind(table_captured, capture.output(writeLines(add_row_twoway(df[i, ], M, pad = pad)))))
      if (i > 0L & i < total_line) {
        if (separators) {
          df <- df %|% "NA"
          if (df[i, 1] != df[i + 1L, 1]) {
            table_captured <-
              as.matrix(rbind(table_captured, capture.output(writeLines(group_dashes))))
          }
        }
      }
      if (i == total_line) {
        table_captured <-
          as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
      }
    }
    wrap_tbl(table_captured, M = M, M1 = M1)
  }
  else if (flavor == "summary") {
    table_line <- add_line(M1, pad = pad)
    group_dashes <- add_dash(M1, pad = pad)
    table_captured <-
      capture.output(writeLines(add_row_oneway(colnames(df), M, pad = pad)))
    table_captured <-
      as.matrix(rbind(table_captured, capture.output(writeLines(table_line))))
    for (i in seq_len(nrow(df))) {
      table_captured <-
        as.matrix(rbind(table_captured, capture.output(writeLines(add_row_oneway(df[i, ], M, pad = pad)))))
      if (i > 0L & i < nrow(df)) {
        if (separators) {
          df <- df %|% "NA"
          if (df[i, 1] != df[i + 1L, 1]) {
            table_captured <-
              as.matrix(rbind(table_captured, capture.output(writeLines(group_dashes))))
          }
        }
      }
    }
    wrap_tbl(table_captured, M = M, M1 = M1)
  }
  invisible(df)
}