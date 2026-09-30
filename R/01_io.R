#' Read a transposed edgeR count export
#'
#' Reads the historical edgeR CSV layout: the first transposed row contains
#' sample identifiers and all other rows contain numeric gene counts.
#' @param file CSV file in the historical edgeR layout.
#' @return Numeric data frame with genes in rows and samples in columns.
#' @export
read_edger_counts <- function(file) {
  raw <- utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(raw) < 2L || nrow(raw) < 2L) stop("Expected at least two rows and columns in edgeR count file.")
  transposed <- t(raw)
  sample_names <- as.character(transposed[1L, ])
  values <- transposed[-1L, , drop = FALSE]
  converted <- suppressWarnings(matrix(as.numeric(values), nrow = nrow(values),
                                      dimnames = list(rownames(values), sample_names)))
  if (anyNA(converted) || any(!is.finite(converted))) stop("Count columns must be finite and numeric.")
  as.data.frame(converted, check.names = FALSE)
}
