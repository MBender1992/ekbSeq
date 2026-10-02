# Shared validation and legacy compatibility state.
.ekbseq_legacy_seen <- new.env(parent = emptyenv())
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

.legacy_api_message <- function(old, replacement) {
  if (!exists(old, envir = .ekbseq_legacy_seen, inherits = FALSE)) {
    assign(old, TRUE, envir = .ekbseq_legacy_seen)
    if (identical(old, replacement)) {
      message(sprintf("`%s()` uses the legacy ekbSeq API. It remains supported for reproducibility.", old))
    } else {
      message(sprintf("`%s()` uses the legacy ekbSeq API. It remains supported for reproducibility. For new analyses, use `%s()`.", old, replacement))
    }
  }
  invisible(NULL)
}

.require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for this function. Please install it first.", package), call. = FALSE)
  }
  invisible(TRUE)
}

.check_columns <- function(data, columns) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.legacy_object <- function(name, environment) {
  if (!exists(name, envir = environment, inherits = TRUE)) {
    stop("The legacy call requires an object named '", name,
         "' in the calling environment. Pass it explicitly to the canonical function.", call. = FALSE)
  }
  get(name, envir = environment, inherits = TRUE)
}

# Symbols used intentionally through data masking in historical compatibility code.
# Declaring them avoids false-positive R CMD check notes without altering legacy logic.
utils::globalVariables(c(
  ".", "ENSEMBL", "SYMBOL", "cell", "gene", "label", "pval", "score", "vsd"
))
