#' Export one or more data frames to Excel
#' @param data Data frame or named list of data frames.
#' @param file Destination filename.
#' @param rownames Include row names.
#' @param styled Style the header, shade alternate rows and freeze the first row.
#' @return Filename, invisibly. Parent directories are created if necessary.
#' @export
export_excel <- function(data, file = "results.xlsx", rownames = FALSE, styled = TRUE) {
  .require_package("openxlsx")
  tables <- if (is.data.frame(data)) list(Sheet1 = data) else data
  if (!is.list(tables) || !length(tables) || is.null(names(tables)) ||
      anyNA(names(tables)) || any(!nzchar(names(tables))) ||
      !all(vapply(tables, is.data.frame, logical(1)))) {
    stop("data must be a data frame or a named list of data frames.")
  }
  sheet_names <- names(tables)
  for (bad in c("[", "]", ":", "*", "?", "/", "\\")) {
    sheet_names <- gsub(bad, "-", sheet_names, fixed = TRUE)
  }
  sheet_names <- substr(sheet_names, 1, 31)
  if (anyDuplicated(sheet_names)) stop("Sheet names must be unique after sanitization and truncation.")
  parent <- dirname(file)
  if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  if (styled) {
    header <- openxlsx::createStyle(fontName = "Arial", fontSize = 11,
                                     fontColour = "white", fgFill = "#4472C4",
                                     textDecoration = "bold")
    alternate <- openxlsx::createStyle(fontName = "Arial", fontSize = 10,
                                        fgFill = "#EEF2FA")
  }
  for (i in seq_along(tables)) {
    sheet <- sheet_names[i]
    frame <- tables[[i]]
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, frame, rowNames = rownames)
    if (styled) {
      n_columns <- ncol(frame) + as.integer(rownames)
      if (n_columns > 0L) {
        columns <- seq_len(n_columns)
        openxlsx::addStyle(wb, sheet, header, rows = 1L, cols = columns, gridExpand = TRUE)
        if (nrow(frame) > 1L) {
          even <- seq.int(3L, nrow(frame) + 1L, by = 2L)
          if (length(even)) openxlsx::addStyle(wb, sheet, alternate, rows = even,
                                               cols = columns, gridExpand = TRUE)
        }
        openxlsx::setColWidths(wb, sheet, cols = columns, widths = "auto")
      }
      openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    }
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  message("Saved: ", file)
  invisible(file)
}
