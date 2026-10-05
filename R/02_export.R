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


#' Save a Plot as PNG and SVG Files Simultaneously
#'
#' This function saves a plot in both PNG and SVG formats. It supports ggplot2
#' objects, patchwork/ggarrange-like objects, base R plotting functions or
#' expressions, and grid-based plots such as ComplexHeatmap.
#'
#' PNG export uses either `ggsave()` for ggplot-like objects or a Cairo-based
#' PNG device for base/grid plotting expressions. SVG export uses either
#' `svglite::svglite()`, `Cairo::CairoSVG()`, or the base R `svg()` device.
#'
#' @param filename_base Character. Base file path without file extension.
#' @param plot_expr A plot object, function, or quoted expression to be saved.
#' @param width Numeric. Width of the plot in inches. Default is 8.
#' @param height Numeric. Height of the plot in inches. Default is 6.
#' @param dpi Numeric. Resolution in dots per inch for PNG output and rasterized layers. Default is 600.
#' @param font Character. Font family used for SVG output. Default is `"Liberation Sans"`.
#' @param rasterize Logical. If TRUE and `ggrastr` is installed, ggplot geometry layers are rasterized inside the SVG. Default is TRUE.
#' @param grid_capture Logical. If TRUE, captures grid-based plots with `grid::grid.grabExpr()` before SVG export, which can prevent empty SVG output for ComplexHeatmap-style plots. Default is FALSE.
#' @param svg_backend Character. SVG backend to use. `"svglite"` keeps text highly editable in Inkscape, `"cairo"` can be useful for some non-ggplot outputs, and `"base"` uses `grDevices::svg()` as a fallback for base graphics. Default is `"svglite"`.
#' @param base_graphics Logical. If TRUE, the plot is exported using direct base graphics devices without `grid::grid.newpage()` or grid capture. This is useful for base R or igraph-style plots such as CellChat network plots. Default is FALSE.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
export_plot_dual <- function(filename_base,
                             plot_expr = NULL,
                             width = 8,
                             height = 6,
                             dpi = 600,
                             font = "Liberation Sans",
                             rasterize = TRUE,
                             grid_capture = FALSE,
                             svg_backend = c("svglite", "cairo", "base"),
                             base_graphics = FALSE) {

  svg_backend <- match.arg(svg_backend)
  caller_env <- parent.frame()

  output_dir <- dirname(filename_base)
  if (!identical(output_dir, ".") && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  .eval_plot <- function() {
    if (is.function(plot_expr)) {
      res <- plot_expr()
    } else {
      res <- eval(plot_expr, envir = caller_env)
    }

    if (inherits(res, "grob")) {
      grid::grid.draw(res)
    }

    invisible(res)
  }

  .eval_base_plot <- function() {
    if (is.function(plot_expr)) {
      plot_expr()
    } else {
      eval(plot_expr, envir = caller_env)
    }

    invisible(NULL)
  }

  .open_svg_device <- function(filepath) {
    if (identical(svg_backend, "svglite")) {

      svglite::svglite(
        file = filepath,
        width = width,
        height = height,
        system_fonts = list(sans = font),
        fix_text_size = FALSE,
        bg = "white"
      )

    } else if (identical(svg_backend, "cairo")) {

      if (!requireNamespace("Cairo", quietly = TRUE)) {
        stop(
          "The Cairo package is required when svg_backend = 'cairo'. ",
          "Install it with: install.packages('Cairo')"
        )
      }

      Cairo::CairoSVG(
        filename = filepath,
        width = width,
        height = height,
        bg = "white"
      )

    } else if (identical(svg_backend, "base")) {

      grDevices::svg(
        filename = filepath,
        width = width,
        height = height,
        bg = "white",
        onefile = FALSE
      )
    }
  }

  .save_svg_plot_object <- function(filepath, p) {
    .open_svg_device(filepath)
    print(p)
    dev.off()
  }

  # Direct base graphics export path.
  # This must not use grid.newpage(), because base graphics and grid graphics
  # use different drawing systems.
  if (base_graphics) {

    png(
      filename = paste0(filename_base, ".png"),
      width = width,
      height = height,
      units = "in",
      res = dpi,
      type = "cairo",
      bg = "white"
    )
    .eval_base_plot()
    dev.off()

    .open_svg_device(paste0(filename_base, ".svg"))
    .eval_base_plot()
    dev.off()

    return(invisible(NULL))
  }

  is_gg_like <- inherits(plot_expr, c("gg", "ggplot", "ggarrange", "grob", "patchwork"))

  if (is_gg_like && !grid_capture) {

    # PNG export for ggplot-like objects
    ggplot2::ggsave(
      filename = paste0(filename_base, ".png"),
      plot = plot_expr,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )

    # SVG export with optional rasterization of heavy ggplot geometry layers
    if (rasterize && requireNamespace("ggrastr", quietly = TRUE)) {
      plot_svg <- ggrastr::rasterise(plot_expr, dpi = dpi, dev = "ragg")
    } else {
      if (rasterize && !requireNamespace("ggrastr", quietly = TRUE)) {
        message(
          "ggrastr was not found - SVG will be saved without rasterization.\n",
          "Install it with: install.packages('ggrastr')"
        )
      }
      plot_svg <- plot_expr
    }

    .save_svg_plot_object(paste0(filename_base, ".svg"), plot_svg)

  } else {

    # PNG export for grid or ComplexHeatmap-style plots
    png(
      filename = paste0(filename_base, ".png"),
      width = width,
      height = height,
      units = "in",
      res = dpi,
      type = "cairo",
      bg = "white"
    )
    grid::grid.newpage()
    .eval_plot()
    dev.off()

    # SVG export
    if (grid_capture) {

      # Capture grid-based plots first, then redraw the captured grob into the SVG device.
      # This is useful for ComplexHeatmap and similar grid-based plots.
      plot_grob <- grid::grid.grabExpr(
        {
          grid::grid.newpage()
          .eval_plot()
        },
        width = width,
        height = height
      )

      .open_svg_device(paste0(filename_base, ".svg"))
      grid::grid.newpage()
      grid::grid.draw(plot_grob)
      dev.off()

    } else {

      # Direct SVG export for grid-like plotting functions or expressions.
      .open_svg_device(paste0(filename_base, ".svg"))
      grid::grid.newpage()
      .eval_plot()
      dev.off()
    }
  }

  invisible(NULL)
}


#' Remove scale() Transforms and Optionally Normalize Font Sizes in SVG Text Elements
#'
#' Parses an SVG file and removes any \code{scale()} transform applied to \code{<text>}
#' and \code{<tspan>} elements, and optionally sets a uniform font size across all
#' text nodes. This is useful as a post-processing step for SVG files generated by
#' \code{svglite}, which may add \code{scale()} transforms to text nodes as a kerning
#' correction. These transforms cause text to appear squished when editing font sizes
#' in vector graphics editors such as Inkscape or Illustrator.
#'
#' @param svg_file Character. Path to the SVG file to be processed. The file is
#'   modified in place.
#' @param font_size Numeric or \code{NULL}. If provided, sets all text elements to
#'   this font size in points (pt). If \code{NULL} (default), font sizes are left
#'   unchanged and only \code{scale()} transforms are removed.
#'
#' @details
#' When \code{svglite} renders text, it occasionally applies a \code{scale(x, 1)}
#' transform to text elements to correct letter spacing. While this produces
#' visually accurate output, it interferes with font size editing in vector
#' graphics editors: changing the font size rescales the text in only one
#' dimension, causing distortion.
#'
#' This function removes all \code{scale()} components from \code{transform}
#' attributes on \code{<text>} and \code{<tspan>} nodes. If the \code{transform}
#' attribute consists solely of a \code{scale()} call, the attribute is removed
#' entirely. Other transform functions (e.g. \code{translate()}, \code{rotate()})
#' are preserved.
#'
#' If \code{font_size} is specified, the function updates the \code{font-size}
#' property in the \code{style} attribute of each text node, and also sets the
#' \code{font-size} attribute directly if present. This allows batch-normalizing
#' font sizes across an entire figure after layout adjustments in Inkscape.
#'
#' Note: if you are using \code{svglite >= 2.0.0}, consider setting
#' \code{fix_text_size = FALSE} in \code{\link[svglite]{svglite}} instead, which
#' prevents \code{textLength} constraints from being written in the first place
#' and is the preferred solution. This function is intended for post-processing
#' of existing SVG files where regeneration is not possible.
#'
#' @return Invisibly returns \code{svg_file} (the input path). The file is
#'   overwritten in place.
#'
#' @seealso \code{\link{export_plot_dual}} for generating publication-ready SVG
#'   files with \code{fix_text_size = FALSE}.
#'
#' @examples
#' \dontrun{
#' # Only remove scale() transforms, keep original font sizes
#' normalize_svg_text("figures/my_umap.svg")
#'
#' # Remove scale() transforms and set all text to 8pt
#' normalize_svg_text("figures/my_umap.svg", font_size = 8)
#'
#' # Use in combination with export_plot_dual()
#' export_plot_dual("figures/my_plot", my_ggplot)
#' normalize_svg_text("figures/my_plot.svg", font_size = 8)
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_set_attr write_xml
#' @export
normalize_svg_text <- function(svg_file, font_size = NULL) {

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required. Please install it with: install.packages('xml2')")
  }

  if (!is.null(font_size) && (!is.numeric(font_size) || length(font_size) != 1 || font_size <= 0)) {
    stop("'font_size' must be a single positive numeric value or NULL.")
  }

  svg <- xml2::read_xml(svg_file)

  text_nodes  <- xml2::xml_find_all(svg, "//*[local-name()='text']")
  tspan_nodes <- xml2::xml_find_all(svg, "//*[local-name()='tspan']")
  all_nodes   <- c(text_nodes, tspan_nodes)

  for (node in all_nodes) {

    # --- Remove scale() transforms ---
    transform <- xml2::xml_attr(node, "transform")
    if (!is.na(transform)) {
      # Remove any scale() call regardless of its arguments:
      # - scale(x, 1): x-only kerning correction added by svglite
      # - scale(1, y): would squish text vertically
      # - scale(x, y): would distort in both dimensions
      # Other transform functions (translate, rotate, etc.) are preserved.
      cleaned <- gsub("scale\\([^)]+\\)", "", transform)
      cleaned <- trimws(cleaned)

      if (nchar(cleaned) == 0) {
        xml2::xml_set_attr(node, "transform", NULL)
      } else {
        xml2::xml_set_attr(node, "transform", cleaned)
      }
    }

    # --- Optionally normalize font size ---
    if (!is.null(font_size)) {

      # Update font-size inside style attribute (e.g. "font-size:12px;fill:#000")
      style <- xml2::xml_attr(node, "style")
      if (!is.na(style)) {
        style <- gsub("font-size:\\s*[0-9.]+\\s*(?:px|pt|em|rem|%)?",
                      paste0("font-size:", font_size, "pt"),
                      style, perl = TRUE)
        xml2::xml_set_attr(node, "style", style)
      }

      # Update standalone font-size attribute if present
      if (!is.na(xml2::xml_attr(node, "font-size"))) {
        xml2::xml_set_attr(node, "font-size", paste0(font_size, "pt"))
      }
    }
  }

  xml2::write_xml(svg, svg_file)

  if (is.null(font_size)) {
    message("Removed scale() transforms from: ", svg_file)
  } else {
    message("Removed scale() transforms and set font size to ", font_size, "pt in: ", svg_file)
  }

  invisible(svg_file)
}
