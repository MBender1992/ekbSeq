#' Estimate a GAM threshold along pseudotime
#' @param data Cell metadata with score and pseudotime columns.
#' @param pseudotime_col,score_col Numeric metadata columns.
#' @param method `inflection` for largest positive slope, `drop` for steepest
#'   negative slope, or the selected local `maximum`/`minimum`.
#' @param which_n Index of the local extremum; out-of-range values use the last.
#' @param pseudotime_range,n_grid Prediction range and number of grid points.
#' @param lineage_assignment_col Metadata column with trajectory numbers.
#' @return A single numeric pseudotime threshold. A global extremum is used
#'   with a warning when the requested type of local extremum is absent.
#' @export
estimate_pseudotime_threshold <- function(data, pseudotime_col, score_col,
                                          method = c("inflection", "drop", "maximum", "minimum"),
                                          which_n = 1L, pseudotime_range = c(0, 0.1),
                                          n_grid = 500L,
                                          lineage_assignment_col = "lineage_assignment") {
  .require_package("mgcv")
  .check_columns(data, c(pseudotime_col, score_col, lineage_assignment_col))
  method <- match.arg(method)
  if (length(pseudotime_range) != 2L || anyNA(pseudotime_range) ||
      pseudotime_range[1L] >= pseudotime_range[2L] || n_grid < 3L) stop("Invalid prediction grid.")
  if (length(which_n) != 1L || is.na(which_n) || which_n < 1L) stop("which_n must be positive.")
  lineage_id <- .lineage_number(pseudotime_col)
  selection <- data[[lineage_assignment_col]] == lineage_id &
    !is.na(data[[pseudotime_col]]) & !is.na(data[[score_col]])
  selected <- data[!is.na(selection) & selection, , drop = FALSE]
  if (nrow(selected) < 4L) stop("Too few valid cells for a pseudotime GAM.")
  s <- mgcv::s
  formula <- stats::as.formula(paste0("`", score_col, "` ~ s(`", pseudotime_col, "`)"), env = environment())
  fit <- mgcv::gam(formula, data = selected)
  grid <- seq(pseudotime_range[1L], pseudotime_range[2L], length.out = n_grid)
  newdata <- stats::setNames(data.frame(grid), pseudotime_col)
  predicted <- stats::predict(fit, newdata = newdata, se.fit = TRUE)$fit
  derivative <- diff(predicted) / diff(grid)
  if (method == "inflection") return(grid[which.max(derivative)])
  if (method == "drop") return(grid[which.min(derivative)])
  peaks <- which(diff(sign(derivative)) == if (method == "maximum") -2L else 2L)
  if (!length(peaks)) {
    warning("No local ", method, " found — using global ", method, ".")
    return(grid[if (method == "maximum") which.max(predicted) else which.min(predicted)])
  }
  grid[peaks[min(which_n, length(peaks))]]
}

#' Plot a pseudotime score with an explicit threshold
#' @param data Cell metadata.
#' @param pseudotime_col,score_col Pseudotime and score columns.
#' @param threshold Numeric threshold computed separately.
#' @param plot_type Scatter plot or boxplot by timepoint.
#' @param lineage_assignment_col,timepoint_col Metadata columns.
#' @param exclude_timepoints Optional timepoints omitted from the boxplot.
#' @param pseudotime_range X-axis range for scatter plots.
#' @param pt_color,fit_color,vline_color Plot colors.
#' @param pt_alpha,pt_size Point opacity and size.
#' @param xlab,ylab Axis labels.
#' @return ggplot object with a `threshold` attribute.
#' @export
plot_pseudotime_score <- function(data, pseudotime_col, score_col, threshold,
                                  plot_type = c("scatter", "boxplot"),
                                  lineage_assignment_col = "lineage_assignment",
                                  timepoint_col = "timepoint", exclude_timepoints = NULL,
                                  pseudotime_range = c(0, 0.1),
                                  pt_color = "#BDBDBD", fit_color = "#2166AC",
                                  vline_color = "#D6604D", pt_alpha = 0.3,
                                  pt_size = 0.8, xlab = NULL, ylab = NULL) {
  .require_package("ggplot2")
  plot_type <- match.arg(plot_type)
  .check_columns(data, c(pseudotime_col, score_col, timepoint_col, lineage_assignment_col))
  if (plot_type == "boxplot") {
    .require_package("ggprism")
    plotted <- data
    if (!is.null(exclude_timepoints)) plotted <- plotted[!plotted[[timepoint_col]] %in% exclude_timepoints, , drop = FALSE]
    by_group <- split(plotted[[pseudotime_col]], plotted[[timepoint_col]])
    y_max <- max(vapply(by_group, function(values) {
      stats::quantile(values, 0.75, na.rm = TRUE) + 1.5 * stats::IQR(values, na.rm = TRUE)
    }, numeric(1)), na.rm = TRUE)
    clean_label <- function(value) trimws(gsub("_", " ",
                                               gsub("Score", " Score", sub("_UCell$", "", value))))
    p <- ggplot2::ggplot(plotted, ggplot2::aes(x = .data[[timepoint_col]], y = .data[[pseudotime_col]])) +
      ggplot2::geom_boxplot(fill = "grey92", color = "black", linewidth = 0.7,
                            width = 0.6, outlier.shape = NA, staplewidth = 0.4) +
      ggplot2::geom_hline(yintercept = threshold, lty = 2, color = vline_color,
                          linewidth = 0.6) +
      ggplot2::scale_y_continuous(limits = c(0, y_max * 1.08), expand = c(0.02, 0)) +
      ggplot2::labs(x = xlab %||% "Days after differentiation",
                    y = ylab %||% clean_label(pseudotime_col)) +
      ggprism::theme_prism(base_size = 12)
  } else {
    .require_package("ggrastr")
    .require_package("ggprism")
    .require_package("mgcv")
    trajectory <- .lineage_number(pseudotime_col)
    plotted <- data[data[[lineage_assignment_col]] == trajectory &
                      !is.na(data[[pseudotime_col]]) & !is.na(data[[score_col]]), , drop = FALSE]
    clean_label <- function(value) trimws(gsub("_", " ",
                                               gsub("Score", " Score", sub("_UCell$", "", value))))

    s <- mgcv::s
    smooth_formula <- stats::as.formula("y ~ s(x)", env = environment())

    p <- ggplot2::ggplot(plotted, ggplot2::aes(x = .data[[pseudotime_col]], y = .data[[score_col]])) +
      ggrastr::geom_point_rast(color = pt_color, alpha = pt_alpha, size = pt_size) +
      ggplot2::geom_smooth(method = mgcv::gam, formula = smooth_formula,
                           color = fit_color, fill = fit_color, alpha = 0.15) +
      ggplot2::geom_vline(xintercept = threshold, lty = 2, color = vline_color,
                          linewidth = 0.6) +
      ggplot2::annotate("text", x = threshold + diff(pseudotime_range) * 0.03,
                        y = max(plotted[[score_col]], na.rm = TRUE) * 0.95,
                        label = paste0("pt = ", round(threshold, 3)), color = vline_color,
                        size = 3.2, hjust = 0) +
      ggplot2::scale_x_continuous(limits = pseudotime_range, expand = c(0.01, 0)) +
      ggplot2::scale_y_continuous(expand = c(0.02, 0)) +
      ggplot2::labs(x = xlab %||% clean_label(pseudotime_col),
                    y = ylab %||% clean_label(score_col)) +
      ggprism::theme_prism(base_size = 12)
  }
  attr(p, "threshold") <- threshold
  p
}


.lineage_number <- function(name) {
  match <- regmatches(name, regexpr("[0-9]+", name))
  if (!length(match) || !nzchar(match)) stop("pseudotime_col must contain a lineage number.")
  as.numeric(match)
}
