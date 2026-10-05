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
    warning("No local ", method, " found \u2014 using global ", method, ".")
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
      ggplot2::geom_smooth(method = "gam", formula = smooth_formula,
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

#' 3D Diffusion Map Visualization with Flexible Coloring
#'
#' This function generates an interactive 3D scatter plot of diffusion map coordinates using plotly.
#' Points can be colored by any continuous or discrete variable (e.g. pseudotime, timepoint, clusters, etc.).
#' Trajectory curves and lineage labels can be added for visualizing differentiation paths, but are optional.
#'
#' @param diffmap_df A data.frame containing at least the columns DC1, DC2, DC3 (diffusion components)
#'   and the variable to color by (e.g. pseudotime, timepoint, cluster).
#' @param curve_df (Optional) A data.frame containing diffusion components (DC1, DC2, DC3) and a curve_id column for trajectory curves.
#' @param label_df (Optional) A data.frame with columns DC1, DC2, DC3, and lineage_name for labeling trajectories.
#' @param color_by Character. The column name in diffmap_df to use for coloring points.
#' @param point_size Size of points.
#' @param legend_size Size of legend.
#' @param camera_eye x, y and z coordinates giving the angle of the graphical 3D representation
#' @param minimal Logical. If TRUE, hide axes and grid. Default is FALSE.
#'
#' @details
#' If color_by refers to a numeric column with more than 20 unique values, a continuous
#' viridis color scale is used. Otherwise, a discrete palette from scCustomize::DiscretePalette_scCustomize() is used.
#'
#' Trajectory curves from curve_df are plotted as black lines if provided.
#' Optional lineage labels from label_df are displayed in black if provided.
#'
#' @return An interactive plotly 3D scatterplot object.
#' @export

plot_3d_diffusion <- function(
    diffmap_df,
    curve_df = NULL,
    label_df = NULL,
    color_by = "Lineage1",
    point_size = 2,
    legend_size = 16,
    camera_eye = list(x = -1.5, y = 0.3, z = 1),
    minimal = FALSE  # New argument: if TRUE, hide axes and grid
) {
  if (!requireNamespace("scCustomize", quietly = TRUE)) stop("Please install the scCustomize package.")
  if (!requireNamespace("viridis", quietly = TRUE)) stop("Please install the viridis package.")
  if (!requireNamespace("plotly", quietly = TRUE)) stop("Please install the plotly package.")
  curve_id <- NULL

  req_cols <- c("DC1", "DC2", "DC3")
  if (!all(req_cols %in% colnames(diffmap_df))) stop("diffmap_df must contain columns: DC1, DC2, DC3")
  if (!(color_by %in% colnames(diffmap_df))) stop(sprintf("Column '%s' not found in diffmap_df", color_by))
  color_vec <- diffmap_df[[color_by]]
  is_continuous <- is.numeric(color_vec) && length(unique(color_vec)) > 20

  plt <- plotly::plot_ly()

  if (is_continuous) {
    n_col <- 100
    viridis_col <- viridis::viridis(n_col, option = "D")
    colorscale <- lapply(seq_along(viridis_col), function(i) list((i - 1) / (n_col - 1), viridis_col[i]))

    plt <- plt %>% plotly::add_trace(
      data = diffmap_df,
      x = ~DC1, y = ~DC2, z = ~DC3,
      type = "scatter3d", mode = "markers",
      marker = list(
        size = point_size,
        opacity = 0.6,
        color = color_vec,
        colorscale = colorscale,
        colorbar = list(title = color_by)
      ),
      name = color_by,
      showlegend = TRUE,
      inherit = FALSE
    )
  } else {
    group_levels <- as.factor(color_vec)
    n_groups <- length(levels(group_levels))
    discrete_colors <- scCustomize::DiscretePalette_scCustomize(max(n_groups, 3), palette = "polychrome")
    color_map <- setNames(discrete_colors[seq_len(n_groups)], levels(group_levels))

    for (lvl in levels(group_levels)) {
      sub_df <- diffmap_df[group_levels == lvl, , drop = FALSE]
      plt <- plt %>% plotly::add_trace(
        data = sub_df,
        x = ~DC1, y = ~DC2, z = ~DC3,
        type = "scatter3d", mode = "markers",
        marker = list(size = point_size, opacity = 0.6, color = color_map[[lvl]]),
        name = lvl,
        showlegend = FALSE,
        inherit = FALSE
      )
      plt <- plt %>% plotly::add_trace(
        data = sub_df[1, , drop = FALSE],
        x = ~DC1, y = ~DC2, z = ~DC3,
        type = "scatter3d", mode = "markers",
        marker = list(size = legend_size, color = color_map[[lvl]], opacity = 1),
        name = lvl,
        showlegend = TRUE,
        inherit = FALSE
      )
    }
  }

  # --- Add curve lines if provided ---
  if (!is.null(curve_df) && all(c("DC1", "DC2", "DC3", "curve_id") %in% colnames(curve_df))) {
    for (i in unique(curve_df$curve_id)) {
      crv <- subset(curve_df, curve_id == i)
      plt <- plt %>% plotly::add_trace(
        data = crv,
        x = ~DC1, y = ~DC2, z = ~DC3,
        type = "scatter3d", mode = "lines",
        line = list(color = "black", width = 4),
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    # --- Add lineage labels at the end of each curve ---
    label_points <- do.call(rbind, lapply(unique(curve_df$curve_id), function(cid) {
      crv <- curve_df[curve_df$curve_id == cid, , drop = FALSE]
      end_point <- crv[nrow(crv), c("DC1", "DC2", "DC3")]
      lineage_name <- if (!is.null(label_df) && "lineage_name" %in% colnames(label_df)) {
        row <- label_df[label_df$curve_id == cid, "lineage_name", drop = TRUE]
        if (length(row) == 0) as.character(cid) else row[1]
      } else {
        as.character(cid)
      }
      data.frame(DC1 = end_point$DC1, DC2 = end_point$DC2, DC3 = end_point$DC3, lineage_name = lineage_name, stringsAsFactors = FALSE)
    }))
    # Add a small nudge only if DC3 is negative (to move label below)
    label_points$DC3_nudge <- label_points$DC3 + ifelse(label_points$DC3 >= 0, 0, -0.003)

    plt <- plt %>% plotly::add_text(
      data = label_points,
      x = ~DC1, y = ~DC2, z = ~DC3_nudge,
      text = ~lineage_name,
      textposition = "top middle",
      textfont = list(
        color = "black",
        size = 22,
        family = "Arial Black"
      ),
      showlegend = FALSE,
      inherit = FALSE
    )
  }

  # --- Layout: minimal option for axes and grid ---
  if (minimal) {
    ax_null <- list(
      title = "",
      showticklabels = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      showline = FALSE,
      ticks = "",
      backgroundcolor = "rgba(0,0,0,0)"
    )
    scene_settings <- list(
      xaxis = ax_null,
      yaxis = ax_null,
      zaxis = ax_null,
      camera = list(eye = camera_eye)
    )
  } else {
    scene_settings <- list(
      xaxis = list(title = "DC1", font = list(size = legend_size), tickfont = list(size = legend_size)),
      yaxis = list(title = "DC2", font = list(size = legend_size), tickfont = list(size = legend_size)),
      zaxis = list(title = "DC3", font = list(size = legend_size), tickfont = list(size = legend_size)),
      camera = list(eye = camera_eye)
    )
  }

  plt <- plt %>% plotly::layout(
    scene = scene_settings,
    legend = list(
      font = list(size = legend_size),
      orientation = "v", x = 1, y = 0.5
    )
  )

  return(plt)
}

