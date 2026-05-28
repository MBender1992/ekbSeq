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
