#' 3D Diffusion Map Visualization with Flexible Coloring
#'
#' This function generates an interactive 3D scatter plot of diffusion map coordinates using plotly.
#' Points can be colored by any continuous or discrete variable, supporting pseudotime or groupings.
#' Trajectory curves and lineage labels can be added for visualizing differentiation paths.
#'
#' @param diffmap_df A data.frame containing at least the columns \code{DC1}, \code{DC2}, \code{DC3} (diffusion components)
#'   and the variable to color by (e.g. pseudotime, timepoint).
#' @param curve_df A data.frame containing diffusion components (DC1, DC2, DC3) and a \code{curve_id} column for trajectory curves.
#' @param label_df (Optional) A data.frame with columns \code{DC1}, \code{DC2}, \code{DC3}, and \code{lineage_name} for labeling trajectories.
#' @param color_by Character. The column name in \code{diffmap_df} to use for coloring points.
#'
#' @details
#' If \code{color_by} refers to a numeric column with more than ten unique values, a continuous
#' \code{viridis} color scale is used. If \code{color_by} refers to a factor, character, or integer column
#' (with few unique values), a discrete palette from \code{scCustomize::DiscretePalette_scCustomize()} is used.
#'
#' Trajectory curves from \code{curve_df} are plotted as black lines. Optional lineage labels from \code{label_df}
#' are displayed in red.
#'
#' @return An interactive plotly 3D scatterplot object.
#' @importFrom plotly plot_ly add_trace add_text layout
#' @importFrom viridis viridis
#' @importFrom scCustomize DiscretePalette_scCustomize
#' @export
#'
#' @examples
#' \dontrun{
#' # After adding pseudotimes for all lineages to diffmap_df:
#' plot_3d_diffusion(diffmap_df, curve_df, label_df, color_by = "Lineage1")
#' plot_3d_diffusion(diffmap_df, curve_df, label_df, color_by = "timepoint")
#' }

plot_3d_diffusion <- function(
    diffmap_df,
    curve_df,
    label_df = NULL,
    color_by = "Lineage1",
    point_size = 2,
    legend_size = 16
) {
  if (!requireNamespace("scCustomize", quietly = TRUE)) stop("Please install the scCustomize package.")
  if (!requireNamespace("viridis", quietly = TRUE)) stop("Please install the viridis package.")
  if (!requireNamespace("plotly", quietly = TRUE)) stop("Please install the plotly package.")

  color_vec <- diffmap_df[[color_by]]
  is_continuous <- is.numeric(color_vec) && length(unique(color_vec)) > 20

  # Base plot object
  plt <- plotly::plot_ly()

  if (is_continuous) {
    # --- Continuous color (viridis) ---
    n_col <- 100
    viridis_col <- viridis::viridis(n_col, option = "C")
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
    # --- Discrete color ---
    group_levels <- as.factor(color_vec)
    n_groups <- length(levels(group_levels))
    discrete_colors <- scCustomize::DiscretePalette_scCustomize(max(n_groups, 3), palette = "polychrome")
    color_map <- setNames(discrete_colors[seq_len(n_groups)], levels(group_levels))

    for (lvl in levels(group_levels)) {
      sub_df <- diffmap_df[group_levels == lvl, ]

      # Main trace (small points, no legend)
      plt <- plt %>% plotly::add_trace(
        data = sub_df,
        x = ~DC1, y = ~DC2, z = ~DC3,
        type = "scatter3d", mode = "markers",
        marker = list(size = point_size, opacity = 0.6, color = color_map[[lvl]]),
        name = lvl,
        showlegend = FALSE,
        inherit = FALSE
      )

      # Dummy legend point (larger, single point)
      plt <- plt %>% plotly::add_trace(
        data = sub_df[1, ],
        x = ~DC1, y = ~DC2, z = ~DC3,
        type = "scatter3d", mode = "markers",
        marker = list(size = legend_size, color = color_map[[lvl]], opacity = 1),
        name = lvl,
        showlegend = TRUE,
        inherit = FALSE
      )
    }
  }

  # --- Add curve lines ---
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

  # --- Add lineage labels ---
  if (!is.null(label_df) && all(c("DC1", "DC2", "DC3", "lineage_name") %in% names(label_df))) {
    plt <- plt %>% plotly::add_text(
      data = label_df,
      x = ~DC1, y = ~DC2, z = ~DC3,
      text = ~lineage_name,
      textposition = "top middle",
      textfont = list(color = "black", size = 20, family = "Arial"),
      showlegend = FALSE,
      inherit = FALSE
    )
  }

  # --- Layout settings ---
  plt <- plt %>% plotly::layout(
    scene = list(
      xaxis = list(title = "DC1", font = list(size = legend_size), tickfont = list(size = legend_size)),
      yaxis = list(title = "DC2", font = list(size = legend_size), tickfont = list(size = legend_size)),
      zaxis = list(title = "DC3", font = list(size = legend_size), tickfont = list(size = legend_size))
    ),
    legend = list(
      font = list(size = legend_size),
      orientation = "v", x = 1, y = 0.5
      )
  )

  return(plt)
}

