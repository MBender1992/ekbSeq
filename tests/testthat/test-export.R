test_that("Excel export creates workbook from named frames", {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  expect_identical(export_excel(list(one = data.frame(a = 1:2),
                                     two = data.frame(b = 3)), path), path)
  expect_true(file.exists(path))
  expect_setequal(openxlsx::getSheetNames(path), c("one", "two"))
})

test_that("dual plot exporter writes PNG and SVG", {
  skip_if_not_installed("svglite")
  path <- file.path(tempdir(), "ekbseq-export", "figure")
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_line()
  export_plot_dual(path, plot, width = 2, height = 2, dpi = 72,
                   font = "DejaVu Sans", rasterize = FALSE)
  expect_true(file.exists(paste0(path, ".png")))
  expect_true(file.exists(paste0(path, ".svg")))
})
