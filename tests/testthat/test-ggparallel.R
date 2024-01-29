test_that("ggparallel works", {
  load(system.file("mtcars.rda", package="ggparallel")) # loads object test_mtcars_plot
  built_saved <- ggplot2::ggplot_build(test_mtcars_plot)
  test <- ggparallel(list("gear", "cyl"), data=mtcars)
  build_now <- ggplot2::ggplot_build(test)

  expect_identical(built_saved$data, build_now$data)
  expect_identical(test$layers, test_mtcars_plot$layers)
})
