test_that("ggparallel works", {
  load(system.file("mtcars.rda", package="ggparallel")) # loads object test_mtcars_plot
  built_saved <- ggplot2::ggplot_build(test_mtcars_plot)
  test <- ggparallel(list("gear", "cyl"), data=mtcars)
  build_now <- ggplot2::ggplot_build(test)

  # data is the same
  expect_identical(built_saved$data, build_now$data)

  # we have three layers
  expect_equal(length(test$layers), 3)
  classes <-
    c(class(test$layers[[1]]$geom), class(test$layers[[2]]$geom), class(test$layers[[3]]$geom))
  # we have the right layer classes
  expect_contains(classes, c("GeomRibbon", "GeomBar", "GeomText"))

})
