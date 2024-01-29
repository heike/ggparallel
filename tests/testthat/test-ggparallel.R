save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code

  path
}

test_that("ggparallel works", {
  load(system.file("mtcars.rda", package="ggparallel")) # loads object test_mtcars_plot
  built_saved <- ggplot2::ggplot_build(test_mtcars_plot)
  test <- ggparallel(list("gear", "cyl"), data=mtcars)
  build_now <- ggplot2::ggplot_build(test)

  # data is the same
  expect_equal(built_saved$data, build_now$data, tolerance=1e-4)

  # we have three layers
  expect_equal(length(test$layers), 3)
  classes <-
    c(class(test$layers[[1]]$geom), class(test$layers[[2]]$geom), class(test$layers[[3]]$geom))
  # we have the right layer classes
  expect_contains(classes, c("GeomRibbon", "GeomBar", "GeomText"))

  # throw error
  expect_message(expect_error(ggparallel(list("gear"), data=mtcars)))
})

test_that("ggparallel hammocks work", {
  load(system.file("titanic_hammock.rda", package="ggparallel")) # loads object titanic_hammock

  titanic <- as.data.frame(Titanic)
  titanic_plot <- ggparallel(names(titanic)[c(1,4,2,3)], titanic, weight=1, asp=0.5,
                             method="hammock", ratio=0.2, order=c(0,0))
  build_now <- ggplot2::ggplot_build(titanic_plot)
  # titanic_hammock <- build_now$data
  # save(titanic_hammock, file="inst/titanic_hammock.rda")

  expect_equal(titanic_hammock, build_now$data, tolerance=1e-4)
})

test_that("ggparallel adjusted angles work", {
  load(system.file("titanic_adjangle.rda", package="ggparallel")) # loads object titanic_adjangle

  titanic <- as.data.frame(Titanic)
  titanic_plot <- ggparallel(list("gear", "cyl"), data=mtcars, method="adj.angle", ratio=2.5)
  build_now <- ggplot2::ggplot_build(titanic_plot)
  # titanic_adjangle <- build_now$data
  # save(titanic_adjangle, file="inst/titanic_adjangle.rda")

  expect_equal(titanic_adjangle, build_now$data, tolerance=1e-4)
})
