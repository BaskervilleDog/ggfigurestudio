library(ggfigurestudio)
library(ggplot2)
p <- ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
  geom_point() + geom_point(data = head(iris, 3), size = 2)
edit <- ggfigurestudio:::edit_plot_aes
catalog <- list_aesthetics(p, 1)
stopifnot(all(c("shape", "fill", "stroke", "alpha") %in% catalog$aesthetic))
reference <- ggplot_build(p)$data
fixed <- edit(p, 1L, "colour", "fixed", '"navy"')
stopifnot(all(ggplot_build(fixed)$data[[1]]$colour == "navy"),
          identical(ggplot_build(p)$data[[1]]$colour, reference[[1]]$colour),
          identical(ggplot_build(fixed)$data[[2]]$colour, reference[[2]]$colour))
mapped <- edit(fixed, 1L, "colour", "column", "Species")
stopifnot(identical(ggplot_build(mapped)$data[[1]]$colour, reference[[1]]$colour))
removed <- edit(mapped, 1L, "colour", "remove", "")
stopifnot(length(unique(ggplot_build(removed)$data[[1]]$colour)) == 1L)
reset <- edit(removed, 1L, "colour", "reset", "")
stopifnot(identical(ggplot_build(reset)$data[[1]]$colour, reference[[1]]$colour))
global <- edit(p, 0L, "x", "expression", "Sepal.Length + 1")
stopifnot(identical(ggplot_build(global)$data[[2]]$x, reference[[2]]$x + 1))
invalid <- edit(p, 0L, "x", "remove", "")
stopifnot(inherits(try(render_figure(figure_project(invalid)), silent = TRUE), "try-error"))
stopifnot("weight" %in% list_aesthetics(ggplot(iris, aes(Species)) + geom_bar(), 1)$aesthetic)
GeomProbe <- ggproto("GeomProbe", GeomPoint, default_aes = aes(probe_value = 1))
custom <- ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  layer(geom = GeomProbe, stat = "identity", position = "identity")
stopifnot("probe_value" %in% list_aesthetics(custom, 1)$aesthetic)
