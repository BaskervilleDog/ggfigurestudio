library(ggfigurestudio)
library(ggplot2)

style <- ggfigurestudio:::styled_plot
p <- example_figure()
p <- style(p, 2L, "1", list(colour = "#FF00FF", shape = 24, size = 5))
p <- style(p, 1L, "2", list(colour = "#000080", linetype = "dashed", linewidth = 2))
project <- figure_project(p)
guide <- ggplot_build(p)$plot$guides$params[[1]]
point <- guide$decor$geom_point$data
line <- guide$decor$geom_line$data
quebec <- match("Quebec", guide$key$.label)
mississippi <- match("Mississippi", guide$key$.label)
stopifnot(point$colour[quebec] == "#FF00FF", point$shape[quebec] == 24,
          point$size[quebec] == 5, point$colour[mississippi] == "#C65D21",
          line$colour[mississippi] == "#000080", line$linetype[mississippi] == "dashed",
          line$linewidth[mississippi] == 2, line$colour[quebec] == "#007C78")

# Check the actual drawn key glyphs as well as the trained legend data.
layout <- render_figure(project)
box <- layout$grobs[[match("guide-box-bottom", layout$layout$name)]]
guide_table <- box$grobs[[match("guides", box$layout$name)]]
keys <- guide_table$grobs[startsWith(guide_table$layout$name, "key-")]
key_colours <- lapply(keys, function(key) {
  unlist(lapply(key$children, function(child) substr(child$gp$col, 1, 7)), use.names = FALSE)
})
stopifnot(any(vapply(key_colours, function(x) "#FF00FF" %in% x, logical(1))),
          any(vapply(key_colours, function(x) "#000080" %in% x, logical(1))))
svg <- tempfile(fileext = ".svg")
export_figure(project, svg)
stopifnot(file.size(svg) > 0)

reset <- style(p, 2L, "1", NULL)
reset_data <- ggplot_build(reset)$plot$guides$params[[1]]$decor$geom_point$data
stopifnot(reset_data$colour[quebec] == "#007C78", reset_data$shape[quebec] == 21)
