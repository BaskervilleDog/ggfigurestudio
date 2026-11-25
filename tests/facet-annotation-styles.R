library(ggfigurestudio)
library(ggplot2)

plot <- ggplot(iris[1:100, ], aes(Sepal.Length)) +
  geom_histogram(binwidth = 0.5) + facet_wrap(vars(Species))
annotation <- list(type = "box", x = "5.3", y = "12", width = 20, height = 10,
                   fill = "gold", border = "black", text = "Example",
                   text_colour = "black", text_size = 10)
plot <- ggfigurestudio:::shape_plot(plot, annotation)
plot <- ggfigurestudio:::add_reference_line(plot, "x", "5", "navy", 0.8, "solid")
project <- figure_project(plot)
preview <- ggfigurestudio:::tick_preview(project)
keys <- names(preview$marks)
shape <- keys[startsWith(keys, "shape/")]
line <- keys[startsWith(keys, "mark/3/")]
set_style <- ggfigurestudio:::set_facet_annotation_style
project$plot <- set_style(project$plot, preview$marks[[shape[1L]]], list(fill = "magenta", height = 13))
project$plot <- set_style(project$plot, preview$marks[[line[2L]]], list(colour = "red", linewidth = 1.2))
edited <- ggfigurestudio:::tick_preview(project)
stopifnot(edited$marks[[shape[1L]]]$annotation$fill == "magenta",
          edited$marks[[shape[2L]]]$annotation$fill == "gold",
          edited$marks[[line[1L]]]$values$colour == "navy",
          edited$marks[[line[2L]]]$values$colour == "red")

shared <- list(colour = "green")
project$plot <- ggfigurestudio:::styled_plot(project$plot, 3L, edited$marks[[line[2L]]]$group, shared)
project$plot <- set_style(project$plot, edited$marks[[line[2L]]], shared, all_facets = TRUE)
edited <- ggfigurestudio:::tick_preview(project)
stopifnot(edited$marks[[line[1L]]]$values$colour == "green",
          edited$marks[[line[2L]]]$values$colour == "green",
          edited$marks[[line[1L]]]$values$linewidth == 0.8,
          edited$marks[[line[2L]]]$values$linewidth == 1.2)

filename <- tempfile(fileext = ".svg")
export_figure(project, filename)
svg <- readLines(filename, warn = FALSE)
stopifnot(sum(grepl("fill: #FF00FF;", svg, fixed = TRUE)) == 1L,
          sum(grepl("stroke: #00FF00;", svg, fixed = TRUE)) == 2L)
