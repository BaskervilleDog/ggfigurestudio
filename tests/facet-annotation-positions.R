library(ggfigurestudio)
library(ggplot2)

data <- data.frame(x = c(1, 10, 100, 2, 20, 200), y = c(1, 5, 9, 2, 6, 10),
                   panel = rep(c("A", "B"), each = 3))
plot <- ggplot(data, aes(x, y)) + geom_point() + scale_x_log10() +
  facet_wrap(vars(panel), scales = "free")
annotation <- list(type = "box", x = "10", y = "5", width = 20, height = 10, fill = "gold",
                   border = "black", text = "Note", text_colour = "black", text_size = 10)
plot <- ggfigurestudio:::shape_plot(plot, annotation)
plot <- ggfigurestudio:::add_reference_line(plot, "x", "10", "navy", 1, "solid")
plot <- ggfigurestudio:::add_reference_line(plot, "y", "5", "red", 1, "solid")
project <- figure_project(plot)
preview <- ggfigurestudio:::tick_preview(project)
keys <- names(preview$marks)
shape <- keys[startsWith(keys, "shape/")]
vertical <- keys[startsWith(keys, "mark/3/")]
horizontal <- keys[startsWith(keys, "mark/4/")]
stopifnot(length(shape) == 2L, length(vertical) == 2L, length(horizontal) == 2L)

shape_key <- preview$marks[[shape[1L]]]$move_key
project$element_edits <- stats::setNames(list(c(3, 2)), shape_key)
set <- ggfigurestudio:::set_facet_annotation_position
project <- set(project, preview$marks[[shape[1L]]], list(x = "20"))
project <- set(project, preview$marks[[vertical[2L]]], list(x = "30"))
project <- set(project, preview$marks[[horizontal[1L]]], list(y = "4"))
stopifnot(identical(project$element_edits[[shape_key]], c(0, 2)))

edited <- ggfigurestudio:::tick_preview(project)
stopifnot(edited$marks[[shape[1L]]]$annotation$x == "20",
          edited$marks[[shape[2L]]]$annotation$x == "10",
          edited$marks[[vertical[1L]]]$reference$value == "10",
          edited$marks[[vertical[2L]]]$reference$value == "30",
          edited$marks[[horizontal[1L]]]$reference$value == "4",
          edited$marks[[horizontal[2L]]]$reference$value == "5")

built <- ggfigurestudio:::apply_facet_annotation_edits(ggplot_build(project$plot))
stopifnot(isTRUE(all.equal(built$data[[2L]]$x, c(log10(20), 1))),
          isTRUE(all.equal(built$data[[3L]]$xintercept, c(1, log10(30)))),
          isTRUE(all.equal(built$data[[4L]]$yintercept, c(4, 5))))
filename <- tempfile(fileext = ".svg")
export_figure(project, filename)
svg <- readLines(filename, warn = FALSE)
stopifnot(sum(grepl("stroke: #000080;", svg, fixed = TRUE)) == 2L,
          sum(grepl("stroke: #FF0000;", svg, fixed = TRUE)) == 2L)
