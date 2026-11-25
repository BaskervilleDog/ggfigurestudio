library(ggfigurestudio)
library(ggplot2)

source <- ggplot(iris, aes(Sepal.Length)) + geom_histogram(binwidth = 0.5)
with_lines <- ggfigurestudio:::add_reference_line(source, "x", "5", "navy", 0.8, "dashed")
with_lines <- ggfigurestudio:::add_reference_line(with_lines, "y", "15", "red", 0.8, "solid")
stopifnot(length(source$layers) == 1L, length(with_lines$layers) == 3L,
          identical(ggplot_build(source)$data[[1]], ggplot_build(with_lines)$data[[1]]))
preview <- ggfigurestudio:::tick_preview(figure_project(with_lines))
stopifnot(preview$marks[["mark/2/-1"]]$reference$value == "5",
          preview$marks[["mark/3/-1"]]$reference$value == "15")
moved <- ggfigurestudio:::move_reference_line(with_lines, 2L, preview$marks[["mark/2/-1"]]$reference, "6")
stopifnot(identical(moved$layers[[2]]$data$xintercept, 6),
          identical(with_lines$layers[[2]]$data$xintercept, 5))

project <- figure_project(with_lines)
keys <- vapply(2:3, function(i) ggfigurestudio:::reference_move_key(with_lines$layers[[i]], i),
               character(1))
stopifnot(all(keys %in% preview$move_keys))
svg <- tempfile(fileext = ".svg")
line_positions <- function(project) {
  export_figure(project, svg)
  lines <- grep("stroke: #(000080|FF0000);", readLines(svg, warn = FALSE), value = TRUE)
  stopifnot(length(lines) == 2L)
  c(as.numeric(sub(".*x1='([^']+)'.*", "\\1", lines[1L])),
    as.numeric(sub(".*y1='([^']+)'.*", "\\1", lines[2L])))
}
positions <- line_positions(project)
project$element_edits <- stats::setNames(list(c(3, 0), c(0, 4)), keys)
stopifnot(!length(ggfigurestudio:::tick_preview(project)$issues),
          all(abs(line_positions(project) - positions - c(3, -4) * 72 / 25.4) < 0.02))

faceted <- with_lines + facet_wrap(vars(Species))
faceted$data <- iris[1:100, ]
project <- figure_project(faceted)
preview <- ggfigurestudio:::tick_preview(project)
vertical <- preview$move_keys[startsWith(preview$move_keys, paste0(keys[1L], "/facet/"))]
horizontal <- preview$move_keys[startsWith(preview$move_keys, paste0(keys[2L], "/facet/"))]
stopifnot(length(vertical) == 2L, length(horizontal) == 2L,
          sum(startsWith(names(preview$marks), "mark/2/-1/facet/")) == 2L,
          sum(startsWith(names(preview$marks), "mark/3/-1/facet/")) == 2L)
facet_line_positions <- function(project) {
  export_figure(project, svg)
  svg_lines <- readLines(svg, warn = FALSE)
  vertical_lines <- grep("stroke: #000080;", svg_lines, value = TRUE)
  horizontal_lines <- grep("stroke: #FF0000;", svg_lines, value = TRUE)
  stopifnot(length(vertical_lines) == 2L, length(horizontal_lines) == 2L)
  c(as.numeric(sub(".*x1='([^']+)'.*", "\\1", vertical_lines)),
    as.numeric(sub(".*y1='([^']+)'.*", "\\1", horizontal_lines)))
}
positions <- facet_line_positions(project)
project$element_edits <- stats::setNames(list(c(1, 0), c(3, 0), c(0, 4)),
                                         c(keys[1L], vertical[1L], horizontal[2L]))
stopifnot(!length(ggfigurestudio:::tick_preview(project)$issues),
          all(abs(facet_line_positions(project) - positions - c(4, 1, 0, -4) * 72 / 25.4) < 0.02))

line <- example_figure()
tiles <- ggfigurestudio:::replace_layer_geom(line, 1L, "tile", tile_size = list(width = 50, height = 2),
                                            map_fill = TRUE)
stopifnot(inherits(line$layers[[1]]$geom, "GeomLine"), inherits(tiles$layers[[1]]$geom, "GeomTile"),
          identical(line$layers[[2]], tiles$layers[[2]]),
          any(startsWith(names(ggfigurestudio:::tick_preview(figure_project(tiles))$marks), "tile/1/")))
