library(ggfigurestudio)
library(ggplot2)

plot <- ggplot(iris, aes(Sepal.Length)) + geom_histogram(binwidth = 0.5)
box <- list(type = "box", x = "5", y = "12", width = 24, height = 14,
            fill = "#FFF2CC", border = "#725433", text = "First annotation",
            text_colour = "#252525", text_size = 10)
circle <- utils::modifyList(box, list(type = "circle", x = "7", y = "4",
                                      width = 16, height = 16, text = "Second annotation"))
plot <- ggfigurestudio:::shape_plot(plot, box)
plot <- ggfigurestudio:::shape_plot(plot, circle)
project <- figure_project(plot)
box_key <- ggfigurestudio:::shape_layer_key(plot$layers[[2L]], 2L)
circle_key <- ggfigurestudio:::shape_layer_key(plot$layers[[3L]], 3L)
preview <- ggfigurestudio:::tick_preview(project)
stopifnot(identical(preview$marks[[box_key]]$annotation, box),
          identical(preview$marks[[circle_key]]$annotation, circle))

moved <- utils::modifyList(box, list(x = "5.5", text = "Updated annotation",
                                    fill_alpha = 0.2, border_alpha = 0.6, border_width = 1.5))
project$plot <- ggfigurestudio:::shape_plot(plot, moved, index = 2L)
preview <- ggfigurestudio:::tick_preview(project)
stopifnot(project$plot$layers[[2L]]$data$x == 5.5,
          identical(preview$marks[[box_key]]$annotation, moved),
          identical(plot$layers[[2L]]$geom_params$annotation, box),
          length(project$plot$layers) == 3L)
file <- tempfile(fileext = ".svg")
export_figure(project, file)
svg <- readLines(file, warn = FALSE)
rect <- svg[grepl("fill: #FFF2CC; fill-opacity:", svg, fixed = TRUE)]
stopifnot(any(grepl("Updated annotation", svg, fixed = TRUE)),
          any(grepl("Second annotation", svg, fixed = TRUE)),
          !any(grepl("First annotation", svg, fixed = TRUE)),
          length(rect) == 1L,
          grepl("stroke: #725433;", rect, fixed = TRUE),
          grepl("stroke-width: 3.20;", rect, fixed = TRUE),
          grepl("stroke-opacity: 0.60;", rect, fixed = TRUE),
          grepl("fill-opacity: 0.20", rect, fixed = TRUE))

move_key <- paste0("element/", box_key)
stopifnot(move_key %in% preview$move_keys)
project$element_edits <- stats::setNames(list(c(5, 3)), move_key)
preview <- ggfigurestudio:::tick_preview(project)
stopifnot(!length(preview$issues), move_key %in% preview$move_keys)
export_figure(project, file)
shifted_svg <- readLines(file, warn = FALSE)
svg_xy <- function(line) {
  as.numeric(sub(".* x='([^']+)' y='([^']+)'.*", "\\1", line)) +
    1i * as.numeric(sub(".* x='([^']+)' y='([^']+)'.*", "\\2", line))
}
shift <- (5 - 3i) * 72 / 25.4
for (part in c("fill: #FFF2CC; fill-opacity:", "Updated annotation")) {
  old <- svg_xy(svg[grepl(part, svg, fixed = TRUE)])
  new <- svg_xy(shifted_svg[grepl(part, shifted_svg, fixed = TRUE)])
  stopifnot(length(old) == 1L, length(new) == 1L, Mod(new - old - shift) < 0.03)
}

ellipse <- utils::modifyList(circle, list(width = 36, height = 12, fill = "#ABE4DD", angle = 90))
project$plot <- ggfigurestudio:::shape_plot(project$plot, ellipse, index = 3L)
preview <- ggfigurestudio:::tick_preview(project)
stopifnot(identical(preview$marks[[circle_key]]$annotation, ellipse),
          paste0("element/", circle_key) %in% preview$move_keys)
export_figure(project, file)
svg <- readLines(file, warn = FALSE)
polygon <- svg[grepl("<polygon.*fill: #ABE4DD", svg)]
stopifnot(length(polygon) == 1L)
points <- sub(".*points='([^']+)'.*", "\\1", polygon)
xy <- matrix(scan(text = gsub(",", " ", points, fixed = TRUE), quiet = TRUE), ncol = 2, byrow = TRUE)
size <- apply(xy, 2, function(v) diff(range(v))) * 25.4 / 72
stopifnot(all(abs(size - c(ellipse$height, ellipse$width)) < 0.02),
          any(grepl("rotate\\(-90(?:[.]0+)?\\).*Second annotation", svg, perl = TRUE)))

faceted <- ggplot(iris[1:100, ], aes(Sepal.Length)) +
  geom_histogram(binwidth = 0.5) + facet_wrap(vars(Species))
faceted <- ggfigurestudio:::shape_plot(faceted, box)
project <- figure_project(faceted)
preview <- ggfigurestudio:::tick_preview(project)
base_key <- ggfigurestudio:::shape_layer_key(faceted$layers[[2L]], 2L)
facet_keys <- preview$move_keys[startsWith(preview$move_keys, paste0("element/", base_key, "/facet/"))]
stopifnot(length(facet_keys) == 2L)
ggfigurestudio:::check_shape_preview(project, 2L)

facet_positions <- function(project) {
  export_figure(project, file)
  rects <- grep("<rect.*fill: #FFF2CC", readLines(file, warn = FALSE), value = TRUE)
  stopifnot(length(rects) == 2L)
  svg_xy(rects)
}
before <- facet_positions(project)
project$element_edits <- stats::setNames(list(c(2, 0), c(5, 3)), c(paste0("element/", base_key), facet_keys[1L]))
after <- facet_positions(project)
shifts <- (after - before) * 25.4 / 72
stopifnot(all(Mod(shifts - c(7 - 3i, 2)) < 0.02),
          !length(ggfigurestudio:::tick_preview(project)$issues))
