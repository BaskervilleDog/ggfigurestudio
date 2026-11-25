library(ggfigurestudio)
library(ggplot2)

make_plot <- function(data) {
  ggplot(data, aes(Sepal.Length, Petal.Length)) + geom_point() +
    facet_wrap(~Species) + scale_x_continuous(breaks = c(5, 6, 7))
}
key <- '{"facets":{"Species":"setosa"},"side":"b","value":6}'
f <- figure_project(make_plot(iris))
preview <- ggfigurestudio:::tick_preview(f)
stopifnot(key %in% preview$keys, !length(preview$issues))
# An img data URL requires a standalone SVG document with its own namespace.
image_src <- preview$canvas$children[[1]]$attribs$src
svg <- rawToChar(jsonlite::base64_dec(sub("^data:image/svg\\+xml;base64,", "", image_src)))
stopifnot(grepl("xmlns=['\"]http://www.w3.org/2000/svg['\"]", svg))
if (requireNamespace("xml2", quietly = TRUE)) xml2::read_xml(svg)
f$tick_edits[[key]] <- c(2, -1)
get_x <- function(fig, axis) {
  layout <- render_figure(fig)
  inner <- layout$grobs[[match(axis, layout$layout$name)]]$children$axis
  node <- Filter(function(x) inherits(x, "titleGrob"), inner$grobs)[[1]]$children[[1]]
  node$x[match("6", node$label)]
}
check_offset <- function(fig, axis, expected) {
  original <- fig
  original$tick_edits <- NULL
  delta <- grid::convertWidth(get_x(fig, axis) - get_x(original, axis), "mm", valueOnly = TRUE)
  stopifnot(isTRUE(all.equal(delta, expected)))
}
check_offset(f, "axis-b-1-1", 2)
check_offset(f, "axis-b-2-1", 0)
data <- iris
data$Species <- factor(data$Species, levels = rev(levels(data$Species)))
f$plot <- make_plot(data)
check_offset(f, "axis-b-3-1", 2)
check_offset(f, "axis-b-1-1", 0)
saved <- tempfile()
saveRDS(f, saved)
check_offset(readRDS(saved), "axis-b-3-1", 2)
f$plot <- make_plot(subset(data, Species != "setosa"))
preview <- ggfigurestudio:::tick_preview(f)
stopifnot(any(grepl(key, preview$issues, fixed = TRUE)), !key %in% preview$keys)
