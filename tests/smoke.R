library(ggfigurestudio)
library(ggplot2)
p <- ggplot(iris, aes(Sepal.Length, Petal.Length, colour = Species)) +
  geom_point() + labs(title = "Original")
fig <- figure_project(p)
fig$title <- "Edited"
fig$palette <- "Viridis"
base <- render_figure(fig)
fig$dx <- 2
edited <- render_figure(fig)
title_x <- function(layout) {
  title <- layout$grobs[[which(layout$layout$name == "title")]]
  title$children[[1]]$x
}
delta <- grid::convertWidth(title_x(edited) - title_x(base), "mm", valueOnly = TRUE)
stopifnot(isTRUE(all.equal(delta, 2)), identical(fig$plot$labels$title, "Original"))
saved <- tempfile()
saveRDS(fig, saved)
formats <- c("png", "pdf", "tiff")
if (requireNamespace("svglite", quietly = TRUE)) formats <- c(formats, "svg")
for (ext in formats) {
  output <- tempfile(fileext = paste0(".", ext))
  export_figure(readRDS(saved), output)
  stopifnot(file.size(output) > 0)
}

# The resized plot and its hit targets both fit inside the original image.
p <- p + theme(plot.background = element_rect(fill = "#F8EEE0", colour = NA))
fig <- figure_project(p, width = 200, height = 150)
app <- ggfigurestudio:::figure_editor(fig)
shiny::testServer(app, {
  panel_button <- function(view) {
    html <- strsplit(as.character(view$canvas), "\n", fixed = TRUE)[[1L]]
    grep('data-key="panel-ticks/', html, value = TRUE, fixed = TRUE)
  }
  button_width <- function(view) {
    as.numeric(sub(".*;width:([0-9.]+)%.*", "\\1", panel_button(view)))
  }
  original_width <- button_width(preview())
  session$setInputs(plot_width_percent = 75, plot_height_percent = 60, apply_plot_size = 1)
  stopifnot(identical(current()$plot_size, c(width = 75, height = 60)),
            current()$width == 200, current()$height == 150,
            button_width(preview()) < original_width)
  restored <- unserialize(serialize(current(), NULL))
  area <- render_figure(restored)$children[[2L]]$vp
  stopifnot(as.numeric(area$width) == 0.75, as.numeric(area$height) == 0.6)
  svg_dimensions <- function(project) {
    file <- tempfile(fileext = ".svg")
    export_figure(project, file)
    readLines(file, n = 5L, warn = FALSE)
  }
  stopifnot(identical(svg_dimensions(fig), svg_dimensions(restored)))
  session$setInputs(plot_width_percent = 0, apply_plot_size = 2)
  stopifnot(current()$plot_size[["width"]] == 75)
  session$setInputs(plot_width_percent = 100, plot_height_percent = 100, apply_plot_size = 3)
  stopifnot(inherits(render_figure(current()), "gtable"))
})

# Return can arrive in the same Shiny flush as a change to the figure.
shiny::testServer(app, {
  key <- grep("^element/title/", preview()$keys, value = TRUE)[1L]
  session$setInputs(inspect_element = key, nudge = 0.5)
  movement <- jsonlite::toJSON(list(list(key = key, dx = 8, dy = 2)), auto_unbox = TRUE)
  session$setInputs(element_size = 17, element_angle = 0, element_colour = "navy",
                    element_label = "Returned title", apply_element = 1L,
                    done = 1L, movements = movement)
  returned <- getFromNamespace(".globals", "shiny")$retval$value
  stopifnot(inherits(returned, "figure_project"), returned$plot$labels$title == "Returned title",
            identical(returned$element_edits[[key]], c(8, 2)))
})
