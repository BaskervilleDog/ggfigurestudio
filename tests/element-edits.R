library(ggfigurestudio)
library(ggplot2)

if (requireNamespace("xml2", quietly = TRUE)) {
  data <- data.frame(x = 1:2, y = 1:2, label = c("First", "Second"))
  p <- ggplot(data, aes(x, y, label = label)) + geom_text() + labs(title = "Heading")
  project <- figure_project(p)
  keys <- ggfigurestudio:::tick_preview(project)$keys
  key <- grep("/First/1$", keys, value = TRUE)
  stopifnot(length(key) == 1L)
  positions <- function(project) {
    file <- tempfile(fileext = ".svg")
    export_figure(project, file)
    elements <- xml2::xml_find_all(xml2::read_xml(file), "//*[local-name()='text']")
    stats::setNames(as.numeric(xml2::xml_attr(elements, "x")), xml2::xml_text(elements))
  }
  before <- positions(project)
  project$element_edits[[key]] <- c(2, 0)
  after <- positions(project)
  stopifnot(abs(after["First"] - before["First"] - 2 * 72 / 25.4) < 0.02,
            after["Second"] == before["Second"], after["Heading"] == before["Heading"])
}

# A label anchored to a PCA observation must not add a second hit at the dot.
pca <- figure_project(bioinfo_example("pca"))
preview <- ggfigurestudio:::tick_preview(pca)
canvas <- as.character(preview$canvas)
stopifnot(identical(preview$marks[["mark/1/2"]]$title, "Point - Control / Batch B (layer 1)"),
          grepl('data-inspect="mark/1/2"', canvas, fixed = TRUE),
          grepl('data-inspect-mark="mark/2/2"', canvas, fixed = TRUE),
          !grepl('data-inspect="mark/2/2"', canvas, fixed = TRUE))

connector_lines <- function(project) {
  file <- tempfile(fileext = ".svg")
  export_figure(project, file)
  grep("stroke: #555555", readLines(file, warn = FALSE), value = TRUE, fixed = TRUE)
}

app <- ggfigurestudio:::figure_editor(figure_project(gallery_example("stacked_bar")))
shiny::testServer(app, {
  key <- grep("^element/panel/.*/[0-9]+/1$", preview()$keys, value = TRUE)[1L]
  stopifnot(!is.na(key))
  session$setInputs(connect_labels = TRUE)
  stopifnot(isTRUE(current()$connect_labels), length(connector_lines(current())) == 0L)
  move <- jsonlite::toJSON(list(list(key = key, dx = 12, dy = 3)), auto_unbox = TRUE)
  session$setInputs(movements = move)
  stopifnot(key %in% preview()$move_keys, length(connector_lines(current())) == 1L)
  session$setInputs(connect_labels = FALSE)
  stopifnot(length(connector_lines(current())) == 0L, !is.null(current()$element_edits[[key]]))
  session$setInputs(connect_labels = TRUE, movements = '[{"undo":true}]')
  stopifnot(is.null(current()$element_edits[[key]]), length(connector_lines(current())) == 0L)
})

shiny::testServer(app, {
  key <- grep("^element/panel/.*/[0-9]+/1$", preview()$keys, value = TRUE)[1L]
  move <- function(dx) jsonlite::toJSON(list(list(key = key, dx = dx, dy = 0)), auto_unbox = TRUE)
  origin_x <- function(line) as.numeric(sub(".* x1='([^']+)'.*", "\\1", line))
  session$setInputs(movements = move(8))
  session$setInputs(connect_labels = TRUE, label_anchor_mode = "previous", movements = move(15))
  stopifnot(identical(current()$previous_label_offsets[[key]], c(8, 0)))
  previous <- connector_lines(current())
  session$setInputs(label_anchor_mode = "original")
  original <- connector_lines(current())
  stopifnot(length(previous) == 1L, length(original) == 1L,
            abs(origin_x(previous) - origin_x(original) - 8 * 72 / 25.4) < 0.02)
  session$setInputs(label_anchor_mode = "previous")
  stopifnot(identical(connector_lines(unserialize(serialize(current(), NULL))), previous))
  session$setInputs(movements = '[{"undo":true}]')
  stopifnot(is.null(current()$previous_label_offsets[[key]]),
            identical(current()$element_edits[[key]], c(8, 0)))
  undone <- connector_lines(current())
  stopifnot(length(undone) == 1L, origin_x(undone) == origin_x(original))
})

data <- data.frame(x = 1:3, y = 1:3, label = c("Left", "Boxed", "Right"))
project <- figure_project(ggplot(data, aes(x, y, label = label)) + geom_label())
key <- grep("/Boxed$", ggfigurestudio:::tick_preview(project)$keys, value = TRUE)
project$connect_labels <- TRUE
project$element_edits[[key]] <- c(20, 0)
line <- connector_lines(project)
stopifnot(length(line) == 1L)
x1 <- as.numeric(sub(".* x1='([^']+)'.*", "\\1", line))
x2 <- as.numeric(sub(".* x2='([^']+)'.*", "\\1", line))
stopifnot(x2 > x1, x2 - x1 < 20 * 72 / 25.4 - 5)
