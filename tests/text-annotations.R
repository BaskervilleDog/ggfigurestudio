library(ggfigurestudio)
library(ggplot2)

plot <- ggplot(iris[1:100, ], aes(Sepal.Length, Sepal.Width)) +
  geom_point() + facet_wrap(vars(Species))
app <- ggfigurestudio:::figure_editor(figure_project(plot))
shiny::testServer(app, {
  session$setInputs(layer_operation = "text", new_text_x = "4.9", new_text_y = "2.7",
                    new_text_angle = 0, new_text_text = "Plain note", new_text_text_colour = "navy",
                    new_text_text_size = 10, apply_layer = 1)
  session$setInputs(layer_operation = "label", new_label_x = "5.7", new_label_y = "3.2",
                    new_label_angle = 25, new_label_text = "Boxed note",
                    new_label_text_colour = "black", new_label_text_size = 13,
                    new_label_border = "black", new_label_fill = "#E8D799",
                    new_label_fill_alpha = 1, new_label_border_alpha = 1,
                    new_label_border_width = 0.5, apply_layer = 2)

  marks <- preview()$marks
  labels <- Filter(function(mark) identical(mark$annotation$type, "label"), marks)
  text <- Filter(function(mark) identical(mark$annotation$type, "text"), marks)
  stopifnot(length(labels) == 2L, length(text) == 2L)
  key <- names(labels)[1L]
  move_key <- labels[[1L]]$move_key
  stopifnot(move_key %in% preview()$move_keys)
  session$setInputs(inspect_element = key)
  html <- output$element_options$html
  stopifnot(grepl('<select[^>]*id="edit_shape_text_family"', html),
            grepl('<select[^>]*id="edit_shape_text_fontface"', html),
            grepl("Bold italic", html, fixed = TRUE))
  session$setInputs(movements = jsonlite::toJSON(list(list(key = move_key, dx = 3, dy = 2)), auto_unbox = TRUE))
  stopifnot(identical(current()$element_edits[[move_key]], c(3, 2)))
  session$setInputs(inspect_element = key, annotation_all_facets = FALSE,
                    edit_shape_x = "5.7", edit_shape_y = "3.2", edit_shape_angle = 25,
                    edit_shape_text = "Edited label", edit_shape_text_colour = "black",
                    edit_shape_text_family = "mono", edit_shape_text_fontface = "bold.italic",
                    edit_shape_text_size = 13, edit_shape_border = "black", edit_shape_fill = "#E8D799",
                    edit_shape_fill_alpha = 1, edit_shape_border_alpha = 1,
                    edit_shape_border_width = 0.5, apply_shape_edit = 1)
  stopifnot(preview()$marks[[key]]$annotation$text == "Edited label",
            preview()$marks[[key]]$annotation$text_family == "mono",
            preview()$marks[[key]]$annotation$text_fontface == "bold.italic",
            preview()$marks[[names(labels)[2L]]]$annotation$text_family == "",
            preview()$marks[[names(labels)[2L]]]$annotation$text == "Boxed note")
  session$setInputs(inspect_element = names(text)[1L], annotation_all_facets = TRUE,
                    edit_shape_x = "4.9", edit_shape_y = "2.7", edit_shape_angle = 0,
                    edit_shape_text = "Plain note", edit_shape_text_colour = "navy",
                    edit_shape_text_size = 10, edit_shape_text_family = "serif",
                    edit_shape_text_fontface = "italic", apply_shape_edit = 2)
  styled_text <- preview()$marks[names(text)]
  stopifnot(all(vapply(styled_text, function(mark) {
    identical(mark$annotation$text_family, "serif") && identical(mark$annotation$text_fontface, "italic")
  }, logical(1))))
  file <- tempfile(fileext = ".svg")
  export_figure(current(), file)
  svg <- readLines(file, warn = FALSE)
  edited_label <- grep("Edited label", svg, value = TRUE, fixed = TRUE)
  stopifnot(sum(grepl("Plain note", svg, fixed = TRUE)) == 2L,
            sum(grepl("fill: #E8D799", svg, fixed = TRUE)) == 2L,
            length(edited_label) == 1L, grepl("font-weight: bold", edited_label, fixed = TRUE),
            grepl("font-style: italic", edited_label, fixed = TRUE))
  families <- intersect(c("Arial", "DejaVu Sans"), ggfigurestudio:::editor_font_families())
  if (capabilities("cairo") && length(families)) {
    pdf_project <- current()
    mark <- preview()$marks[[key]]
    pdf_project$plot <- ggfigurestudio:::set_facet_annotation_style(pdf_project$plot, mark,
      list(text_family = families[[1L]]), panels = mark$panel_suffix)
    pdf <- tempfile(fileext = ".pdf")
    export_figure(pdf_project, pdf)
    stopifnot(identical(readChar(pdf, 4L, useBytes = TRUE), "%PDF"))
  }
  session$setInputs(inspect_element = key, delete_shape = 1)
  stopifnot(length(current()$plot$layers) == 2L)
  session$setInputs(movements = '[{"undo":true}]')
  stopifnot(length(current()$plot$layers) == 3L,
            identical(current()$element_edits[[move_key]], c(3, 2)))
})

notes <- data.frame(x = c(1, 2), y = c(1, 2), text = c("First", "Second"), group = c("a", "b"))
p <- ggplot(notes, aes(x, y, label = text, group = group)) +
  geom_text(fontface = 1) + geom_label(fontface = 3, family = "serif", nudge_y = 0.2)
shiny::testServer(ggfigurestudio:::figure_editor(figure_project(p)), {
  marks <- preview()$marks
  text <- Filter(function(mark) identical(mark$kind, "Text"), marks)
  labels <- Filter(function(mark) identical(mark$kind, "Boxed label"), marks)
  stopifnot(length(text) == 2L, length(labels) == 2L)
  text_key <- names(text)[1L]
  session$setInputs(inspect_element = text_key)
  html <- output$element_options$html
  stopifnot(grepl('<select[^>]*id="mark_family"', html),
            grepl('<select[^>]*id="mark_fontface"', html),
            grepl('<option value="plain" selected>Plain</option>', html, fixed = TRUE),
            grepl('selected>Device default</option>', html, fixed = TRUE))
  session$setInputs(mark_family = "mono", mark_fontface = "bold", apply_mark = 1)
  stopifnot(preview()$marks[[text_key]]$values$fontface == "bold",
            preview()$marks[[text_key]]$values$family == "mono",
            ggfigurestudio:::mark_fontface_name(preview()$marks[[names(text)[2L]]]$values$fontface) == "plain")
  label_key <- names(labels)[1L]
  session$setInputs(inspect_element = label_key)
  html <- output$element_options$html
  stopifnot(grepl('<option value="italic" selected>Italic</option>', html, fixed = TRUE),
            grepl('<option value="serif" selected>Serif</option>', html, fixed = TRUE))
  session$setInputs(mark_family = "[device default]", mark_fontface = "italic", apply_mark = 2)
  stopifnot(preview()$marks[[label_key]]$values$family == "",
            preview()$marks[[names(labels)[2L]]]$values$family == "serif",
            is.null(edited_plot()$layers[[2L]]$geom$figure_group_styles[[labels[[1L]]$group]]$fontface))
  file <- tempfile(fileext = ".svg")
  export_figure(current(), file)
  stopifnot(any(grepl("font-weight: bold", readLines(file, warn = FALSE), fixed = TRUE)))
})
