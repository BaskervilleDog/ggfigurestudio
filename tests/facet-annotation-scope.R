library(ggfigurestudio)
library(ggplot2)

plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() + facet_wrap(vars(Species))
app <- ggfigurestudio:::figure_editor(figure_project(plot))
shiny::testServer(app, {
  facets <- unname(preview()$annotation_facets)
  copies <- function(layer) Filter(function(mark) identical(mark$layer, layer), preview()$marks)
  panels <- function(layer) vapply(copies(layer), function(mark) mark$panel_suffix, character(1))
  session$setInputs(layer_operation = "vertical", annotation_add_facets = facets[2:3],
                    reference_at = "5.7", reference_colour = "navy", reference_width = 0.8,
                    reference_type = "solid", apply_layer = 1)
  session$setInputs(layer_operation = "horizontal", annotation_add_facets = facets[3L],
                    reference_at = "3", apply_layer = 2)
  session$setInputs(layer_operation = "box", annotation_add_facets = facets[1L],
                    new_shape_x = "5.2", new_shape_y = "3.1", new_shape_width = 12,
                    new_shape_height = 8, new_shape_angle = 0, new_shape_fill = "gold",
                    new_shape_border = "black", new_shape_border_width = 0.4,
                    new_shape_fill_alpha = 1, new_shape_border_alpha = 1, new_shape_text = "",
                    new_shape_text_colour = "black", new_shape_text_size = 10, apply_layer = 3)
  session$setInputs(layer_operation = "text", annotation_add_facets = facets[1:2],
                    new_text_x = "5.5", new_text_y = "3.3", new_text_angle = 0,
                    new_text_text = "Two facets", new_text_text_colour = "black",
                    new_text_text_size = 10, apply_layer = 4)
  session$setInputs(layer_operation = "label", annotation_add_facets = facets[2L],
                    new_label_x = "6", new_label_y = "2.8", new_label_angle = 0,
                    new_label_text = "One facet", new_label_text_colour = "black",
                    new_label_text_size = 10, new_label_fill = "white", new_label_border = "black",
                    new_label_border_width = 0.4, new_label_fill_alpha = 1,
                    new_label_border_alpha = 1, apply_layer = 5)
  stopifnot(setequal(panels(2L), facets[2:3]), setequal(panels(3L), facets[3L]),
            setequal(panels(4L), facets[1L]), setequal(panels(5L), facets[1:2]),
            setequal(panels(6L), facets[2L]))

  session$setInputs(inspect_element = names(copies(2L))[1L], annotation_edit_mode = "selected",
                    annotation_edit_facets = facets[2:3], mark_intercept = "6.2",
                    mark_colour = "#224477", apply_mark = 1)
  stopifnot(all(vapply(copies(2L), function(mark) mark$reference$value == "6.2" &&
                         mark$values$colour == "#224477", logical(1))))

  session$setInputs(inspect_element = names(copies(5L))[1L], annotation_edit_mode = "all",
                    edit_shape_x = "5.8", edit_shape_y = "3.3", edit_shape_angle = 0,
                    edit_shape_text = "Edited together", edit_shape_text_colour = "black",
                    edit_shape_text_size = 10, apply_shape_edit = 1)
  stopifnot(all(vapply(copies(5L), function(mark) mark$annotation$x == "5.8" &&
                         mark$annotation$text == "Edited together", logical(1))))
  session$setInputs(annotation_edit_mode = "this", edit_shape_text = "Just this copy", apply_shape_edit = 2)
  stopifnot(sum(vapply(copies(5L), function(mark) mark$annotation$text == "Just this copy", logical(1))) == 1L)

  output_file <- tempfile(fileext = ".svg")
  export_figure(current(), output_file)
  svg <- readLines(output_file, warn = FALSE)
  stopifnot(sum(grepl("One facet", svg, fixed = TRUE)) == 1L,
            sum(grepl("stroke: #224477", svg, fixed = TRUE)) == 2L)

  project <- current()
  project$plot$data$Species <- factor(project$plot$data$Species, levels = rev(levels(iris$Species)))
  reordered <- ggfigurestudio:::tick_preview(project)
  labels <- Filter(function(mark) identical(mark$layer, 6L), reordered$marks)
  stopifnot(length(labels) == 1L, identical(labels[[1L]]$panel_suffix, facets[2L]))
})
