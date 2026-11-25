library(ggfigurestudio)
library(ggplot2)

groups <- ggfigurestudio:::custom_theme_groups()
stopifnot(setequal(unlist(groups, use.names = FALSE), names(get_element_tree())))
parse_value <- ggfigurestudio:::read_theme_value
stopifnot(identical(parse_value('grid::unit(c(1, 2), "mm")'), grid::unit(c(1, 2), "mm")),
          inherits(parse_value('element_text(size = rel(1.2))'), "element_text"),
          inherits(try(parse_value('base::get("x")'), silent = TRUE), "try-error"),
          inherits(try(parse_value('{ x <- 1 }'), silent = TRUE), "try-error"))

plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point() + labs(title = "Theme check") + theme_light()
app <- ggfigurestudio:::figure_editor(figure_project(plot))
shiny::testServer(app, {
  session$setInputs(custom_theme_element = "plot.title", custom_theme_property_size = "18",
                    custom_theme_property_family = '"mono"',
                    custom_theme_property_face = '"bold"', custom_theme_property_colour = '"#7D2385"',
                    apply_custom_theme = 1L)
  stopifnot(edited_plot()$theme$plot.title$size == 18,
            edited_plot()$theme$plot.title$colour == "#7D2385",
            edited_plot()$theme$plot.title$family == "mono",
            edited_plot()$theme$plot.title$face == "bold")
  html <- output$custom_theme_fields$html
  stopifnot(grepl('<select[^>]*id="custom_theme_property_family"', html),
            grepl('<select[^>]*id="custom_theme_property_face"', html),
            grepl("Device default", html, fixed = TRUE), grepl("Bold italic", html, fixed = TRUE),
            grepl("Inherited", html, fixed = TRUE))
  session$setInputs(custom_theme_element = "plot.margin", custom_theme_value = "margin(10, 12, 8, 4)",
                    apply_custom_theme = 2L)
  stopifnot(identical(as.numeric(edited_plot()$theme$plot.margin), c(10, 12, 8, 4)))
  session$setInputs(custom_theme_element = "legend.position", custom_theme_choice = '"bottom"',
                    apply_custom_theme = 3L)
  stopifnot(identical(edited_plot()$theme$legend.position, "bottom"))
  session$setInputs(custom_theme_element = "panel.grid.major", hide_custom_theme = 1L)
  stopifnot(!any(startsWith(names(preview()$marks), "grid/panel.grid.major")))
  session$setInputs(custom_theme_element = "axis.text.x", custom_theme_code = TRUE,
                    custom_theme_value = 'element_text(angle = 35, hjust = 1)', apply_custom_theme = 4L)
  stopifnot(edited_plot()$theme$axis.text.x$angle == 35)
  session$setInputs(custom_theme_value = 'element_rect()', apply_custom_theme = 5L)
  stopifnot(edited_plot()$theme$axis.text.x$angle == 35)
  session$setInputs(inherit_custom_theme = 1L)
  stopifnot(is.null(edited_plot()$theme$axis.text.x))
  session$setInputs(custom_theme_element = "plot.title", custom_theme_code = FALSE,
                    custom_theme_property_family = "NULL", apply_custom_theme = 6L)
  stopifnot(is.null(edited_plot()$theme$plot.title$family),
            edited_plot()$theme$plot.title$face == "bold")
  file <- tempfile(fileext = ".svg")
  export_figure(current(), file)
  stopifnot(any(grepl("#7D2385", readLines(file, warn = FALSE), fixed = TRUE)))
})
