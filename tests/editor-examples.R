library(ggfigurestudio)
library(ggplot2)

groups <- ggfigurestudio:::editor_examples()
stopifnot(
  identical(groups[[1L]]$catalog, gallery_examples()),
  identical(groups[[2L]]$catalog, bioinfo_examples()),
  identical(groups[[3L]]$catalog, breeding_examples()),
  identical(groups[[4L]]$catalog, map_examples())
)

plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() + labs(title = "Starting figure")
app <- ggfigurestudio:::figure_editor(figure_project(plot, width = 210, height = 140))
shiny::testServer(app, {
  session$setInputs(nudge = 0.5, annotation_all_facets = TRUE, example_gallery = "General plots",
                    example_id = "scatter")
  title_override("Edited starting figure")
  session$setInputs(open_example = 1)
  stopifnot(document()$title == "Annotated scatter", is.null(current()$title),
            current()$width == 210, current()$height == 140)
  title_override("Edited scatter")
  session$setInputs(nudge = 2, restore_original = 1)
  stopifnot(document()$key == "original", current()$title == "Edited starting figure",
            current()$nudge == 0.5)
  session$setInputs(nudge = 0.5, open_example = 2)
  stopifnot(current()$title == "Edited scatter", current()$nudge == 2)
  session$setInputs(example_gallery = "Bioinformatics", example_id = "pca", open_example = 3)
  stopifnot(document()$title == "Sample PCA", is.null(current()$title))
  session$setInputs(example_gallery = "Plant breeding and quantitative genetics",
                    example_id = "field_map", open_example = 4)
  stopifnot(document()$key == "Plant breeding and quantitative genetics/field_map")
  if (requireNamespace("sf", quietly = TRUE)) {
    session$setInputs(example_gallery = "Maps", example_id = "counties", open_example = 5)
    stopifnot(document()$title == "County choropleth")
  }
})

shiny::testServer(app, {
  session$setInputs(example_gallery = "General plots", example_id = "stacked_bar", open_example = 1)
  bars <- function() {
    marks <- preview()$marks
    marks[startsWith(names(marks), "bar/1/")]
  }
  original <- bars()
  keys <- names(original)
  hits <- gregexpr('data-inspect="bar/1/', as.character(preview()$canvas), fixed = TRUE)[[1L]]
  stopifnot(length(keys) == 20L, length(hits) == 20L)
  fills <- function(marks) vapply(marks, function(bar) bar$values$fill, character(1))
  session$setInputs(inspect_element = keys[1L], tile_scope = "cell")
  stopifnot(grepl("Selected segment", output$element_options$html, fixed = TRUE))
  session$setInputs(mark_fill = "#E040FB", apply_mark = 1)
  stopifnot(fills(bars())[[1L]] == "#E040FB", identical(fills(bars())[-1L], fills(original)[-1L]))

  session$setInputs(tile_scope = "layer")
  html <- output$element_options$html
  stopifnot(grepl("All segments in layer", html, fixed = TRUE), grepl("categorical", html, fixed = TRUE))
  session$setInputs(mark_fill = "#009688", apply_mark = 2)
  stopifnot(all(fills(bars()) == "#009688"))
  file <- tempfile(fileext = ".svg")
  export_figure(current(), file)
  stopifnot(sum(grepl("fill: #009688", readLines(file, warn = FALSE), fixed = TRUE)) == length(keys))
  session$setInputs(reset_mark = 1)
  stopifnot(identical(fills(bars()), fills(original)))
  session$setInputs(point_palette = "Viridis", apply_point_palette = 1)
  stopifnot(length(unique(fills(bars()))) == 4L, !identical(fills(bars()), fills(original)))
})

composition <- figure_composition(list(Scatter = plot, Bars = gallery_example("stacked_bar")),
                                  width = 260, height = 150, gap = 6)
shiny::testServer(ggfigurestudio:::figure_editor(composition), {
  stopifnot(current()$width == 127, current()$height == 150)
  title_override("Edited scatter")
  session$setInputs(composition_plot = "2", edit_composition_plot = 1L)
  stopifnot(document()$title == "Bars", is.null(current()$title))
  title_override("Edited bars")
  session$setInputs(composition_columns = 1, composition_gap = 8, apply_composition_layout = 1L)
  stopifnot(current()$width == 260, current()$height == 71)
  session$setInputs(composition_plot = "1", edit_composition_plot = 2L, show_composition = TRUE)
  stopifnot(current()$title == "Edited scatter",
            grepl("Combined image preview", output$combined_preview$html, fixed = TRUE))
  result <- unserialize(serialize(combined_project(), NULL))
  stopifnot(result$plots$Bars$title == "Edited bars", result$plots$Scatter$title == "Edited scatter")
  svg <- tempfile(fileext = ".svg")
  export_figure(result, svg)
  lines <- readLines(svg, warn = FALSE)
  stopifnot(any(grepl("Edited scatter", lines, fixed = TRUE)), any(grepl("Edited bars", lines, fixed = TRUE)),
            any(grepl("width='737.01pt' height='425.20pt'", lines, fixed = TRUE)))
  session$setInputs(composition_gap = 200, apply_composition_layout = 2L)
  stopifnot(layout()$gap == 8)
})
