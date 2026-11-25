library(ggfigurestudio)
library(ggplot2)

categorical <- ggfigurestudio:::palette_groups(c(colour = "discrete"))
continuous <- ggfigurestudio:::palette_groups(c(fill = "continuous"))
mixed <- ggfigurestudio:::palette_groups(c(colour = "discrete", fill = "continuous"))
stopifnot(all(startsWith(names(categorical)[-1L], "Categorical")),
          all(startsWith(names(continuous)[-1L], "Continuous")),
          identical(names(mixed)[2L], "Categorical and continuous (viridis)"),
          !any(startsWith(unlist(categorical), "Gradient:")),
          all(startsWith(unlist(continuous[-1L]), "Gradient:")))
picker <- as.character(ggfigurestudio:::mark_palette_input(
  list(palette_targets = c(colour = "discrete", fill = "continuous")), character(), FALSE))
stopifnot(grepl("Colour uses categorical data. Fill uses continuous data.", picker, fixed = TRUE))

for (type in c("discrete", "continuous")) {
  options <- setdiff(ggfigurestudio:::palette_choices(type), c("", "Original"))
  if (!requireNamespace("colorspace", quietly = TRUE)) options <- options[!grepl("HCL", options)]
  for (id in options) {
    scale <- ggfigurestudio:::palette_scale(id, "fill")
    values <- scale$palette(if (type == "continuous") c(0, 0.5, 1) else 3L)
    stopifnot(length(values) == 3L, all(!is.na(values)))
  }
}
stopifnot(length(ggfigurestudio:::palette_scale("Brewer:Dark2", "colour")$palette(12L)) == 12L)
stopifnot(identical(ggfigurestudio:::viridis_palette("Viridis:G", c("discrete", "continuous")),
                    c("Viridis:G", "Gradient:G")))

plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species, fill = Petal.Length)) +
  geom_point(shape = 21) + scale_colour_manual(values = c("red", "green", "blue"), name = "Species") +
  scale_fill_gradient(low = "white", high = "black", limits = c(1, 8), name = "Length")
updated <- ggfigurestudio:::apply_plot_palettes(plot, c(colour = "Brewer:Dark2", fill = "Gradient:Brewer:RdBu"))
stopifnot(identical(updated$scales$get_scales("colour")$name, "Species"),
          identical(updated$scales$get_scales("fill")$limits, c(1, 8)),
          identical(updated$scales$get_scales("fill")$name, "Length"))
project <- figure_project(plot)
project$point_palettes <- c(colour = "Brewer:Dark2", fill = "Gradient:Brewer:RdBu")
file <- tempfile(fileext = ".svg")
export_figure(project, file)
stopifnot(file.size(file) > 1000)

shiny::testServer(ggfigurestudio:::figure_editor(figure_project(plot)), {
  keys <- names(preview()$marks)
  key <- keys[startsWith(keys, "mark/")][1L]
  session$setInputs(inspect_element = key)
  session$setInputs(point_palette = "Viridis:G", apply_point_palette = 1L)
  stopifnot(identical(current()$point_palettes, c(colour = "Viridis:G", fill = "Gradient:G")))
})
