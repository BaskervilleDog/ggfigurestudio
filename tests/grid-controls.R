library(ggfigurestudio)
library(ggplot2)

plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
  theme_light() + theme(panel.grid.major = element_line(colour = "#B83E70"))
app <- ggfigurestudio:::figure_editor(figure_project(plot))
shiny::testServer(app, {
  grid_keys <- function(type) any(startsWith(names(preview()$marks), paste0("grid/panel.grid.", type)))
  stopifnot(grid_keys("major"), grid_keys("minor"))
  session$setInputs(grid_major = FALSE, grid_minor = TRUE, apply_grid = 1)
  stopifnot(!grid_keys("major"), grid_keys("minor"))
  file <- tempfile(fileext = ".svg")
  export_figure(current(), file)
  stopifnot(!any(grepl("#B83E70", readLines(file, warn = FALSE), fixed = TRUE)))
  session$setInputs(grid_major = TRUE, grid_minor = FALSE, apply_grid = 2)
  stopifnot(grid_keys("major"), !grid_keys("minor"))
  export_figure(current(), file)
  stopifnot(any(grepl("#B83E70", readLines(file, warn = FALSE), fixed = TRUE)))
})

project <- figure_project(plot + theme_void())
project$grid_visibility <- list(major = TRUE, minor = TRUE)
keys <- names(ggfigurestudio:::tick_preview(project)$marks)
stopifnot(any(startsWith(keys, "grid/panel.grid.major")),
          any(startsWith(keys, "grid/panel.grid.minor")))
