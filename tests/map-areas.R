library(ggfigurestudio)
library(ggplot2)

if (requireNamespace("sf", quietly = TRUE)) {
  counties <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  plot <- ggplot(counties) + geom_sf(aes(fill = BIR74)) + coord_sf(crs = 32119)
  project <- figure_project(plot)
  preview <- ggfigurestudio:::tick_preview(project)
  areas <- preview$marks[startsWith(names(preview$marks), "area/")]
  stopifnot(length(areas) == nrow(counties), areas[[1L]]$palette_type == "continuous")
  map <- ggplot(counties) + geom_sf(aes(fill = BIR74)) + coord_sf(default_crs = sf::st_crs(4326))
  map <- ggfigurestudio:::add_reference_line(map, "x", "-80", "navy", 0.8, "solid")
  key <- ggfigurestudio:::reference_move_key(map$layers[[2L]], 2L)
  stopifnot(key %in% ggfigurestudio:::tick_preview(figure_project(map))$move_keys)

  project$plot <- ggfigurestudio:::styled_plot(plot, 1L, NULL, list(fill = "magenta"),
                                             cell = areas[[1L]]$cell)
  project$point_palettes <- c(fill = "Gradient:C")
  edited <- ggfigurestudio:::tick_preview(project)
  stopifnot(edited$marks[[names(areas)[1L]]]$values$fill == "magenta",
            edited$marks[[names(areas)[2L]]]$values$fill != areas[[2L]]$values$fill)

  filename <- tempfile(fileext = ".svg")
  export_figure(project, filename)
  stopifnot(any(grepl("#FF00FF", readLines(filename, warn = FALSE), fixed = TRUE)))
}
