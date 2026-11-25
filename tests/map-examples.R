library(ggfigurestudio)

catalog <- map_examples()
stopifnot(nrow(catalog) == 7L, !anyDuplicated(catalog$id))
expected_kinds <- list(
  counties = "Area", county_classes = "Area", stations = c("Point", "Text"),
  routes = c("Line", "Point"), raster = character(), tiles = "Tile", contours = "Line"
)
for (i in seq_len(nrow(catalog))) {
  if (!requireNamespace(catalog$packages[i], quietly = TRUE)) next
  id <- catalog$id[i]
  project <- figure_project(map_example(i), width = 220, height = 155)
  preview <- ggfigurestudio:::tick_preview(project)
  kinds <- unique(vapply(preview$marks, function(mark) {
    if (is.null(mark$kind)) "" else mark$kind
  }, character(1)))
  stopifnot(!length(preview$issues), all(expected_kinds[[id]] %in% kinds))
  if (id == "raster") {
    stopifnot(inherits(project$plot$layers[[1L]]$geom, "GeomRaster"), !"Tile" %in% kinds)
    project$plot <- ggfigurestudio:::replace_layer_geom(project$plot, 1L, "tile")
    preview <- ggfigurestudio:::tick_preview(project)
    stopifnot(sum(startsWith(names(preview$marks), "tile/")) == nrow(project$plot$data))
  }
  if (id == "county_classes") {
    key <- names(preview$marks)[startsWith(names(preview$marks), "area/")][1L]
    project$point_palettes <- c(fill = "Set2")
    changed <- ggfigurestudio:::tick_preview(project)
    stopifnot(preview$marks[[key]]$palette_type == "discrete",
              changed$marks[[key]]$values$fill != preview$marks[[key]]$values$fill)
  }
  filename <- tempfile(fileext = ".svg")
  export_figure(project, filename)
  stopifnot(file.size(filename) > 1000)
}
