tick_preview <- function(project) {
  capture <- new.env(parent = emptyenv())
  capture$hits <- list()
  issues <- character()
  canvas <- svglite::svgstring(width = project$width / 25.4, height = project$height / 25.4,
                               standalone = TRUE)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device))
  figure <- withCallingHandlers(build_figure(project, capture), warning = function(w) {
    issues <<- c(issues, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  grid::grid.draw(figure)
  grDevices::dev.off(device)
  svg <- charToRaw(enc2utf8(as.character(canvas())))
  keys <- names(capture$hits)
  axis_keys <- keys[grepl("^element/axis-[btlr](-[0-9]+)*$", keys)]
  axis_groups <- stats::setNames(sub("(-[0-9]+)+$", "", axis_keys), axis_keys)
  panel_keys <- keys[startsWith(keys, "panel-ticks/")]
  mark_move_keys <- unlist(lapply(capture$paths, function(path) path$move_key), use.names = FALSE)
  buttons <- lapply(keys, function(key) {
    box <- capture$hits[[key]] * 25.4
    style <- sprintf(paste0("position:absolute;left:%f%%;top:%f%%;width:%f%%;height:%f%%;",
                            "background:transparent;border:0;padding:0;touch-action:none;cursor:move"),
                     100 * box[1] / project$width, 100 * (1 - box[4] / project$height),
                     100 * (box[3] - box[1]) / project$width,
                     100 * (box[4] - box[2]) / project$height)
    label <- capture$names[[key]]
    region <- key %in% axis_keys
    if (region) style <- paste0(style, ";cursor:pointer")
    panel <- key %in% panel_keys
    if (panel) style <- paste0(style, ";z-index:0;cursor:pointer")
    else if (region || identical(label, "Legend (whole group)")) style <- paste0(style, ";z-index:1")
    else style <- paste0(style, ";z-index:2")
    shiny::tags$button(type = "button", class = if (region || panel) "figure-axis" else "figure-tick", style = style,
                       title = label, `aria-label` = label, `data-key` = key,
                       `data-inspect-mark` = capture$links[[key]],
                       `data-move-key` = if (region) axis_groups[[key]] else if (!panel) key)
  })
  wrapper <- shiny::tags$div(class = "figure-canvas", `data-width` = project$width,
    `data-height` = project$height, tabindex = "-1",
    style = sprintf("position:relative;width:%fmm;aspect-ratio:%f/%f",
                    project$width, project$width, project$height),
    shiny::tags$img(alt = "Figure preview", draggable = "false",
                    style = "position:absolute;inset:0;width:100%;height:100%",
                    src = paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(svg))),
    mark_overlay(capture$paths, project$width, project$height),
    buttons)
  list(canvas = wrapper, keys = setdiff(keys, axis_keys), axis_keys = axis_keys,
       move_keys = unique(c(setdiff(keys, c(axis_keys, panel_keys)), unname(axis_groups), mark_move_keys)),
       issues = issues, marks = capture$marks, annotation_facets = capture$annotation_facets,
       text_targets = capture$text_targets, facet_targets = capture$facet_targets)
}
