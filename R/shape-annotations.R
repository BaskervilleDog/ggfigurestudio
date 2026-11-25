shape_style_defaults <- list(border_width = 1 / ggplot2::.pt,
                             border_alpha = 1, fill_alpha = 1,
                             text_family = "", text_fontface = "plain", text_format = "plain")

annotation_text <- function(text, format) {
  if (!identical(format, "plotmath")) return(text)
  expression <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) {
    stop("Enter a valid plotmath expression, such as hat(y) == beta[0] + beta[1]*x.", call. = FALSE)
  })
  if (length(expression) != 1L)
    stop("Enter one plotmath expression. Use atop() to stack two lines.", call. = FALSE)
  expression
}

shape_fields <- function(prefix, values = list(), type = values$type) {
  textual <- length(type) > 0L && type %in% c("text", "label")
  field <- function(name) paste0(prefix, name)
  value <- function(name, default) {
    if (is.null(values[[name]])) default else values[[name]]
  }
  shiny::tagList(
    editor_group("Position",
      editor_fields(shiny::textInput(field("x"), "Centre x", value("x", "")),
                    shiny::textInput(field("y"), "Centre y", value("y", ""))),
      description = "Enter the centre in the plot's axis units."),
    editor_group(if (textual) "Rotation" else "Size and rotation",
      if (!textual) editor_fields(shiny::numericInput(field("width"), "Width (mm)", value("width", 20), min = 0),
                    shiny::numericInput(field("height"), "Height (mm)", value("height", 20), min = 0)),
      shiny::numericInput(field("angle"), "Rotation (degrees)", value("angle", 0), step = 1)),
    if (!identical(type, "text")) editor_group("Fill and border",
      editor_fields(
        shiny::textInput(field("fill"), "Fill colour", value("fill", "#FFF2CC")),
        shiny::numericInput(field("fill_alpha"), "Fill opacity (alpha)",
                            value("fill_alpha", shape_style_defaults$fill_alpha), min = 0, max = 1, step = 0.05),
        shiny::textInput(field("border"), "Border colour", value("border", "#725433")),
        shiny::numericInput(field("border_width"), "Border width (mm)",
                            value("border_width", round(shape_style_defaults$border_width, 2)), min = 0, step = 0.05),
        shiny::numericInput(field("border_alpha"), "Border opacity (alpha)",
                            value("border_alpha", shape_style_defaults$border_alpha), min = 0, max = 1, step = 0.05)),
      description = "Opacity runs from 0 (transparent) to 1 (opaque)."),
    editor_group(if (textual) "Content" else "Attached label",
      shiny::textAreaInput(field("text"), if (textual) "Text" else "Attached text (optional)",
                           value("text", ""), rows = 2, width = "100%"),
      editor_fields(shiny::textInput(field("text_colour"), "Text colour", value("text_colour", "#252525")),
                    shiny::numericInput(field("text_size"), "Text size (pt)", value("text_size", 10), min = 0)),
      if (textual) {
        family <- value("text_family", shape_style_defaults$text_family)
        families <- c(`Device default` = "[device default]", editor_font_families())
        selected <- if (nzchar(family)) family else "[device default]"
        if (!selected %in% families) families <- c(families, stats::setNames(selected, selected))
        shiny::tagList(
          editor_select(field("text_format"), "Text format",
            c("Plain text" = "plain", "Math (plotmath)" = "plotmath"),
            selected = value("text_format", shape_style_defaults$text_format), width = "100%"),
          shiny::conditionalPanel(sprintf("input['%s'] === 'plotmath'", field("text_format")),
            shiny::p(class = "editor-help",
              "Enter an equation such as hat(y) == beta[0] + beta[1]*x. Use x^2 for powers and x[i] for subscripts.",
              shiny::tags$br(),
              shiny::tags$a("Plotmath reference",
                href = "https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html",
                target = "_blank", rel = "noopener noreferrer"))),
          editor_fields(
          editor_select(field("text_family"), "Font family", choices = families, selected = selected, width = "100%"),
          editor_select(field("text_fontface"), "Font style", choices = editor_font_faces,
                        selected = value("text_fontface", shape_style_defaults$text_fontface), width = "100%")))
      })
  )
}

shape_spec <- function(input, prefix, type, previous = list()) {
  field <- function(name) input[[paste0(prefix, name)]]
  positive <- function(name, label) {
    value <- field(name)
    if (length(value) != 1L || !is.finite(value) || value <= 0)
      stop(label, " must be greater than zero.")
    value
  }
  content <- field("text")
  if (type %in% c("text", "label") && (length(content) != 1L || !nzchar(trimws(content))))
    stop("Enter the text to display.")
  annotation <- list(type = type, x = field("x"), y = field("y"),
    angle = mark_value("angle", field("angle")), text = content,
    text_colour = mark_value("colour", field("text_colour")), text_size = positive("text_size", "Text size"))
  if (type %in% c("text", "label")) {
    defaults <- utils::modifyList(shape_style_defaults, previous)
    family <- field("text_family")
    face <- field("text_fontface")
    format <- field("text_format")
    annotation$text_family <- if (!length(family)) defaults$text_family
      else if (identical(family, "[device default]")) "" else family
    annotation$text_fontface <- if (length(face)) match.arg(face, unname(editor_font_faces)) else defaults$text_fontface
    annotation$text_format <- if (length(format)) match.arg(format, c("plain", "plotmath")) else defaults$text_format
    annotation_text(content, annotation$text_format)
  }
  if (type %in% c("box", "circle")) {
    annotation$width <- positive("width", "Width")
    annotation$height <- positive("height", "Height")
  }
  if (type != "text") {
    annotation$fill <- mark_value("fill", field("fill"))
    annotation$border <- mark_value("colour", field("border"))
    annotation$fill_alpha <- mark_value("alpha", field("fill_alpha"))
    annotation$border_width <- mark_value("linewidth", field("border_width"))
    annotation$border_alpha <- mark_value("alpha", field("border_alpha"))
  }
  annotation
}

shape_title <- function(shape) {
  if (shape$type == "text") return("Text annotation")
  if (shape$type == "label") return("Boxed label")
  if (shape$type == "box") return("Box annotation")
  if (shape$width == shape$height) "Circle annotation" else "Ellipse annotation"
}

shape_edit_name <- function(annotation) switch(annotation$type, text = "text", label = "label", "shape")

shape_angle <- function(annotation) {
  if (is.null(annotation$angle)) 0 else annotation$angle %% 360
}

shape_outline <- function(annotation, width = annotation$width, height = annotation$height) {
  if (annotation$type == "circle") {
    theta <- 2 * pi * (0:127) / 128
    dx <- cos(theta)
    dy <- sin(theta)
  } else {
    dx <- c(-1, 1, 1, -1)
    dy <- c(-1, -1, 1, 1)
  }
  x <- dx * width / 2
  y <- dy * height / 2
  radians <- shape_angle(annotation) * pi / 180
  list(x = x * cos(radians) - y * sin(radians),
       y = x * sin(radians) + y * cos(radians))
}

GeomFigureShape <- ggplot2::ggproto("GeomFigureShape", ggplot2::Geom,
  required_aes = c("x", "y"),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(self, data, panel_params, coord, annotation = NULL) {
    pos <- coord$transform(data, panel_params)
    if (!nrow(pos) || !all(is.finite(c(pos$x[1L], pos$y[1L])))) return(grid::nullGrob())
    facet_style <- data$.figure_facet_style[[1L]]
    if (length(facet_style)) annotation[names(facet_style)] <- facet_style
    panels <- self$figure_shape_panels
    suffix <- panels[[as.character(data$PANEL[1L])]]
    key <- paste0(self$figure_shape_key, suffix)
    offset <- self$figure_shape_offset
    if (is.null(offset)) offset <- c(0, 0)
    base_offset <- offset
    if (length(suffix)) {
      panel_offset <- self$figure_shape_panel_offsets[[paste0("element/", key)]]
      if (length(panel_offset)) offset <- offset + panel_offset
    }
    x <- grid::unit(pos$x[1L], "native") + grid::unit(offset[1L], "mm")
    y <- grid::unit(pos$y[1L], "native") + grid::unit(offset[2L], "mm")
    angle <- shape_angle(annotation)
    style <- utils::modifyList(shape_style_defaults, annotation)
    label <- grid::textGrob(annotation_text(style$text, style$text_format), x, y,
      gp = grid::gpar(col = annotation$text_colour, fontsize = annotation$text_size,
                      fontfamily = style$text_family, fontface = style$text_fontface))
    dims <- if (annotation$type %in% c("text", "label")) {
      padding <- grid::unit(if (annotation$type == "label") 4 else 1.2, "mm")
      list(width = grid::grobWidth(label) + padding, height = grid::grobHeight(label) + padding)
    }
    label$rot <- angle
    shape <- grid::nullGrob()
    if (annotation$type != "text") {
      border <- if (style$border_width > 0)
        grDevices::adjustcolor(style$border, alpha.f = style$border_alpha) else NA_character_
      gp <- grid::gpar(fill = grDevices::adjustcolor(style$fill, alpha.f = style$fill_alpha),
                       col = border, lwd = style$border_width * ggplot2::.pt)
      shape <- if (annotation$type == "label") {
        grid::grobTree(grid::rectGrob(gp = gp),
          vp = grid::viewport(x = x, y = y, width = dims$width, height = dims$height, angle = angle))
      } else if (annotation$type == "box" && angle == 0) {
        grid::rectGrob(x, y, width = grid::unit(annotation$width, "mm"),
                       height = grid::unit(annotation$height, "mm"), gp = gp)
      } else if (annotation$type == "circle" && annotation$width == annotation$height) {
        grid::circleGrob(x, y, r = grid::unit(annotation$width / 2, "mm"), gp = gp)
      } else {
        outline <- shape_outline(annotation)
        grid::polygonGrob(x + grid::unit(outline$x, "mm"), y + grid::unit(outline$y, "mm"), gp = gp)
      }
    }
    connector <- if (isTRUE(self$figure_connect_labels) && length(dims)) {
      target <- if (annotation$type == "label") {
        outline <- shape_outline(annotation, dims$width, dims$height)
        grid::polygonGrob(x + outline$x, y + outline$y)
      } else label
      anchor <- self$figure_previous_label_offsets[[paste0("element/", key)]]
      if (length(suffix) && length(anchor)) anchor <- base_offset + anchor
      label_connector(grid::unit(pos$x[1L], "native"), grid::unit(pos$y[1L], "native"), offset, target, anchor)
    }
    if (is.null(self$figure_hit_capture))
      return(grid::grobTree(connector, shape, label, cl = "figure_shape_annotation"))
    if (length(suffix)) {
      positions <- self$figure_shape_panel_anchors[[suffix]]
      annotation[names(positions)] <- positions
    }
    probe <- grid::grob(x = x, y = y, annotation = annotation, capture = self$figure_hit_capture,
                        layer = self$figure_hit_layer, key = key, panel_suffix = suffix, dims = dims,
                        cl = "figure_shape_probe")
    grid::grobTree(connector, shape, label, probe, cl = "figure_shape_annotation")
  }
)

shape_layer_key <- function(layer, index) {
  id <- layer$figure_shape_id
  paste0("shape/", if (is.null(id)) index else id)
}

identify_annotation_layers <- function(plot) {
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    field <- if (inherits(layer$geom, "GeomFigureShape")) "figure_shape_id"
      else if (length(reference_line_info(layer))) "figure_reference_id"
    if (is.null(field) || !is.null(layer[[field]])) next
    copy <- rlang::env_clone(layer)
    attributes(copy) <- attributes(layer)
    copy[[field]] <- basename(tempfile("annotation-"))
    plot$layers[[i]] <- copy
  }
  plot
}

shape_plot <- function(plot, annotation, index = length(plot$layers) + 1L, panel = 1L) {
  scales <- ggplot2::ggplot_build(scope_annotation_facets(plot))$layout$get_scales(panel)
  data <- data.frame(x = parse_axis_position(scales$x, annotation$x),
                     y = parse_axis_position(scales$y, annotation$y))
  layer <- ggplot2::layer(geom = GeomFigureShape, stat = "identity", position = "identity",
    data = data, mapping = ggplot2::aes(x, y), inherit.aes = FALSE, show.legend = FALSE,
    params = list(annotation = annotation))
  if (index <= length(plot$layers)) {
    original <- plot$layers[[index]]
    layer$layout <- original$layout
    layer$figure_shape_id <- original$figure_shape_id
    layer$figure_facet_positions <- original$figure_facet_positions
    layer$figure_facet_styles <- original$figure_facet_styles
  }
  if (is.null(layer$figure_shape_id)) layer$figure_shape_id <- basename(tempfile("annotation-"))
  plot$layers[[index]] <- layer
  plot
}

check_shape_preview <- function(project, index) {
  key <- shape_layer_key(project$plot$layers[[index]], index)
  marks <- tick_preview(project)$marks
  if (is.null(marks[[key]]) && !any(startsWith(as.character(names(marks)), annotation_panel_prefix(key))))
    stop("The annotation is outside the visible plot. Enter a centre inside the panel.")
}

drawDetails.figure_shape_probe <- function(x, recording) {
  centre <- grid::deviceLoc(x$x, x$y, valueOnly = TRUE)
  limits <- grid::deviceLoc(grid::unit(0:1, "npc"), grid::unit(0:1, "npc"), valueOnly = TRUE)
  annotation <- x$annotation
  outline <- if (length(x$dims)) {
    shape_outline(annotation, grid::convertWidth(x$dims$width, "mm", valueOnly = TRUE),
                  grid::convertHeight(x$dims$height, "mm", valueOnly = TRUE))
  } else shape_outline(annotation)
  xs <- centre$x + outline$x / 25.4
  ys <- centre$y + outline$y / 25.4
  if (max(xs) <= limits$x[1L] || min(xs) >= limits$x[2L] ||
      max(ys) <= limits$y[1L] || min(ys) >= limits$y[2L]) return(invisible(NULL))
  key <- x$key
  x$capture$marks[[key]] <- list(layer = x$layer, kind = "Shape", annotation = annotation,
    faceted = length(x$panel_suffix) > 0L, panel_suffix = x$panel_suffix, move_key = paste0("element/", key))
  x$capture$paths[[length(x$capture$paths) + 1L]] <- list(
    key = key, move_key = paste0("element/", key), x = xs, y = ys,
    bounds = c(limits$x, limits$y), polygons = list(seq_along(xs)))
}
