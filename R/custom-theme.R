custom_theme_groups <- function(tree = ggplot2::get_element_tree()) {
  elements <- sort(names(tree))
  prefixes <- sub("\\..*$", "", elements)
  headings <- c(plot = "Plot and titles", axis = "Axes", legend = "Legends",
                panel = "Panels and grids", strip = "Facets and strips")
  groups <- unname(headings[prefixes])
  groups[is.na(groups)] <- "Defaults and other settings"
  order <- c(unname(headings), "Defaults and other settings")
  choices <- split(elements, factor(groups, levels = order), drop = TRUE)
  lapply(choices, function(items) stats::setNames(items, items))
}

theme_value_functions <- function() {
  ggplot <- c("element_blank", "element_text", "element_line", "element_rect",
              "element_point", "element_polygon", "element_geom", "margin",
              "margin_auto", "margin_part", "rel")
  ggplot <- intersect(ggplot, getNamespaceExports("ggplot2"))
  grid <- c("unit", "unit.c", "arrow")
  functions <- c(mget(ggplot, asNamespace("ggplot2")), mget(grid, asNamespace("grid")),
                 list(c = base::c, list = base::list, `-` = base::`-`, `+` = base::`+`,
                      `*` = base::`*`, `/` = base::`/`))
  attr(functions, "packages") <- c(stats::setNames(rep("ggplot2", length(ggplot)), ggplot),
                                  stats::setNames(rep("grid", length(grid)), grid))
  functions
}

read_theme_value <- function(text) {
  expression <- parse(text = text, keep.source = FALSE)
  if (length(expression) != 1L) stop("Enter one theme value.")
  functions <- theme_value_functions()
  read <- function(node) {
    if (is.atomic(node) || is.null(node)) return(node)
    if (!is.call(node)) stop("Use literal values or a supported theme constructor.")
    head <- node[[1L]]
    if (is.call(head) && length(head) == 3L && identical(head[[1L]], quote(`::`))) {
      package <- as.character(head[[2L]])
      name <- as.character(head[[3L]])
      if (length(package) != 1L || length(name) != 1L || !name %in% names(functions) ||
          !identical(unname(attr(functions, "packages")[name]), package))
        stop("Use a supported ggplot2 or grid constructor.")
    } else {
      if (!is.symbol(head)) stop("Use a supported theme constructor.")
      name <- as.character(head)
    }
    if (!name %in% names(functions)) stop("Unsupported theme constructor: ", name, ".")
    do.call(functions[[name]], lapply(as.list(node)[-1L], read))
  }
  read(expression[[1L]])
}

theme_value_text <- function(value) {
  literal <- function(x) paste(deparse(x, control = "keepNA"), collapse = " ")
  if (is.null(value)) return("")
  if (inherits(value, "rel")) return(paste0("rel(", literal(as.numeric(value)), ")"))
  if (inherits(value, "unit")) {
    units <- grid::unitType(value)
    if (any(units %in% c("sum", "min", "max", "grobwidth", "grobheight", "grobx", "groby")))
      return("")
    if (inherits(value, "margin") || inherits(value, "ggplot2::margin"))
      return(paste0("margin(", paste(as.numeric(value), collapse = ", "), ", unit = ", literal(units[1L]), ")"))
    if (length(unique(units)) == 1L) units <- units[1L]
    return(paste0("unit(", literal(as.numeric(value)), ", ", literal(units), ")"))
  }
  if (is.atomic(value)) literal(value) else ""
}

theme_element_class <- function(spec) {
  if (inherits(spec$class, "S7_class")) spec$class@name else spec$class
}

theme_element_properties <- function(constructor) {
  fields <- setdiff(names(formals(constructor)), c("...", "color"))
  if ("linewidth" %in% fields) fields <- setdiff(fields, "size")
  fields
}

theme_property_choices <- function(field, families) {
  line_types <- c(Solid = "solid", Dashed = "dashed", Dotted = "dotted",
                  `Dot-dash` = "dotdash", `Long dash` = "longdash",
                  `Two dashes` = "twodash", Blank = "blank")
  switch(field,
    family = c(`Device default` = "", families),
    face = editor_font_faces,
    linetype = line_types,
    bordertype = line_types,
    linejoin = c(Round = "round", Mitre = "mitre", Bevel = "bevel"),
    lineend = c(Round = "round", Flat = "butt", Square = "square"),
    hjust = c(Left = 0, Center = 0.5, Right = 1),
    vjust = c(Bottom = 0, Middle = 0.5, Top = 1),
    debug = c(Yes = TRUE, No = FALSE),
    inherit.blank = c(Yes = TRUE, No = FALSE))
}

theme_setting_choices <- function(key, type) {
  if (identical(type, "logical")) return(c(Yes = TRUE, No = FALSE))
  sides <- c(Top = "top", Right = "right", Bottom = "bottom", Left = "left")
  direction <- c(Horizontal = "horizontal", Vertical = "vertical")
  reference <- c(Panel = "panel", Plot = "plot")
  switch(key,
    legend.position = c(sides, Inside = "inside", Hidden = "none"),
    legend.title.position = sides,
    legend.text.position = sides,
    legend.direction = direction,
    legend.box = direction,
    legend.box.just = c(sides, Center = "center"),
    legend.location = reference,
    plot.title.position = reference,
    plot.caption.position = reference,
    plot.tag.location = c(reference, Margin = "margin"),
    plot.tag.position = c(sides, `Top left` = "topleft", `Top right` = "topright",
                          `Bottom left` = "bottomleft", `Bottom right` = "bottomright"),
    strip.clip = c(On = "on", Off = "off", `Follow viewport` = "inherit"),
    strip.placement = c(Inside = "inside", Outside = "outside"))
}

theme_choice_control <- function(id, label, choices, value) {
  values <- c(Inherited = "NULL", vapply(choices, theme_value_text, character(1)))
  selected <- if (nzchar(value)) value else "NULL"
  if (!selected %in% values) values <- c(values, stats::setNames(selected, paste("Current:", selected)))
  editor_select(id, label, choices = values, selected = selected, width = "100%")
}

theme_property_control <- function(field, value, families) {
  labels <- c(family = "Font family (family)", face = "Font style (face)",
              hjust = "Horizontal alignment (hjust)", vjust = "Vertical alignment (vjust)",
              inherit.blank = "Hide if parent is hidden (inherit.blank)")
  examples <- c(colour = 'e.g. "navy"', fill = 'e.g. "white"', size = "e.g. 14",
                angle = "e.g. 45", linewidth = "e.g. 0.5", lineheight = "e.g. 1.1",
                margin = "e.g. margin(4, 0, 0, 0)")
  id <- paste0("custom_theme_property_", field)
  label <- if (field %in% names(labels)) labels[[field]] else field
  choices <- theme_property_choices(field, families)
  if (length(choices)) return(theme_choice_control(id, label, choices, value))
  placeholder <- if (field %in% names(examples)) examples[[field]] else "Inherited"
  shiny::textInput(id, label, value, placeholder = placeholder)
}

set_custom_theme_element <- function(plot, key, value) {
  spec <- ggplot2::get_element_tree()[[key]]
  if (is.null(spec)) stop("Choose a theme element.")
  if (!is.null(value) && !inherits(value, theme_element_class(spec)) &&
      !(inherits(spec$class, "S7_class") && inherits(value, "element_blank")))
    stop("This element expects ", paste(theme_element_class(spec), collapse = " or "), ".")
  if (is.null(plot$theme)) plot$theme <- ggplot2::theme()
  plot$theme[[key]] <- value
  plot
}

custom_theme_controls <- function() {
  shiny::tags$details(class = "editor-details",
    shiny::tags$summary("Custom theme"),
    shiny::p(class = "editor-help",
      "Start with the current theme. Search for an element by its ggplot2 name, such as plot.title or panel.grid.major."),
    editor_select("custom_theme_element", "Theme element", custom_theme_groups(), selected = "plot.title", width = "100%"),
    shiny::uiOutput("custom_theme_fields"),
    shiny::tags$div(class = "editor-actions editor-custom-theme-actions",
      shiny::actionButton("apply_custom_theme", "Apply element", class = "btn-primary"),
      shiny::actionButton("inherit_custom_theme", "Inherit element")),
    editor_status("custom_theme_status"),
    editor_actions(shiny::downloadButton("download_custom_theme", "Download theme")),
    shiny::p(class = "editor-help", "Reuse the downloaded theme in R with plot + readRDS(\"ggfigurestudio-theme.rds\").")
  )
}

bind_custom_theme <- function(input, output, edited_plot, current, grid_visibility) {
  font_families <- shiny::reactive(editor_font_families())
  state <- shiny::reactive({
    key <- shiny::req(input$custom_theme_element)
    spec <- shiny::req(ggplot2::get_element_tree()[[key]])
    type <- theme_element_class(spec)
    functions <- theme_value_functions()
    constructor <- if (length(type) == 1L && startsWith(type, "element_")) functions[[type]]
    value <- edited_plot()$theme[[key]]
    list(key = key, spec = spec, type = type, constructor = constructor, value = value,
         choices = theme_setting_choices(key, type))
  })
  status <- shiny::reactiveVal("")
  output$custom_theme_status <- shiny::renderText(status())
  output$custom_theme_fields <- shiny::renderUI({
    s <- state()
    inherit <- s$spec$inherit
    graphical <- length(s$constructor) > 0L
    example <- if (graphical) paste0(s$type, "(...)")
      else if ("margin" %in% s$type || "ggplot2::margin" %in% s$type) "margin(6, 6, 6, 6)"
      else if ("unit" %in% s$type) 'unit(4, "mm")'
      else if ("character" %in% s$type) '"bottom"'
      else if ("logical" %in% s$type) "TRUE" else "0.5 or c(0.8, 0.2)"
    fields <- if (graphical) theme_element_properties(s$constructor)
    valid <- graphical && inherits(s$value, s$type)
    field_values <- if (valid)
      stats::setNames(vapply(fields, function(field) theme_value_text(s$value[[field]]), character(1)), fields)
    text <- if (!graphical) theme_value_text(s$value)
      else if (inherits(s$value, "element_blank")) "element_blank()"
      else if (length(field_values)) {
        explicit <- field_values[nzchar(field_values)]
        paste0(s$type, "(", paste(paste(names(explicit), explicit, sep = " = "), collapse = ", "), ")")
      } else ""
    value_control <- shiny::textInput("custom_theme_value", "R value", text, placeholder = example, width = "100%")
    shiny::tagList(
      if (length(inherit)) shiny::p(class = "editor-help", paste("Inherits from", paste(inherit, collapse = ", "))),
      if (inherits(s$value, "element_blank")) shiny::p(class = "editor-help", "This element is hidden."),
      if (graphical) shiny::tagList(
        shiny::checkboxInput("custom_theme_code", "Use a full R value", FALSE),
        shiny::conditionalPanel("!input.custom_theme_code",
          shiny::p(class = "editor-help", 'Choose Inherited or leave a field blank to use the parent setting. In text fields, put colour names and other text in quotes, such as "navy".'),
          editor_fields(lapply(fields, function(field) {
            value <- if (valid) field_values[[field]] else ""
            families <- if (field == "family") font_families() else character()
            theme_property_control(field, value, families)
          }))),
        shiny::conditionalPanel("input.custom_theme_code", value_control),
        shiny::tags$div(class = "editor-actions editor-custom-theme-hide",
          shiny::actionButton("hide_custom_theme", "Hide element"))
      ) else if (length(s$choices)) shiny::tagList(
        shiny::checkboxInput("custom_theme_setting_code", "Use a full R value", FALSE),
        shiny::conditionalPanel("!input.custom_theme_setting_code",
          theme_choice_control("custom_theme_choice", "Value", s$choices, text)),
        shiny::conditionalPanel("input.custom_theme_setting_code", value_control)
      ) else value_control)
  })
  change <- function(mode) {
    tryCatch({
      s <- state()
      value <- switch(mode,
        inherit = NULL,
        hide = ggplot2::element_blank(),
        apply = {
          if (length(s$choices) && !isTRUE(input$custom_theme_setting_code)) {
            read_theme_value(input$custom_theme_choice)
          } else if (is.null(s$constructor) || isTRUE(input$custom_theme_code)) {
            read_theme_value(input$custom_theme_value)
          } else {
            properties <- list()
            for (field in theme_element_properties(s$constructor)) {
              text <- input[[paste0("custom_theme_property_", field)]]
              if (length(text) && nzchar(trimws(text))) properties[field] <- list(read_theme_value(text))
            }
            do.call(s$constructor, properties)
          }
        })
      trial <- current()
      trial$plot <- set_custom_theme_element(trial$plot, s$key, value)
      if (identical(s$key, "panel.grid")) trial$grid_visibility <- NULL
      else if (grepl("^panel\\.grid\\.(major|minor)($|\\.)", s$key)) {
        type <- sub("^panel\\.grid\\.(major|minor).*", "\\1", s$key)
        trial$grid_visibility[[type]] <- NULL
      }
      tick_preview(trial)
      edited_plot(trial$plot)
      grid_visibility(trial$grid_visibility)
      status(paste("Updated", s$key))
    }, error = function(e) status(paste("Could not update the theme.", conditionMessage(e))))
  }
  shiny::observeEvent(input$apply_custom_theme, change("apply"))
  shiny::observeEvent(input$inherit_custom_theme, change("inherit"))
  shiny::observeEvent(input$hide_custom_theme, change("hide"))
  output$download_custom_theme <- shiny::downloadHandler(
    filename = function() "ggfigurestudio-theme.rds",
    content = function(file) {
      project <- current()
      plot <- apply_grid_visibility(project$plot, project$grid_visibility)
      saveRDS(ggplot2::theme_get() + plot$theme, file)
    })
}
