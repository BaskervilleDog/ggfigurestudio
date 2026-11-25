editor_group <- function(title, ..., description = NULL) {
  shiny::tags$fieldset(class = "editor-group",
    shiny::tags$legend(title),
    if (length(description)) shiny::p(class = "editor-help", description),
    ...)
}

editor_fields <- function(...) shiny::tags$div(class = "editor-fields", ...)

editor_actions <- function(...) shiny::tags$div(class = "editor-actions", ...)

editor_select <- function(inputId, ..., width = NULL) {
  shiny::selectizeInput(inputId, ..., width = width, options = list(
    dropdownParent = "body",
    dropdownClass = "editor-select-menu",
    onInitialize = I("function() { this.positionDropdown = window.ggfigurestudioPositionMenu; }")
  ))
}

editor_font_families <- function() {
  installed <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
  families <- sort(setdiff(installed, c("", "sans", "serif", "mono")))
  c(`Sans serif` = "sans", Serif = "serif", Monospace = "mono",
    stats::setNames(families, families))
}

editor_font_faces <- c(Plain = "plain", Bold = "bold", Italic = "italic", `Bold italic` = "bold.italic")

editor_status <- function(id) {
  shiny::textOutput(id, container = function(...) {
    shiny::tags$div(..., class = "editor-status", role = "status", `aria-live` = "polite")
  })
}

editor_ui <- function(project, composition = NULL) {
  shiny::fluidPage(
    title = "GG Figure Studio",
    shiny::includeCSS(system.file("editor.css", package = "ggfigurestudio")),
    shiny::includeScript(system.file("appearance.js", package = "ggfigurestudio")),
    shiny::includeScript(system.file("dropdowns.js", package = "ggfigurestudio")),
    shiny::includeScript(system.file("drag.js", package = "ggfigurestudio")),
    shiny::tags$div(class = "editor-shell",
      shiny::tags$header(class = "editor-header",
        shiny::tags$h1("GG Figure Studio"),
        editor_actions(
          shiny::tags$label(class = "editor-night-toggle",
            shiny::tags$input(id = "editor-night-mode", type = "checkbox"), "Night mode"),
          shiny::actionButton("done", "Return edited project", class = "btn-primary"))),
      shiny::tags$main(class = "editor-layout",
        shiny::tags$aside(class = "editor-sidebar", `aria-label` = "Editing controls",
          shiny::tabsetPanel(id = "editor_tab", selected = "figure",
            shiny::tabPanel("Figure", value = "figure",
              shiny::h2("Figure settings"),
              shiny::p(class = "editor-help", "Adjust the overall appearance of the plot."),
              if (!is.null(composition)) composition_controls(composition),
              editor_group("Plot size",
                editor_fields(
                  shiny::numericInput("plot_width_percent", "Width (%)", plot_size(project)[["width"]], min = 1, max = 100),
                  shiny::numericInput("plot_height_percent", "Height (%)", plot_size(project)[["height"]], min = 1, max = 100)),
                editor_actions(shiny::actionButton("apply_plot_size", "Apply plot size", class = "btn-primary")),
                editor_status("plot_size_status"),
                description = "Size of the centred plot within the image. Set both values to 100% to fill the image."),
              theme_controls(), shiny::uiOutput("grid_controls"), custom_theme_controls(),
              editor_group("Label placement",
                shiny::checkboxInput("connect_labels", "Connect moved labels",
                                     isTRUE(project$connect_labels)),
                shiny::conditionalPanel("input.connect_labels",
                  editor_select("label_anchor_mode", "Connect to", label_anchor_choices,
                                selected = label_anchor_mode(project)),
                  shiny::p(class = "editor-help", "Previous position uses the spot where the label was just before its latest move.")),
                description = "Drag or nudge text in the plot panel to make room for crowded labels.")),
            shiny::tabPanel("Layers", value = "layers",
              shiny::h2("Manage layers"),
              shiny::p(class = "editor-help",
                "Add an annotation, switch how an existing layer draws its data, or remove a layer."),
              layer_controls(project$plot, !identical(project$annotation_all_facets, FALSE))),
            shiny::tabPanel("Selection", value = "selection",
              shiny::uiOutput("element_options"), editor_status("element_status")),
            shiny::tabPanel("Examples", value = "examples", example_controls()))),
        shiny::tags$section(class = "editor-preview", `aria-labelledby` = "editor-preview-title",
          shiny::tags$div(class = "editor-preview-toolbar",
            shiny::tags$div(
              shiny::tags$h2(id = "editor-preview-title", "Preview"),
              shiny::textOutput("preview_figure", container = function(...) {
                shiny::p(..., class = "editor-help")
              })),
            shiny::numericInput("nudge", "Arrow-key nudge (mm)", project$nudge,
                                min = 0.001, step = NA, width = "168px")),
          shiny::p(class = "editor-preview-help",
            "Drag movable elements or use the arrow keys. Ctrl+Z undoes a move or layer deletion."),
          editor_status("history_status"),
          shiny::tags$div(class = "editor-preview-scroll",
            if (is.null(composition)) shiny::uiOutput("preview") else shiny::tagList(
              shiny::conditionalPanel("!input.show_composition", shiny::uiOutput("preview")),
              shiny::conditionalPanel("input.show_composition", shiny::uiOutput("combined_preview")))),
          editor_status("issues")))
    )
  )
}
