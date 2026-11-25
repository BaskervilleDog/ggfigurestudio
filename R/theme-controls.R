theme_presets <- function() {
  c("Grey" = "theme_grey", "Black and white" = "theme_bw",
    "Line drawing" = "theme_linedraw", "Light" = "theme_light", "Dark" = "theme_dark",
    "Minimal" = "theme_minimal", "Classic" = "theme_classic", "Void" = "theme_void")
}

theme_groups <- function() {
  list(
    ggplot2 = theme_presets(),
    cowplot = c("Cowplot" = "cowplot:theme_cowplot",
                "Minimal grid" = "cowplot:theme_minimal_grid"),
    ggpubr = c("Publication" = "ggpubr:theme_pubr"),
    ggthemes = c("The Economist" = "ggthemes:theme_economist",
                 "FiveThirtyEight" = "ggthemes:theme_fivethirtyeight",
                 "Tufte" = "ggthemes:theme_tufte", "Stata" = "ggthemes:theme_stata",
                 "Solarized" = "ggthemes:theme_solarized")
  )
}

theme_controls <- function() {
  editor_group("Theme",
    editor_select("theme_preset", "Plot theme",
                  c(list("Starting theme" = c("Theme at opening" = "original")), theme_groups())),
    shiny::p(class = "editor-help",
      "A preset replaces the plot's theme styling. Choose Theme at opening to restore it. Package themes require the package shown in the list."),
    editor_actions(shiny::actionButton("apply_theme", "Apply theme", class = "btn-primary")),
    editor_status("theme_status")
  )
}

set_plot_theme <- function(plot, preset, original_theme) {
  if (identical(preset, "original")) {
    plot$theme <- original_theme
    return(plot)
  }
  groups <- theme_groups()
  package <- names(Filter(function(choices) preset %in% choices, groups))
  if (!length(package)) stop("Select a theme from the list.")
  if (!requireNamespace(package, quietly = TRUE))
    stop("Install the ", package, " package in R to use this theme.", call. = FALSE)
  plot + getExportedValue(package, sub("^[^:]+:", "", preset))()
}

bind_theme_controls <- function(input, output, edited_plot, current, original_theme) {
  status <- shiny::reactiveVal("")
  output$theme_status <- shiny::renderText(status())
  shiny::observeEvent(input$apply_theme, {
    preset <- shiny::req(input$theme_preset)
    tryCatch({
      trial <- current()
      trial$plot <- set_plot_theme(trial$plot, preset, original_theme())
      tick_preview(trial)
      edited_plot(trial$plot)
      status("Theme applied.")
    }, error = function(e) status(paste("Theme could not be applied.", conditionMessage(e))))
  })
}
