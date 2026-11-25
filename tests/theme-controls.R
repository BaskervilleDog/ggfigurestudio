library(ggfigurestudio)
library(ggplot2)

opening <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point() + theme_minimal() + theme(legend.position = "top")
plot <- opening + geom_hline(yintercept = 3)
for (preset in ggfigurestudio:::theme_presets()) {
  themed <- ggfigurestudio:::set_plot_theme(plot, preset, opening$theme)
  stopifnot(identical(themed$theme, getExportedValue("ggplot2", preset)()),
            identical(themed$layers, plot$layers))
  ggfigurestudio:::tick_preview(figure_project(themed))
}
groups <- ggfigurestudio:::theme_groups()
for (package in setdiff(names(groups), "ggplot2")) {
  if (!requireNamespace(package, quietly = TRUE)) next
  for (preset in groups[[package]]) {
    themed <- ggfigurestudio:::set_plot_theme(plot, preset, opening$theme)
    expected <- getExportedValue(package, sub("^[^:]+:", "", preset))()
    stopifnot(identical(themed$theme, expected), identical(themed$layers, plot$layers))
    ggfigurestudio:::tick_preview(figure_project(themed))
  }
}
restored <- ggfigurestudio:::set_plot_theme(themed, "original", opening$theme)
stopifnot(identical(restored$theme, opening$theme),
          identical(restored$layers, plot$layers))
