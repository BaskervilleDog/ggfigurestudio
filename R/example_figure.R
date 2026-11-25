# A compact fixture for every family of movable element in the editor.
example_figure <- function() {
  samples <- as.data.frame(datasets::CO2)
  means <- stats::aggregate(uptake ~ conc + Treatment + Type, samples, mean)
  labels <- subset(means, conc == 675 & Type == "Quebec")
  plain <- subset(labels, Treatment == "nonchilled")
  boxed <- subset(labels, Treatment == "chilled")

  ggplot2::ggplot(means, ggplot2::aes(conc, uptake, colour = Type)) +
    ggplot2::geom_line(ggplot2::aes(group = Type), linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(fill = Type), shape = 21, size = 2.8) +
    ggplot2::geom_text(data = plain, label = "Quebec mean", nudge_y = 3,
                       colour = "grey20", size = 3, show.legend = FALSE) +
    ggplot2::geom_label(data = boxed, label = "Quebec mean", nudge_y = 3,
                        colour = "grey20", size = 3, show.legend = FALSE) +
    ggplot2::facet_wrap(~Treatment, nrow = 1) +
    ggplot2::scale_x_continuous(
      breaks = c(100, 400, 700, 1000),
      sec.axis = ggplot2::sec_axis(~ . / 10000, name = "CO2 (%)",
                                   breaks = c(0.01, 0.04, 0.07, 0.10))) +
    ggplot2::scale_y_continuous(
      limits = c(0, 50), breaks = seq(0, 50, 10),
      sec.axis = ggplot2::sec_axis(~ . * 0.06, name = "Uptake (mmol/m2/min)",
                                   breaks = 0:3)) +
    ggplot2::scale_colour_manual(
      aesthetics = c("colour", "fill"),
      values = c(Quebec = "#007C78", Mississippi = "#C65D21")) +
    ggplot2::labs(title = "Carbon dioxide uptake in grass",
                  subtitle = "Mean uptake by plant origin under two chilling treatments",
                  x = "CO2 concentration (ppm)", y = "Uptake (umol/m2/s)",
                  colour = "Plant origin", fill = "Plant origin", tag = "A",
                  caption = "Source: Carbon Dioxide Uptake in Grass Plants (R datasets)") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(10, 14, 12, 12))
}
