gallery_examples <- function() {
  original <- data.frame(
    id = c("scatter", "co2", "facet_grid", "time_ribbon", "violin", "lollipop",
           "stacked_bar", "heatmap", "log_scatter", "polar_bar"),
    plot = c("Annotated scatter", "Faceted CO2 curves", "Free-scale facet grid",
             "Date series with ribbons", "Violin with boxplots and points",
             "Paired lollipop", "Stacked bars with counts", "Heatmap with values",
             "Log-scale scatter", "Polar bars"),
    try_editing = c(
      "Select a point group, change its palette, and move a boxed label.",
      "Rename a facet, set its tick positions, then edit a line and its legend key.",
      "Edit ticks in one panel and rename a row or column strip.",
      "Click inside a ribbon to change its fill or opacity, then style the line separately.",
      "Click a violin flank, then the box inside it, to style the density and boxplot separately.",
      "Click a gray connector to edit its line style, then edit a dot separately.",
      "Style one or all coloured segments. Turn on label connectors under Figure, then drag a count label.",
      "Style one tile, then move a cell label and rename the colour-bar title.",
      "Style a point group and edit the x-axis title. Check the log spacing.",
      "Edit the plot or legend text and export. Check the polar bars render."),
    stringsAsFactors = FALSE
  )
  additions <- experimental_examples()
  original$category <- "General charts"
  rbind(original, additions)
}

gallery_example <- function(id) {
  catalog <- gallery_examples()
  if (is.numeric(id) && length(id) == 1L && is.finite(id) &&
      id %in% seq_len(nrow(catalog))) id <- catalog$id[id]
  if (!is.character(id) || length(id) != 1L || !id %in% catalog$id)
    stop("Choose an example number or ID from gallery_examples().", call. = FALSE)
  plot <- switch(id,
    scatter = gallery_scatter(),
    co2 = example_figure(),
    facet_grid = gallery_facet_grid(),
    time_ribbon = gallery_time_ribbon(),
    violin = gallery_violin(),
    lollipop = gallery_lollipop(),
    stacked_bar = gallery_stacked_bar(),
    heatmap = gallery_heatmap(),
    log_scatter = gallery_log_scatter(),
    polar_bar = gallery_stacked_bar() + ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(title = "Diamond cuts in polar coordinates",
                    subtitle = "One ring per cut, segmented by colour grade"),
    anova_one_way = gallery_anova_one_way(),
    anova_interaction = gallery_anova_interaction(),
    regression_linear = gallery_regression("linear"),
    regression_quadratic = gallery_regression("quadratic"),
    regression_logistic = gallery_regression("logistic"),
    regression_exponential = gallery_regression("exponential"),
    regression_residuals = gallery_regression_residuals(),
    randomized_blocks = gallery_randomized_blocks(),
    paired_measurements = gallery_paired_measurements()
  )
  if (id %in% c("co2", "facet_grid")) return(plot)
  plot + ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom",
                   plot.margin = ggplot2::margin(10, 14, 10, 10))
}

gallery_scatter <- function() {
  cars <- transform(datasets::mtcars, model = rownames(datasets::mtcars),
                    cylinders = factor(cyl))
  labels <- cars[c("Toyota Corolla", "Ferrari Dino", "Lincoln Continental"), ]
  ggplot2::ggplot(cars, ggplot2::aes(wt, mpg, colour = cylinders, shape = cylinders)) +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::geom_label(data = labels, ggplot2::aes(label = model), nudge_y = 1.55,
                        size = 2.8, show.legend = FALSE) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.14))) +
    ggplot2::labs(title = "Fuel economy and vehicle weight",
      subtitle = "Selected models are labelled", x = "Weight (1,000 lb)",
      y = "Fuel economy (mpg)", colour = "Cylinders", shape = "Cylinders",
      caption = "Source: mtcars")
}

gallery_facet_grid <- function() {
  cars <- transform(datasets::mtcars, transmission = factor(am, labels = c("Automatic", "Manual")),
                    cylinders = factor(cyl, labels = c("4 cylinders", "6 cylinders", "8 cylinders")))
  ggplot2::ggplot(cars, ggplot2::aes(hp, mpg, colour = factor(gear))) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::facet_grid(rows = ggplot2::vars(cylinders), cols = ggplot2::vars(transmission),
                        scales = "free_x") +
    ggplot2::labs(title = "Fuel economy by engine and transmission",
      x = "Horsepower", y = "Fuel economy (mpg)", colour = "Gears", caption = "Source: mtcars") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")
}

gallery_time_ribbon <- function() {
  temp <- data.frame(year = rep(1920:1939, each = 12), month = rep(1:12, 20),
                     temperature = as.numeric(datasets::nottem))
  temp <- subset(temp, month <= 3 | (month >= 7 & month <= 9))
  temp$season <- ifelse(temp$month <= 3, "Jan to Mar", "Jul to Sep")
  periods <- split(temp, list(temp$year, temp$season), drop = TRUE)
  data <- do.call(rbind, lapply(periods, function(period) {
    data.frame(date = as.Date(sprintf("%d-%02d-01", period$year[1], period$month[2])),
      season = period$season[1], mean = mean(period$temperature),
      low = min(period$temperature), high = max(period$temperature))
  }))
  ggplot2::ggplot(data, ggplot2::aes(date, mean, colour = season)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high, fill = season),
                         alpha = 0.2, colour = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ggplot2::labs(title = "Seasonal temperatures at Nottingham Castle",
      subtitle = "Bands show the range of monthly averages within each period",
      x = "Year", y = "Temperature (F)", colour = "Months", fill = "Months",
      caption = "Source: nottem, 1920 to 1939")
}

gallery_violin <- function() {
  counts <- aggregate(Petal.Length ~ Species, datasets::iris, length)
  ggplot2::ggplot(datasets::iris, ggplot2::aes(Species, Petal.Length, fill = Species)) +
    ggplot2::geom_violin(alpha = 0.55, width = 0.85) +
    ggplot2::geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(colour = Species),
      position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 19),
      size = 1.2, alpha = 0.6) +
    ggplot2::geom_text(data = counts, ggplot2::aes(label = paste0("n = ", Petal.Length)),
                        y = 7.15, colour = "grey25", size = 3.1) +
    ggplot2::labs(title = "Petal lengths by iris species",
      subtitle = "Violin shapes, boxplots, and individual measurements",
      x = "Species", y = "Petal length (cm)", colour = "Species", fill = "Species",
      caption = "Source: iris")
}

gallery_lollipop <- function() {
  means <- aggregate(mpg ~ cyl + am, datasets::mtcars, mean)
  means$transmission <- factor(means$am, labels = c("Automatic", "Manual"))
  means$cylinders <- factor(means$cyl)
  stems <- reshape(means[c("cylinders", "am", "mpg")], idvar = "cylinders",
                   timevar = "am", direction = "wide")
  ggplot2::ggplot(means, ggplot2::aes(mpg, cylinders, colour = transmission)) +
    ggplot2::geom_segment(data = stems,
      ggplot2::aes(x = mpg.0, xend = mpg.1, y = cylinders, yend = cylinders),
      inherit.aes = FALSE, linewidth = 1, colour = "grey68") +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", mpg)),
                        nudge_y = 0.22, size = 3.1, show.legend = FALSE) +
    ggplot2::labs(title = "Mean fuel economy by transmission",
      x = "Mean fuel economy (mpg)", y = "Cylinders", colour = "Transmission",
      caption = "Source: mtcars")
}

gallery_stacked_bar <- function() {
  counts <- as.data.frame(table(ggplot2::diamonds$cut, ggplot2::diamonds$color))
  names(counts) <- c("cut", "colour_grade", "count")
  counts <- subset(counts, colour_grade %in% c("D", "E", "F", "G"))
  ggplot2::ggplot(counts, ggplot2::aes(cut, count, fill = colour_grade)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = count),
      position = ggplot2::position_stack(vjust = 0.5), size = 2.8) +
    ggplot2::labs(title = "Diamond counts by cut and colour grade",
      subtitle = "Four colour grades from the diamonds dataset",
      x = "Cut", y = "Count", fill = "Colour grade", caption = "Source: ggplot2 diamonds")
}

gallery_heatmap <- function() {
  air <- subset(datasets::airquality, !is.na(Ozone))
  air$month <- factor(air$Month, levels = 5:9, labels = month.abb[5:9])
  ggplot2::ggplot(air, ggplot2::aes(Day, month, fill = Ozone)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.45) +
    ggplot2::geom_text(data = subset(air, Ozone >= 100), ggplot2::aes(label = Ozone),
                        size = 2.9, colour = "grey15") +
    ggplot2::scale_fill_gradient(low = "#F0F6F5", high = "#55A0B3") +
    ggplot2::labs(title = "Daily ozone in New York",
      subtitle = "Days with measurements of 100 ppb or more are labelled",
      x = "Day of month", y = "Month", fill = "Ozone (ppb)", caption = "Source: airquality, 1973")
}

gallery_log_scatter <- function() {
  states <- data.frame(state = datasets::state.name, region = datasets::state.region,
                      population = datasets::state.x77[, "Population"],
                      area = datasets::state.x77[, "Area"])
  labels <- subset(states, state %in% c("California", "Alaska", "Rhode Island"))
  ggplot2::ggplot(states, ggplot2::aes(population, area, colour = region)) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::geom_text(data = labels, ggplot2::aes(label = state),
                        nudge_y = 0.15, size = 3, show.legend = FALSE) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
    ggplot2::labs(title = "Population and land area of US states",
      subtitle = "Both axes use logarithmic spacing", x = "Population (thousands)",
      y = "Land area (square miles)", colour = "Region", caption = "Source: state.x77")
}
