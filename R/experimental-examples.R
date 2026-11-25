experimental_examples <- function() {
  data.frame(
    id = c("anova_one_way", "anova_interaction", "regression_linear", "regression_quadratic",
           "regression_logistic", "regression_exponential", "regression_residuals",
           "randomized_blocks", "paired_measurements"),
    plot = c("One-way ANOVA with mean intervals", "Two-way ANOVA interaction",
             "Linear regression with confidence band", "Quadratic regression",
             "Logistic regression", "Exponential decay regression", "Regression residuals",
             "Randomized block experiment", "Paired before-and-after measurements"),
    try_editing = c(
      "Style a treatment's observations or select the ANOVA equation to edit its notation.",
      "Select a line to edit one group's appearance and check its legend.",
      "Style the confidence band or move and edit the fitted equation.",
      "Change the fitted curve's line type and adjust the numeric axis ticks.",
      "Recolour the fitted curve or edit the probability axis.",
      "Edit the curve's colour and the time-axis title.",
      "Style the zero reference line separately from the residual points.",
      "Select a block's line or change the palette of the points.",
      "Select a unit's connector or recolour the before and after points."
    ),
    category = rep(c("ANOVA", "Regression", "Experimental designs"), c(2L, 5L, 2L)),
    stringsAsFactors = FALSE
  )
}

experimental_means <- function(data, groups, model) {
  means <- stats::aggregate(stats::reformulate(groups, "response"), data, mean)
  counts <- stats::aggregate(stats::reformulate(groups, "response"), data, length)
  df <- stats::df.residual(model)
  pooled_sd <- sqrt(sum(stats::residuals(model)^2) / df)
  error <- stats::qt(0.975, df) * pooled_sd / sqrt(counts$response)
  means$lower <- means$response - error
  means$upper <- means$response + error
  means
}

gallery_anova_one_way <- function() {
  data <- expand.grid(treatment = c("Control", "Low", "Medium", "High"), replicate = 1:10)
  data$response <- c(14, 17, 21, 19)[match(data$treatment, levels(data$treatment))] +
    withr::with_seed(61, stats::rnorm(nrow(data), sd = 1.7))
  means <- experimental_means(data, "treatment", stats::aov(response ~ treatment, data))
  plot <- ggplot2::ggplot(data, ggplot2::aes(treatment, response, colour = treatment)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.16, height = 0, seed = 61),
                        alpha = 0.5, size = 1.8) +
    ggplot2::geom_segment(data = means, ggplot2::aes(y = lower, yend = upper, xend = treatment),
                          linewidth = 0.85, show.legend = FALSE) +
    ggplot2::geom_point(data = means, size = 3.1, shape = 18, show.legend = FALSE) +
    ggplot2::labs(title = "Response by treatment", subtitle = "Means and 95% confidence intervals",
                  x = "Treatment", y = "Response", colour = "Treatment",
                  caption = "Simulated data. Intervals use the pooled one-way ANOVA error.")
  shape_plot(plot, list(type = "text", x = "Low", y = as.character(max(data$response) + 1),
    text = "Y[ij] == mu + tau[i] + epsilon[ij]", text_format = "plotmath",
    text_colour = "#185884", text_size = 11))
}

gallery_anova_interaction <- function() {
  data <- expand.grid(fertilizer = c("None", "Added"), watering = c("Low", "Regular"), replicate = 1:12)
  data$response <- 14 + 2 * (data$fertilizer == "Added") + 3 * (data$watering == "Regular") +
    5 * (data$fertilizer == "Added" & data$watering == "Regular") +
    withr::with_seed(49, stats::rnorm(nrow(data), sd = 1.8))
  means <- experimental_means(data, c("fertilizer", "watering"),
                              stats::aov(response ~ fertilizer * watering, data))
  ggplot2::ggplot(means, ggplot2::aes(fertilizer, response, colour = watering, group = watering)) +
    ggplot2::geom_segment(ggplot2::aes(y = lower, yend = upper, xend = fertilizer),
                          linewidth = 0.8, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(title = "Fertilizer and watering interaction",
                  subtitle = "Group means and 95% confidence intervals",
                  x = "Fertilizer", y = "Response", colour = "Watering",
                  caption = "Simulated balanced factorial experiment. Intervals use pooled ANOVA error.")
}

gallery_regression_data <- function(quadratic = FALSE) {
  dose <- seq(0, 10, length.out = 40)
  response <- if (quadratic) 15 + 4 * dose - 0.4 * dose^2 else 7 + 1.8 * dose
  data.frame(dose = dose, response = response + withr::with_seed(42, stats::rnorm(length(dose), sd = 1.7)))
}

gallery_regression <- function(type) {
  data <- gallery_regression_data(type == "quadratic")
  title <- switch(type, linear = "Linear regression", quadratic = "Quadratic regression",
                  logistic = "Logistic regression", exponential = "Exponential decay regression")
  x_label <- "Dose"
  y_label <- "Response"
  if (type == "logistic") {
    data <- data.frame(dose = rep(0:10, each = 12))
    data$response <- withr::with_seed(87, stats::rbinom(length(data$dose), 1, stats::plogis(data$dose - 5)))
    model <- stats::glm(response ~ dose, data, family = stats::binomial())
    y_label <- "Outcome (0 or 1) / fitted probability"
  } else if (type == "exponential") {
    data$response <- 28 * exp(-0.3 * data$dose) + withr::with_seed(32, stats::rnorm(nrow(data), sd = 0.6))
    model <- stats::nls(response ~ initial * exp(-rate * dose), data, start = list(initial = 28, rate = 0.3))
    x_label <- "Time"
  } else {
    formula <- if (type == "quadratic") response ~ dose + I(dose^2) else response ~ dose
    model <- stats::lm(formula, data)
  }
  curve <- data.frame(dose = seq(min(data$dose), max(data$dose), length.out = 150))
  if (type == "linear") {
    interval <- stats::predict(model, curve, interval = "confidence")
    curve$response <- interval[, "fit"]
    curve$lower <- interval[, "lwr"]
    curve$upper <- interval[, "upr"]
  } else {
    curve$response <- if (type == "logistic") stats::predict(model, curve, type = "response")
      else stats::predict(model, curve)
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(dose, response)) +
    (if (type == "linear") ggplot2::geom_ribbon(data = curve, ggplot2::aes(ymin = lower, ymax = upper),
                                               fill = "#B7D9F0", alpha = 0.7)) +
    ggplot2::geom_point(position = if (type == "logistic")
      ggplot2::position_jitter(width = 0.18, height = 0, seed = 87) else "identity",
      size = 1.8, alpha = 0.5) +
    ggplot2::geom_line(data = curve, colour = "#185884", linewidth = 0.9) +
    ggplot2::labs(title = title, x = x_label, y = y_label,
      subtitle = if (type == "linear") "Fitted mean with a pointwise 95% confidence band" else "Observed values and fitted response",
      caption = "Simulated data")
  if (type != "linear") return(plot)
  coefficients <- stats::coef(model)
  equation <- sprintf("hat(y) == %.2f %s %.2f*x", coefficients[[1L]],
                      if (coefficients[[2L]] < 0) "-" else "+", abs(coefficients[[2L]]))
  shape_plot(plot, list(type = "text", x = "2.5", y = as.character(max(data$response) - 1),
    text = equation, text_format = "plotmath", text_colour = "#185884", text_size = 11))
}

gallery_regression_residuals <- function() {
  data <- gallery_regression_data(quadratic = TRUE)
  model <- stats::lm(response ~ dose, data)
  diagnostics <- data.frame(fitted = stats::fitted(model), residual = stats::residuals(model))
  ggplot2::ggplot(diagnostics, ggplot2::aes(fitted, residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey42") +
    ggplot2::geom_point(colour = "#185884", size = 2) +
    ggplot2::labs(title = "Residuals versus fitted values",
                  subtitle = "A straight-line fit leaves curvature in this example",
                  x = "Fitted response", y = "Residual", caption = "Simulated data")
}

gallery_randomized_blocks <- function() {
  data <- expand.grid(treatment = c("Control", "A", "B"), block = factor(paste("Block", 1:5)))
  data$response <- 18 + c(0, 4, 7)[match(data$treatment, levels(data$treatment))] +
    c(-5, -2, 0, 2, 5)[as.integer(data$block)] +
    withr::with_seed(92, stats::rnorm(nrow(data), sd = 0.9))
  ggplot2::ggplot(data, ggplot2::aes(treatment, response, group = block, colour = block)) +
    ggplot2::geom_line(linewidth = 0.65) + ggplot2::geom_point(size = 2.6) +
    ggplot2::labs(title = "Treatment response within blocks",
      subtitle = "Each block contains one experimental unit for each treatment",
      x = "Treatment", y = "Response", colour = "Block", caption = "Simulated randomized block experiment")
}

gallery_paired_measurements <- function() {
  data <- expand.grid(time = c("Before", "After"), unit = factor(seq_len(12)))
  starting <- withr::with_seed(21, stats::rnorm(12, mean = 20, sd = 3))
  change <- withr::with_seed(22, stats::rnorm(12, mean = 2, sd = 1.8))
  data$response <- starting[as.integer(data$unit)] + change[as.integer(data$unit)] * (data$time == "After")
  ggplot2::ggplot(data, ggplot2::aes(time, response)) +
    ggplot2::geom_line(ggplot2::aes(group = unit), colour = "grey65", linewidth = 0.65) +
    ggplot2::geom_point(ggplot2::aes(colour = time), size = 2.4) +
    ggplot2::labs(title = "Paired measurements by experimental unit",
      subtitle = "Each line connects two observations on the same unit",
      x = NULL, y = "Response", colour = "Time", caption = "Simulated data")
}
