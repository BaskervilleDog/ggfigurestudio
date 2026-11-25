breeding_examples <- function() {
  data.frame(
    id = c("field_map", "reaction_norm", "ammi", "gge", "genotype_intervals",
           "heritability", "trait_correlations", "manhattan", "gwas_qq",
           "ld_heatmap", "genomic_prediction", "statgen_field"),
    plot = c("Field layout and yield map", "Genotype-by-environment reaction norms",
             "AMMI interaction biplot", "metan GGE biplot", "Ranked genotype means",
             "Entry-mean heritability", "Phenotypic trait correlations", "GWAS Manhattan",
             "GWAS Q-Q", "Linkage disequilibrium heatmap", "Held-out genomic prediction",
             "statgenSTA trial map"),
    packages = c(rep("ggplot2", 3), "metan", rep("ggplot2", 6), "rrBLUP", "statgenSTA"),
    try_editing = c(
      "Recolour all tiles, then change one plot border or move its genotype label.",
      "Select a genotype's line and point layers separately.",
      "Restyle a genotype's marker, move a label, and edit an environment arrow.",
      "Try the points, text, and legend in a ggplot returned by metan.",
      "Edit one mean point and its replicate-range segment.",
      "Style a trait's marker on a zero-to-one scale and edit the axis ticks.",
      "Change the fill palette or override one paired-trait cell.",
      "Recolour the chromosome groups and move a peak label.",
      "Edit a point group or the null line and adjust the axis ticks.",
      "Apply a continuous palette and then change one cell.",
      "Style validation points by family and inspect the identity line.",
      "Open a trial map returned by statgenSTA and try the title, axes, and legend."),
    stringsAsFactors = FALSE
  )
}

breeding_example <- function(id) {
  catalog <- breeding_examples()
  if (is.numeric(id) && length(id) == 1L && is.finite(id) &&
      id %in% seq_len(nrow(catalog))) id <- catalog$id[id]
  if (!is.character(id) || length(id) != 1L || !id %in% catalog$id)
    stop("Choose an example number or ID from breeding_examples().", call. = FALSE)
  package <- catalog$packages[match(id, catalog$id)]
  if (!requireNamespace(package, quietly = TRUE))
    stop("This example requires ", package, ".", call. = FALSE)
  plot <- switch(id,
    field_map = breeding_field_map(),
    reaction_norm = breeding_reaction_norm(),
    ammi = breeding_ammi(),
    gge = breeding_gge(),
    genotype_intervals = breeding_genotype_intervals(),
    heritability = breeding_heritability(),
    trait_correlations = breeding_trait_correlations(),
    manhattan = breeding_manhattan(),
    gwas_qq = breeding_qq(),
    ld_heatmap = breeding_ld_heatmap(),
    genomic_prediction = breeding_prediction(),
    statgen_field = breeding_statgen_field()
  )
  plot <- plot + ggplot2::labs(caption = "Simulated data for editor testing")
  if (id %in% c("gge", "statgen_field")) return(plot)
  plot + ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(10, 14, 10, 12))
}

breeding_trials <- function() {
  withr::with_seed(431L, {
    trials <- expand.grid(genotype = sprintf("G%02d", 1:10),
                          environment = c("Coast", "Highland", "North", "Plain", "Valley"),
                          replicate = factor(1:3), KEEP.OUT.ATTRS = FALSE)
    g <- match(trials$genotype, unique(trials$genotype))
    e <- match(trials$environment, unique(trials$environment))
    performance <- stats::rnorm(10, sd = 0.65)[g]
    interaction <- outer(sin(seq_len(10) * 1.6), cos(seq_len(5))) +
      outer(cos(seq_len(10) * 0.7), sin(seq_len(5) * 1.5))
    stability <- interaction[cbind(g, e)]
    trials$yield <- 5.3 + performance + c(0.1, -0.7, 0.4, 0.8, -0.2)[e] +
      0.57 * stability + stats::rnorm(nrow(trials), sd = 0.35)
    trials$height <- 92 + 7 * performance + c(4, -5, 0, 6, -2)[e] +
      3.3 * stability + stats::rnorm(nrow(trials), sd = 4.5)
    trials$flowering <- 68 - 3.8 * performance + c(1, 4, -1, -3, 2)[e] +
      2.2 * stability + stats::rnorm(nrow(trials), sd = 2.1)
    trials
  })
}

breeding_field_data <- function() {
  withr::with_seed(612L, {
    field <- expand.grid(row = 1:6, column = 1:8)
    field$replicate <- factor((field$column - 1L) %/% 4L + 1L)
    field$genotype <- NA_character_
    genotypes <- rep(sprintf("B%02d", 1:24), 2L)
    for (block in levels(field$replicate)) {
      i <- which(field$replicate == block)
      field$genotype[i] <- sample(genotypes[i])
    }
    effects <- stats::rnorm(24L, sd = 0.5)
    field$yield <- 5.2 + effects[match(field$genotype, sprintf("B%02d", 1:24))] +
      0.15 * field$column - 0.18 * field$row + stats::rnorm(nrow(field), sd = 0.25)
    field$trial <- "North station"
    field
  })
}

breeding_field_map <- function() {
  field <- breeding_field_data()
  ggplot2::ggplot(field, ggplot2::aes(column, row, fill = yield)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = genotype), size = 2.6, colour = "grey15") +
    ggplot2::geom_vline(xintercept = 4.5, colour = "grey20", linewidth = 0.7) +
    ggplot2::scale_fill_gradient(low = "#F2E5B5", high = "#4E9B86", name = "Yield (t/ha)") +
    ggplot2::scale_y_reverse(breaks = 1:6) + ggplot2::scale_x_continuous(breaks = 1:8) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Yield across a replicated field trial",
      subtitle = "The divider separates two randomized blocks", x = "Field column", y = "Field row")
}

breeding_reaction_norm <- function() {
  trial <- breeding_trials()
  means <- stats::aggregate(yield ~ genotype + environment, trial, mean)
  means <- subset(means, genotype %in% c("G01", "G03", "G05", "G07", "G09"))
  index <- stats::aggregate(yield ~ environment, trial, mean)
  means$index <- index$yield[match(means$environment, index$environment)]
  ggplot2::ggplot(means, ggplot2::aes(index, yield, colour = genotype, group = genotype)) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_x_continuous(breaks = index$yield, labels = index$environment) +
    ggplot2::labs(title = "Genotype response across environments",
      subtitle = "Environments are ordered by their mean yield", x = "Environment",
      y = "Mean yield (t/ha)", colour = "Genotype")
}

breeding_ammi <- function() {
  means <- stats::xtabs(yield ~ genotype + environment, breeding_trials()) / 3
  interaction <- sweep(sweep(means, 1L, rowMeans(means)), 2L, colMeans(means)) + mean(means)
  fit <- svd(interaction)
  axes <- sqrt(fit$d[1:2])
  gen <- data.frame(x = fit$u[, 1] * axes[1], y = fit$u[, 2] * axes[2],
                    name = rownames(means), type = "Genotype")
  env <- data.frame(x = fit$v[, 1] * axes[1], y = fit$v[, 2] * axes[2],
                    name = colnames(means), type = "Environment")
  scores <- rbind(gen, env)
  explained <- 100 * fit$d^2 / sum(fit$d^2)
  ggplot2::ggplot(scores, ggplot2::aes(x, y, colour = type)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_segment(data = env, ggplot2::aes(x = 0, y = 0, xend = x, yend = y, group = name),
      arrow = grid::arrow(length = grid::unit(2.3, "mm")), show.legend = FALSE) +
    ggplot2::geom_point(ggplot2::aes(shape = type), size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = name), nudge_y = 0.05, size = 2.7, show.legend = FALSE) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "AMMI interaction biplot",
      subtitle = "Symmetric scores from the double-centred yield means",
      x = sprintf("IPCA1 (%.1f%%)", explained[1]), y = sprintf("IPCA2 (%.1f%%)", explained[2]),
      colour = "Score type", shape = "Score type")
}

breeding_gge <- function() {
  model <- metan::gge(breeding_trials(), environment, genotype, yield, svp = "symmetrical")
  graphics::plot(model, type = 1, repel = FALSE) +
    ggplot2::labs(title = "GGE biplot from metan")
}

breeding_genotype_intervals <- function() {
  data <- subset(breeding_trials(), environment == "North")
  groups <- split(data$yield, data$genotype)
  means <- data.frame(genotype = names(groups), mean = vapply(groups, mean, numeric(1)),
    low = vapply(groups, min, numeric(1)), high = vapply(groups, max, numeric(1)))
  means$genotype <- stats::reorder(means$genotype, means$mean)
  ggplot2::ggplot(means, ggplot2::aes(mean, genotype)) +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, yend = genotype),
                          colour = "#8294A8", linewidth = 1) +
    ggplot2::geom_point(colour = "#2C678D", size = 2.9) +
    ggplot2::labs(title = "Ranked genotype means at the North site",
      subtitle = "Segments span the observed range of three plot replicates",
      x = "Yield (t/ha)", y = "Genotype")
}

breeding_heritability <- function() {
  trials <- breeding_trials()
  traits <- c(Yield = "yield", Height = "height", Flowering = "flowering")
  e <- length(unique(trials$environment))
  r <- length(unique(trials$replicate))
  estimates <- vapply(traits, function(trait) {
    trials$response <- trials[[trait]]
    table <- stats::anova(stats::lm(response ~ genotype * environment, data = trials))
    ms <- stats::setNames(table[["Mean Sq"]], rownames(table))
    g <- max(0, (ms[["genotype"]] - ms[["genotype:environment"]]) / (e * r))
    gx <- max(0, (ms[["genotype:environment"]] - ms[["Residuals"]]) / r)
    g / (g + gx / e + ms[["Residuals"]] / (e * r))
  }, numeric(1))
  data <- data.frame(trait = names(estimates), estimate = unname(estimates))
  ggplot2::ggplot(data, ggplot2::aes(estimate, trait, colour = trait)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = estimate, yend = trait), linewidth = 1) +
    ggplot2::geom_point(size = 3.4, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", estimate)), nudge_y = 0.15,
                        size = 3, show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "Broad-sense heritability of entry means",
      subtitle = "Balanced trial illustration with five environments and three replicates",
      x = "Entry-mean H-squared", y = NULL, colour = "Trait")
}

breeding_trait_correlations <- function() {
  data <- stats::aggregate(cbind(yield, height, flowering) ~ genotype + environment,
                            breeding_trials(), mean)
  names(data)[3:5] <- c("Yield", "Height", "Flowering")
  correlations <- stats::cor(data[3:5])
  cells <- as.data.frame(as.table(correlations), responseName = "correlation")
  ggplot2::ggplot(cells, ggplot2::aes(Var1, Var2, fill = correlation)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 3.5) +
    ggplot2::scale_fill_gradient2(low = "#3B73A0", mid = "#F4F3EE", high = "#AE4662",
                                  limits = c(-1, 1), name = "Pearson r") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Phenotypic correlations among traits",
      subtitle = "Calculated from genotype-by-environment means", x = NULL, y = NULL)
}

breeding_gwas <- function() {
  withr::with_seed(1902L, {
    n <- 100L
    grid <- expand.grid(position = seq_len(n), chromosome = factor(1:5))
    grid$position <- grid$position / 2
    grid$marker <- sprintf("M%03d", seq_len(nrow(grid)))
    grid$p <- stats::runif(nrow(grid))
    grid$p[c(50, 238, 245, 422)] <- c(1.7e-5, 3e-8, 8e-7, 2e-6)
    grid
  })
}

breeding_manhattan <- function() {
  data <- breeding_gwas()
  data$x <- (as.integer(data$chromosome) - 1) * 55 + data$position
  peaks <- data[head(order(data$p), 3L), ]
  ggplot2::ggplot(data, ggplot2::aes(x, -log10(p), colour = chromosome)) +
    ggplot2::geom_hline(yintercept = -log10(0.05 / nrow(data)), colour = "grey55", linetype = 2) +
    ggplot2::geom_point(size = 1.45, alpha = 0.8) +
    ggplot2::geom_text(data = peaks, ggplot2::aes(label = marker), nudge_y = 0.2,
                        size = 2.6, show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(25, 245, by = 55), labels = paste0("Chr ", 1:5)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.12))) +
    ggplot2::labs(title = "GWAS Manhattan plot",
      subtitle = "The dashed line shows a Bonferroni threshold",
      x = "Chromosome", y = "-Log10 p-value", colour = "Chromosome")
}

breeding_qq <- function() {
  p <- sort(breeding_gwas()$p)
  data <- data.frame(expected = -log10(seq_along(p) / (length(p) + 1L)),
                     observed = -log10(p), tail = ifelse(p < 0.05 / length(p), "Above threshold", "Other"))
  limit <- max(data$expected)
  ggplot2::ggplot(data, ggplot2::aes(expected, observed, colour = tail)) +
    ggplot2::geom_segment(x = 0, y = 0, xend = limit, yend = limit,
                          colour = "grey45", linetype = 2) +
    ggplot2::geom_point(size = 1.6, alpha = 0.8) +
    ggplot2::labs(title = "GWAS quantile-quantile plot", x = "Expected -log10 p",
      y = "Observed -log10 p", colour = "Markers")
}

breeding_ld_heatmap <- function() {
  dosages <- withr::with_seed(1903L, {
    do.call(cbind, lapply(seq_len(3), function(block) {
      founder <- stats::rbinom(100L, 2L, 0.25 + block * 0.05)
      replicate(6L, ifelse(stats::runif(100L) < 0.18, stats::rbinom(100L, 2L, 0.4), founder))
    }))
  })
  colnames(dosages) <- sprintf("M%02d", seq_len(ncol(dosages)))
  cells <- as.data.frame(as.table(stats::cor(dosages)^2), responseName = "r2")
  ggplot2::ggplot(cells, ggplot2::aes(Var1, Var2, fill = r2)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), name = "LD (r-squared)") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Linkage disequilibrium heatmap",
      subtitle = "Squared correlations of simulated marker dosages",
      x = "Marker", y = "Marker")
}

breeding_prediction <- function() {
  data <- withr::with_seed(450L, {
    markers <- matrix(stats::rbinom(72L * 24L, 2L, 0.4) - 1, nrow = 72L)
    effects <- stats::rnorm(24L, sd = 0.24)
    phenotype <- 5 + drop(markers %*% effects) + stats::rnorm(72L, sd = 0.45)
    list(markers = markers, phenotype = phenotype)
  })
  train <- seq_len(48L)
  holdout <- setdiff(seq_len(nrow(data$markers)), train)
  model <- rrBLUP::mixed.solve(data$phenotype[train], Z = data$markers[train, , drop = FALSE])
  predicted <- drop(data$markers[holdout, , drop = FALSE] %*% model$u) + as.numeric(model$beta)
  data <- data.frame(observed = data$phenotype[holdout], predicted = predicted,
                     family = rep(c("Family A", "Family B"), each = 12))
  # Family labels group the displayed validation points, without entering the model.
  diagonal <- range(c(data$observed, data$predicted))
  ggplot2::ggplot(data, ggplot2::aes(observed, predicted, colour = family)) +
    ggplot2::geom_segment(x = diagonal[1], y = diagonal[1], xend = diagonal[2],
                          yend = diagonal[2], linetype = 2, colour = "grey50") +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Observed versus predicted yield",
      subtitle = sprintf("RR-BLUP on held-out lines. Predictive correlation r = %.2f",
                         stats::cor(data$observed, data$predicted)),
      x = "Observed yield (t/ha)", y = "Predicted yield (t/ha)", colour = "Display group")
}

breeding_statgen_field <- function() {
  trial <- statgenSTA::createTD(breeding_field_data(), genotype = "genotype", trial = "trial",
                                repId = "replicate", rowCoord = "row", colCoord = "column")
  plots <- graphics::plot(trial, plotType = "layout", traits = "yield",
                          showGeno = TRUE, output = FALSE)
  plots[[1L]] + ggplot2::labs(title = "Trial layout from statgenSTA",
                              subtitle = "Simulated plot yields in two blocks")
}
