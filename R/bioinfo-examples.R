bioinfo_examples <- function() {
  data.frame(
    id = c("volcano", "ma", "pca", "expression_heatmap", "enrichment", "coverage",
           "sample_tree", "deseq2_pca", "ggpubr_expression", "repel_volcano"),
    plot = c("Differential expression volcano", "MA plot", "Sample PCA",
             "Expression heatmap", "Enrichment dot plot", "Faceted coverage",
             "Sample clustering tree", "DESeq2 sample PCA", "ggpubr expression boxplots",
             "Volcano with ggrepel labels"),
    packages = c(rep("ggplot2", 7), "DESeq2, SummarizedExperiment", "ggpubr", "ggrepel"),
    try_editing = c(
      "Change one differential-expression group's points and legend key.",
      "Edit a point group and change ticks on the log-scaled mean axis.",
      "Click a condition and batch group to edit its points, then move a sample label.",
      "Change the fill palette, then style one tile independently.",
      "Apply a fill palette to the dots, then move the size legend.",
      "Style an uncertainty ribbon and change the ticks in one coverage panel.",
      "Style the branches or sample dots, then move a tip label.",
      "Click a sample point to edit its condition group in a plot returned by DESeq2.",
      "Click a boxplot and a jittered point independently.",
      "Style the point layer and check that the repelled labels still render."),
    stringsAsFactors = FALSE
  )
}

bioinfo_example <- function(id) {
  catalog <- bioinfo_examples()
  if (is.numeric(id) && length(id) == 1L && is.finite(id) &&
      id %in% seq_len(nrow(catalog))) id <- catalog$id[id]
  if (!is.character(id) || length(id) != 1L || !id %in% catalog$id)
    stop("Choose an example number or ID from bioinfo_examples().", call. = FALSE)
  packages <- strsplit(catalog$packages[match(id, catalog$id)], ", ", fixed = TRUE)[[1L]]
  missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing))
    stop("This example requires: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  plot <- switch(id,
    volcano = bioinfo_volcano(),
    ma = bioinfo_ma(),
    pca = bioinfo_pca(),
    expression_heatmap = bioinfo_heatmap(),
    enrichment = bioinfo_enrichment(),
    coverage = bioinfo_coverage(),
    sample_tree = bioinfo_tree(),
    deseq2_pca = bioinfo_deseq2_pca(),
    ggpubr_expression = bioinfo_boxplot(),
    repel_volcano = bioinfo_volcano(repel = TRUE)
  )
  plot + ggplot2::labs(caption = "Simulated data for editor testing") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 14, 10, 12),
                   panel.grid.minor = ggplot2::element_blank())
}

# Deterministic synthetic values keep the test cases reproducible across sessions.
bioinfo_expression <- function() {
  genes <- seq_len(36L)
  samples <- data.frame(sample = sprintf("S%02d", 1:12),
    condition = factor(rep(c("Control", "Treatment"), each = 6L)),
    batch = factor(rep(c("Batch A", "Batch B"), times = 6L)))
  rownames(samples) <- samples$sample
  values <- outer(genes, seq_len(nrow(samples)), function(g, s) {
    6 + 1.5 * sin(g * 0.8) + (s > 6) * 1.4 * cos(g / 3) +
      (s %% 2 == 0) * 0.9 * sin(g / 5) + 0.24 * sin(g * s / 3)
  })
  dimnames(values) <- list(sprintf("Gene%02d", genes), samples$sample)
  list(values = values, samples = samples)
}

bioinfo_de_results <- function() {
  i <- seq_len(220L)
  abundance <- 2^(1 + (i * 43 %% 125) / 10)
  effect <- sin(i * 2.04) * (0.6 + 2.5 / (1 + abundance / 30)) +
    ifelse(i %% 17 == 0L, 2.4, ifelse(i %% 19 == 0L, -2.4, 0))
  error <- 0.36 + 1.2 / sqrt(1 + abundance / 9)
  fdr <- stats::p.adjust(2 * stats::pnorm(-abs(effect / error)), method = "BH")
  status <- ifelse(fdr < 0.05 & effect > 1, "Higher",
                   ifelse(fdr < 0.05 & effect < -1, "Lower", "Other"))
  data.frame(gene = sprintf("G%03d", i), abundance = abundance, effect = effect,
             fdr = fdr, status = factor(status, levels = c("Lower", "Other", "Higher")))
}

bioinfo_volcano <- function(repel = FALSE) {
  data <- bioinfo_de_results()
  labels <- data[head(order(data$fdr), 5L), ]
  annotation <- if (repel) {
    ggrepel::geom_text_repel(data = labels, ggplot2::aes(label = gene), seed = 12,
                            size = 3, max.overlaps = Inf, show.legend = FALSE)
  } else {
    ggplot2::geom_label(data = labels, ggplot2::aes(label = gene), nudge_y = 0.3,
                        size = 2.6, show.legend = FALSE)
  }
  ggplot2::ggplot(data, ggplot2::aes(effect, -log10(fdr), colour = status)) +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "grey55") +
    ggplot2::geom_vline(xintercept = c(-1, 1), linetype = "dashed", colour = "grey55") +
    ggplot2::geom_point(size = 1.7, alpha = 0.7) +
    annotation +
    ggplot2::scale_colour_manual(values = c(Lower = "#35669A", Other = "#A4AAB2", Higher = "#B53C56")) +
    ggplot2::labs(title = if (repel) "Volcano plot with repelled labels" else "Differential expression volcano",
      x = "Log2 fold change", y = "-Log10 adjusted p-value", colour = "Expression change")
}

bioinfo_ma <- function() {
  ggplot2::ggplot(bioinfo_de_results(), ggplot2::aes(abundance, effect, colour = status)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45") +
    ggplot2::geom_point(size = 1.75, alpha = 0.75) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(title = "MA plot of differential expression", x = "Mean normalized expression",
      y = "Log2 fold change", colour = "Expression change")
}

bioinfo_pca <- function() {
  data <- bioinfo_expression()
  pca <- stats::prcomp(t(data$values))
  explained <- 100 * pca$sdev^2 / sum(pca$sdev^2)
  scores <- cbind(data$samples, as.data.frame(pca$x[, 1:2]))
  representative <- !duplicated(scores[c("condition", "batch")])
  ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2, colour = condition, shape = batch)) +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::geom_text(data = scores[representative, ], ggplot2::aes(label = sample),
                        vjust = -1.2, size = 2.7, show.legend = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.16)) +
    ggplot2::labs(title = "PCA of synthetic expression profiles",
      x = sprintf("PC1 (%.1f%%)", explained[1]), y = sprintf("PC2 (%.1f%%)", explained[2]),
      colour = "Condition", shape = "Batch")
}

bioinfo_heatmap <- function() {
  data <- bioinfo_expression()
  values <- t(scale(t(data$values[seq_len(16L), ])))
  cells <- expand.grid(gene = rownames(values), sample = colnames(values), stringsAsFactors = FALSE)
  cells$z <- as.vector(values)
  cells$gene <- factor(cells$gene, levels = rev(rownames(values)))
  ggplot2::ggplot(cells, ggplot2::aes(sample, gene, fill = z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradient2(low = "#27588A", mid = "#F7F6F2", high = "#AA3457",
                                 name = "Row z-score") +
    ggplot2::labs(title = "Gene expression across samples", x = "Sample", y = "Gene")
}

bioinfo_enrichment <- function() {
  terms <- data.frame(term = rev(c("Vesicle transport", "Chromatin organization", "Protein folding",
                                 "Lipid metabolism", "RNA processing", "Cell adhesion")),
                      count = c(6, 9, 8, 12, 15, 17),
                      fdr = c(0.045, 0.038, 0.024, 0.017, 0.009, 0.004))
  terms$term <- factor(terms$term, levels = terms$term)
  ggplot2::ggplot(terms, ggplot2::aes(count / 60, term, size = count, fill = fdr)) +
    ggplot2::geom_point(shape = 21, colour = "grey25", stroke = 0.4) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::scale_size_continuous(range = c(3, 9)) +
    ggplot2::labs(title = "Functional enrichment dot plot", x = "Gene ratio", y = NULL,
      fill = "Adjusted p-value", size = "Gene count")
}

bioinfo_coverage <- function() {
  positions <- 1:100
  samples <- c("Control", "Treatment")
  data <- expand.grid(position = positions, sample = samples,
                      condition = c("Replicate A", "Replicate B"))
  data$depth <- 3 + 14 * exp(-((data$position - 30) / 10)^2) +
    (7 + 13 * (data$sample == "Treatment")) * exp(-((data$position - 73) / 11)^2) +
    0.9 * sin(data$position * 0.45 + (data$condition == "Replicate A"))
  data$lower <- pmax(0, data$depth - 2)
  data$upper <- data$depth + 2
  ggplot2::ggplot(data, ggplot2::aes(position, depth, colour = sample)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = sample),
                        colour = NA, alpha = 0.18) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::facet_wrap(~condition) +
    ggplot2::labs(title = "Read coverage across a simulated region",
      x = "Position (bp)", y = "Read depth", colour = "Condition", fill = "Condition")
}

bioinfo_tree <- function() {
  data <- bioinfo_expression()
  tree <- stats::hclust(stats::dist(t(data$values)))
  n <- length(tree$order)
  tips <- data$samples[tree$order, ]
  tips$y <- seq_len(n)
  y <- numeric(2L * n - 1L)
  y[tree$order] <- tips$y
  x <- c(rep(0, n), -tree$height)
  branches <- lapply(seq_len(n - 1L), function(i) {
    child <- tree$merge[i, ]
    child <- ifelse(child < 0, -child, n + child)
    parent <- n + i
    y[parent] <<- mean(y[child])
    data.frame(x = c(x[child], x[parent]), xend = rep(x[parent], 3),
               y = c(y[child], y[child[1]]), yend = c(y[child], y[child[2]]),
               merge = factor(i))
  })
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = do.call(rbind, branches),
      ggplot2::aes(x, y, xend = xend, yend = yend, group = merge), colour = "grey35") +
    ggplot2::geom_point(data = tips, ggplot2::aes(x = 0, y = y, colour = condition), size = 2.8) +
    ggplot2::geom_text(data = tips, ggplot2::aes(x = 0, y = y, label = sample),
                      hjust = -0.35, size = 2.8) +
    ggplot2::scale_x_continuous(labels = abs, expand = ggplot2::expansion(mult = c(0.04, 0.18))) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::labs(title = "Hierarchical clustering of samples",
      x = "Clustering height", y = NULL, colour = "Condition")
}

bioinfo_deseq2_pca <- function() {
  data <- bioinfo_expression()
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(expression = data$values), colData = data$samples)
  DESeq2::plotPCA(DESeq2::DESeqTransform(se), intgroup = "condition") +
    ggplot2::labs(title = "Sample PCA from DESeq2", colour = "Condition")
}

bioinfo_boxplot <- function() {
  data <- bioinfo_expression()
  data$samples$expression <- as.numeric(data$values["Gene10", ])
  ggpubr::ggboxplot(data$samples, x = "condition", y = "expression", fill = "condition",
                   add = "jitter", add.params = list(size = 1.8), width = 0.55) +
    ggplot2::labs(title = "Marker expression by condition",
      x = "Condition", y = "Log-scale expression", fill = "Condition")
}
