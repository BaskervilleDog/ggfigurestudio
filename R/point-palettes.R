mapped_palette_aesthetics <- function(plot, layer, aesthetics) {
  mapping <- if (isTRUE(layer$inherit.aes)) as.list(plot$mapping) else list()
  if (length(layer$mapping)) mapping[names(layer$mapping)] <- as.list(layer$mapping)
  candidates <- setdiff(aesthetics, names(layer$aes_params))
  Filter(function(aesthetic) !is.null(mapping[[aesthetic]]), candidates)
}

palette_scale_type <- function(scale) {
  if (is.null(scale) || inherits(scale, c("ScaleContinuousIdentity", "ScaleDiscreteIdentity")))
    return("")
  if (scale$is_discrete()) "discrete" else if (inherits(scale, "ScaleContinuous")) "continuous" else ""
}

point_palette_targets <- function(plot, layer) {
  if (!inherits(layer$geom, "GeomPoint")) return(character())
  candidates <- mapped_palette_aesthetics(plot, layer, c("colour", "fill"))
  types <- vapply(candidates, function(aesthetic) palette_scale_type(plot$scales$get_scales(aesthetic)),
                  character(1))
  stats::setNames(types[nzchar(types)], candidates[nzchar(types)])
}

tile_palette_type <- function(plot, layer) {
  if (!inherits(layer$geom, c("GeomTile", "GeomSf", "GeomBar", "GeomCol")) ||
      !length(mapped_palette_aesthetics(plot, layer, "fill")))
    return("")
  palette_scale_type(plot$scales$get_scales("fill"))
}

project_palettes <- function(project) {
  if (!is.null(project$point_palettes)) return(project$point_palettes)
  if (identical(project$palette, "Original") || !length(project$aesthetic)) return(character())
  stats::setNames(rep(project$palette, length(project$aesthetic)), project$aesthetic)
}

palette_scale <- function(palette, aesthetics) {
  continuous <- startsWith(palette, "Gradient:")
  code <- sub("^Gradient:", "", palette)
  if (continuous && nchar(code) == 1L)
    return(ggplot2::scale_colour_viridis_c(option = code, aesthetics = aesthetics))
  if (code == "Viridis" || startsWith(code, "Viridis:")) {
    option <- if (code == "Viridis") "D" else sub("^Viridis:", "", code)
    return(ggplot2::scale_colour_viridis_d(option = option, aesthetics = aesthetics))
  }
  if (code == "Set2" || startsWith(code, "Brewer:")) {
    name <- sub("^Brewer:", "", code)
    info <- RColorBrewer::brewer.pal.info
    if (continuous) return(ggplot2::scale_colour_distiller(
      type = info[name, "category"], palette = name, direction = 1, aesthetics = aesthetics))
    maximum <- info[name, "maxcolors"]
    scale <- ggplot2::scale_colour_brewer(palette = name, aesthetics = aesthetics)
    original <- scale$palette
    scale$palette <- function(n) {
      if (n <= maximum) original(n)
      else grDevices::colorRampPalette(RColorBrewer::brewer.pal(maximum, name))(n)
    }
    return(scale)
  }
  if (startsWith(code, "HCL:") || startsWith(code, "HCLD:")) {
    if (!requireNamespace("colorspace", quietly = TRUE))
      stop("Install the colorspace package in R to use this palette.", call. = FALSE)
    name <- sub("^HCLD?:", "", code)
    if (!continuous)
      return(colorspace::scale_colour_discrete_qualitative(palette = name, aesthetics = aesthetics))
    if (startsWith(code, "HCLD:"))
      return(colorspace::scale_colour_continuous_diverging(palette = name, aesthetics = aesthetics))
    return(colorspace::scale_colour_continuous_sequential(palette = name, aesthetics = aesthetics))
  }
  stop("Unknown palette.")
}

apply_plot_palettes <- function(plot, palettes) {
  palettes <- palettes[palettes != "Original"]
  if (!length(palettes)) return(plot)
  scales <- plot$scales$clone()
  updated <- list()
  for (scale in scales$scales) {
    affected <- intersect(scale$aesthetics, names(palettes))
    type <- ifelse(startsWith(unname(palettes[affected]), "Gradient:"), "continuous", "discrete")
    affected <- affected[type == palette_scale_type(scale)]
    if (!length(affected)) {
      updated <- c(updated, list(scale))
      next
    }
    remaining <- setdiff(scale$aesthetics, affected)
    if (length(remaining)) {
      original <- scale$clone()
      original$aesthetics <- remaining
      updated <- c(updated, list(original))
    }
    for (aesthetics in split(affected, palettes[affected])) {
      clone <- scale$clone()
      clone$aesthetics <- aesthetics
      clone$palette <- palette_scale(palettes[[aesthetics[1L]]], aesthetics)$palette
      clone$palette.cache <- NULL
      updated <- c(updated, list(clone))
    }
    palettes <- palettes[!names(palettes) %in% affected]
  }
  present <- unlist(lapply(updated, function(scale) scale$aesthetics), use.names = FALSE)
  missing <- setdiff(names(palettes), present)
  # The plot may rely on ggplot2 to create its default scale.
  for (aesthetics in split(missing, palettes[missing])) {
    updated <- c(updated, list(palette_scale(palettes[[aesthetics[1L]]], aesthetics)))
  }
  scales$scales <- updated
  plot$scales <- scales
  plot
}

viridis_palette <- function(palette, type) {
  ifelse(type == "continuous" & palette == "Viridis", "Gradient:D",
    ifelse(type == "continuous" & startsWith(palette, "Viridis:"),
           sub("^Viridis:", "Gradient:", palette), palette))
}

palette_groups <- function(targets) {
  common <- c("Choose a palette" = "", "Original" = "Original")
  types <- unique(unname(targets))
  viridis <- c(Viridis = "Viridis", Magma = "Viridis:A", Inferno = "Viridis:B",
               Plasma = "Viridis:C", Cividis = "Viridis:E", Rocket = "Viridis:F",
               Mako = "Viridis:G", Turbo = "Viridis:H")
  if (length(types) > 1L) return(list(Scale = common, "Categorical and continuous (viridis)" = viridis))
  if (identical(types, "continuous")) {
    return(list(Scale = common,
      "Continuous (viridis)" = stats::setNames(viridis_palette(viridis, "continuous"), names(viridis)),
      "Continuous (ColorBrewer)" = c(Blues = "Gradient:Brewer:Blues", Greens = "Gradient:Brewer:Greens",
                      YlGnBu = "Gradient:Brewer:YlGnBu", RdBu = "Gradient:Brewer:RdBu",
                      Spectral = "Gradient:Brewer:Spectral"),
      "Continuous (colorspace)" = c(Oslo = "Gradient:HCL:Oslo", "Purple-Blue" = "Gradient:HCL:Purple-Blue",
                     Terrain = "Gradient:HCL:Terrain", "Blue-Red 3" = "Gradient:HCLD:Blue-Red 3",
                     "Purple-Green" = "Gradient:HCLD:Purple-Green")))
  }
  list(Scale = common, "Categorical (viridis)" = viridis,
       "Categorical (ColorBrewer)" = c(Set2 = "Set2", Dark2 = "Brewer:Dark2", Paired = "Brewer:Paired",
                       Set1 = "Brewer:Set1", Set3 = "Brewer:Set3", Accent = "Brewer:Accent"),
       "Categorical (colorspace)" = c("Dark 3" = "HCL:Dark 3", "Pastel 1" = "HCL:Pastel 1",
                      Harmonic = "HCL:Harmonic", Dynamic = "HCL:Dynamic"))
}

palette_choices <- function(targets) unlist(palette_groups(targets), use.names = FALSE)

mark_palette_targets <- function(mark) {
  if (!is.null(mark$cell)) return(c(fill = mark$palette_type))
  mark$palette_targets
}

mark_palette_input <- function(mark, palettes, all_cells) {
  if (!is.null(mark$cell) && (!all_cells || !nzchar(mark$palette_type))) return(NULL)
  targets <- mark_palette_targets(mark)
  if (!length(targets)) return(NULL)
  choices <- palette_choices(targets)
  active <- palettes[names(targets)]
  if (length(unique(targets)) > 1L) {
    gradient <- !is.na(active) & grepl("^Gradient:[A-H]$", active)
    active[gradient] <- sub("^Gradient:", "Viridis:", active[gradient])
    active[active %in% "Viridis:D"] <- "Viridis"
  }
  active[is.na(active)] <- "Original"
  selected <- if (length(unique(active)) == 1L && active[1L] %in% choices) unname(active[1L]) else ""
  type_names <- ifelse(targets == "discrete", "categorical", "continuous")
  mapped_types <- paste(paste0(tools::toTitleCase(names(targets)), " uses ", type_names, " data."),
                        collapse = " ")
  editor_group("Colour scale",
    editor_select("point_palette", "Palette", palette_groups(targets), selected = selected),
    shiny::p(class = "editor-help", mapped_types),
    shiny::p(class = "editor-help", paste0("Applies to mapped ", paste(names(targets), collapse = " and "), " across the plot.")),
    shiny::actionButton("apply_point_palette", "Apply palette")
  )
}
