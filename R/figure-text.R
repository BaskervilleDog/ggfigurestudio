axis_title_target <- function(built, role) {
  panel <- built$layout$panel_params[[1]]
  axis <- substr(role, 1L, 1L)
  side <- c(b = "bottom", t = "top", l = "left", r = "right")[[substr(role, 6, 6)]]
  primary <- panel[[axis]]
  secondary <- panel[[paste0(axis, ".sec")]]
  if (identical(primary$position, side))
    return(list(axis = primary$scale$aesthetics[1L], secondary = FALSE))
  if (identical(secondary$position, side) && inherits(secondary$scale$secondary.axis, "AxisSecondary"))
    return(list(axis = secondary$scale$aesthetics[1L], secondary = TRUE))
  NULL
}

set_axis_title <- function(plot, target, value) {
  axis <- target$axis
  scale <- plot$scales$get_scales(axis)
  if (is.null(scale) || (!target$secondary && inherits(scale$name, "waiver"))) {
    return(plot + do.call(ggplot2::labs, stats::setNames(list(value), axis)))
  }
  clone <- scale$clone()
  if (target$secondary) {
    clone$secondary.axis <- ggplot2::ggproto(NULL, scale$secondary.axis, name = value)
  } else {
    clone$name <- value
  }
  # Replace the cloned scale in place to retain its position in the scale list.
  scales <- plot$scales$clone()
  i <- which(vapply(scales$scales, function(s) axis %in% s$aesthetics, logical(1)))
  scales$scales[[i]] <- clone
  plot$scales <- scales
  plot
}

guide_text_id <- function(params) paste(sort(params$aesthetic), collapse = "/")

guide_break_id <- function(value) describe_aes(value)

apply_guide_text <- function(guide, edits) {
  if (is.null(edits)) return(guide)
  if (!is.null(edits$title)) {
    guide$figureedit_original_title <- guide$title
    guide$title <- edits$title
  }
  if (length(edits$entries) && length(guide$key$.value)) {
    guide$figureedit_original_labels <- guide$key$.label
    for (i in seq_along(guide$key$.value)) {
      label <- edits$entries[[guide_break_id(guide$key$.value[[i]])]]
      if (!is.null(label)) guide$key$.label[i] <- label
    }
  }
  guide
}

set_legend_text <- function(plot, target, value) {
  guides <- group_style_guides(plot$guides)
  # Copy the guide container so the input plot can still be printed independently.
  updated <- rlang::env_clone(guides)
  attributes(updated) <- attributes(guides)
  edits <- guides$figureedit_text
  if (is.null(target$entry)) edits[[target$guide]]$title <- value
  else edits[[target$guide]]$entries[[target$entry]] <- value
  updated$figureedit_text <- edits
  plot$guides <- updated
  plot
}

# Match a rendered legend table to its trained guide by the title and labels.
match_legend_table <- function(table, guides) {
  if (!inherits(table, "gtable")) return(NULL)
  get_label <- function(grob) {
    if (inherits(grob, "text")) return(as.character(grob$label))
    unlist(lapply(grob$children, get_label), use.names = FALSE)
  }
  title <- unlist(lapply(table$grobs[table$layout$name == "title"], get_label), use.names = FALSE)
  labels <- unlist(lapply(table$grobs[startsWith(table$layout$name, "label")], get_label), use.names = FALSE)
  matches <- Filter(function(guide) {
    length(guide$aesthetic) && identical(as.character(guide$title), as.character(title)) &&
      identical(sort(as.character(guide$key$.label)), sort(as.character(labels)))
  }, guides)
  if (length(matches) == 1L) matches[[1L]]
}

legend_text_target <- function(guide, role, label) {
  if (is.null(guide)) return(NULL)
  target <- list(guide = guide_text_id(guide), original = label)
  if (identical(role, "title")) {
    if (length(guide$figureedit_original_title)) target$original <- as.character(guide$figureedit_original_title)
    return(target)
  }
  if (!startsWith(role, "label")) return(NULL)
  row <- which(as.character(guide$key$.label) == label)
  if (length(row) > 1L && length(guide$figureedit_label_row) &&
      guide$figureedit_label_row %in% row) row <- guide$figureedit_label_row
  if (length(row) != 1L || length(guide$key$.value) < row) return(NULL)
  target$entry <- guide_break_id(guide$key$.value[[row]])
  if (length(guide$figureedit_original_labels))
    target$original <- as.character(guide$figureedit_original_labels[[row]])
  target
}
