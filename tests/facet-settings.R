library(ggfigurestudio)
library(ggplot2)

p <- example_figure()
q <- ggfigurestudio:::set_facet_breaks(p, '{"Treatment":"chilled"}',
                                     list(x = c(100, 300, 600), y = c(5, 15, 25)))
before <- ggplot_build(p)$layout$panel_params
after <- ggplot_build(q)$layout$panel_params
stopifnot(identical(after[[1]]$x$breaks, before[[1]]$x$breaks),
          identical(after[[2]]$x$breaks, c(100, 300, 600)),
          identical(after[[2]]$x.range, before[[2]]$x.range))
preview <- ggfigurestudio:::tick_preview(figure_project(q))
stopifnot('{"facets":{"Treatment":"chilled"},"side":"l","value":25}' %in% preview$keys,
          '{"facets":{"Treatment":"nonchilled"},"side":"l","value":20}' %in% preview$keys)
key <- grep("strip.*[/]chilled/", names(preview$text_targets), value = TRUE)
q <- ggfigurestudio:::set_facet_title(q, preview$text_targets[[key]]$target, "Cold treatment")
stopifnot(identical(ggfigurestudio:::tick_preview(figure_project(q))$text_targets[[key]]$displayed,
                    "Cold treatment"))
q <- ggfigurestudio:::set_facet_breaks(q, '{"Treatment":"chilled"}', list(x = NULL))
stopifnot(identical(ggplot_build(q)$layout$panel_params[[2]]$x$breaks, before[[2]]$x$breaks))

# Explicit ticks can extend a fixed scale in one panel, including with a secondary axis.
extended <- ggfigurestudio:::set_facet_breaks(p, '{"Treatment":"nonchilled"}',
                                            list(x = ggfigurestudio:::parse_facet_breaks("0 to 1100 by 100")))
panels <- ggplot_build(extended)$layout$panel_params
stopifnot(identical(panels[[1]]$x.range, c(0, 1100)),
          identical(panels[[2]]$x.range, before[[2]]$x.range))
preview <- ggfigurestudio:::tick_preview(figure_project(extended))
stopifnot('{"facets":{"Treatment":"nonchilled"},"side":"b","value":0}' %in% preview$keys,
          '{"facets":{"Treatment":"nonchilled"},"side":"b","value":1100}' %in% preview$keys,
          '{"facets":{"Treatment":"chilled"},"side":"b","value":1000}' %in% preview$keys)
reset <- ggfigurestudio:::set_facet_breaks(extended, '{"Treatment":"nonchilled"}', list(x = NULL))
stopifnot(identical(ggplot_build(reset)$layout$panel_params[[1]]$x.range, before[[1]]$x.range))

parse_ticks <- ggfigurestudio:::parse_facet_breaks
stopifnot(identical(as.numeric(parse_ticks("-2 to 2 by 1.5")), c(-2, -0.5, 1)),
          identical(parse_ticks("700, 100, 300"), c(100, 300, 700)),
          is.null(parse_ticks(" ")),
          inherits(try(parse_ticks("0 to 100 by 0"), silent = TRUE), "try-error"))
scatter <- gallery_example("scatter")
target <- ggfigurestudio:::tick_preview(figure_project(scatter))$facet_targets[["element/axis-b"]]
stopifnot(identical(names(target$values), "x"))
scatter <- ggfigurestudio:::set_facet_breaks(scatter, target$id,
                                         list(x = parse_ticks("1 to 6 by 1")))
built <- ggplot_build(scatter)
stopifnot(identical(built$layout$panel_params[[1]]$x.range, c(1, 6)),
          '{"facets":{},"side":"b","value":6}' %in%
            ggfigurestudio:::tick_preview(figure_project(scatter))$keys)
