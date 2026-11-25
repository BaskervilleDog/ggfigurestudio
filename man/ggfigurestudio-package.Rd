\name{ggfigurestudio-package}
\alias{ggfigurestudio-package}
\alias{ggfigurestudio}
\docType{package}
\title{Edit ggplot figures interactively}
\description{
Open a ggplot in a local Shiny editor, select elements in the preview,
and export the edited figure. A figure project stores the plot, dimensions,
and manual adjustments so you can reopen it later.
}
\section{Quick start}{
\preformatted{
library(ggfigurestudio)
p <- example_figure()
fig <- edit_figure(figure_project(p, width = 210, height = 140))
# Click Return edited project in the editor before continuing.
export_figure(fig, "figure.svg")
saveRDS(fig, "figure-project.rds")
}
In the source project, run \code{pkgload::load_all()} in place of
\code{library(ggfigurestudio)}. Reopen a saved project with
\code{edit_figure(readRDS("figure-project.rds"))}.

After you click \strong{Return edited project}, the result is assigned
to \code{fig}. With RStudio's default graphics device, the updated figure
appears in the Plots pane. Use \code{edit_figure(p, show = FALSE)} to skip
automatic drawing, or use \code{grid::grid.newpage()} followed by
\code{grid::grid.draw(render_figure(fig))} to redraw it later.
}
\section{Editor appearance}{
Use \strong{Night mode} in the header to switch the editor interface
between light and dark. Plot styling is controlled by the plot theme.
}
\section{Editor tabs}{
\describe{
  \item{Selection}{Click an element in the preview to see its controls.
  For example, a title has text controls, an axis can have tick settings,
  and a point group has appearance controls. For supported tile and map
  layers and stacked bars, choose an individual mark or all marks in the
  layer. Select the whole layer to choose a palette for mapped fill.
  Palette choices are labeled categorical or continuous and grouped by
  provider. If colour and fill use different data types, the picker lists
  viridis palettes that can apply to both. The colorspace palettes require
  that package.}
  \item{Layers}{Add a reference line, shape, plain text, or boxed label.
  A boxed label fits its text. Use the font family and font style menus when
  adding or selecting plain text or a boxed label. Shapes support attached text, size,
  rotation, and opacity.
  You can also change a layer's geometry if its data supplies the required
  aesthetics, or delete a layer. Use \strong{Add to facets} to choose
  where to place an annotation. Leave it empty to add a copy in every
  facet. \strong{Facet styling} sets the default style scope.}
  \item{Figure}{Use \strong{Plot size} to set width and height as
  percentages of the image. The plot layout is centred, including titles,
  axes, and legends. The image keeps its project dimensions. Set both
  values to 100 percent to fill the image. Choose a theme preset for the
  whole plot. The list includes
  themes from ggplot2 and optional themes from cowplot, ggpubr, and
  ggthemes. Applying a preset
  replaces the current theme styling. Choose \strong{Theme at opening} to
  restore the starting theme. Use \strong{Grid lines} to show or hide
  major and minor grid lines separately. Explicit visibility settings
  carry through to export. Under \strong{Label placement}, turn on
  \strong{Connect moved labels} to draw a line back from each text or
  boxed label you drag or nudge in the plot panel. Under \strong{Connect to},
  choose \strong{Original position} for its starting point or
  \strong{Previous position} for the spot just before its latest move.
  Under \strong{Custom theme}, search for a
  ggplot2 element such as \code{plot.title}, \code{legend.position},
  or \code{panel.grid.major}. Choose common values from the property menus,
  including installed font families and font styles. Choose \strong{Inherited}
  to use the parent setting for a property. Edit individual properties or enter a
  complete value such as \code{element_text(size = 14, face = "bold")}.
  Use \strong{Inherit element} to remove its explicit settings and
  \strong{Hide element} for drawable elements. Download the resulting
  theme and apply it to another plot with
  \code{plot + readRDS("ggfigurestudio-theme.rds")}.}
  \item{Examples}{Browse the included galleries and open a plot in the
  editor. Each example suggests an edit to try. Choose
  \strong{Back to starting figure} to return to your original work.}
}
}
\section{Equations and mixed text}{
In \strong{Layers}, choose \strong{Add text} or \strong{Add label}.
Set \strong{Text format} to \strong{Math (plotmath)} and enter one
expression in \strong{Text}. Click \strong{Apply layer change}. To edit
it later, select the annotation in the preview.

\preformatted{
hat(y) == beta[0] + beta[1]*x + beta[2]*x^2
Y[ij] == mu + tau[i] + epsilon[ij]
paste("Fitted model: ", hat(y) == 2.4 + 1.8*x)
paste("Treatment effect: ", F[2*","*27] == 4.31, ", ", p == 0.024)
}

Inside \code{paste()}, quote ordinary words and punctuation. Use
\code{==} for an equals sign, \code{x^2} for a power, and \code{x[i]}
for a subscript. Click \strong{Plotmath reference} below the equation
hint, or open \code{\link[grDevices:plotmath]{plotmath}} in R.
The one-way ANOVA and linear regression examples include editable equations.
}
\section{Positioning and facets}{
Drag a movable item or select it and use the arrow keys. Set the distance
per press with \strong{Arrow-key nudge (mm)}. The default is 0.5 mm.
Click the axis to nudge its line, ticks, labels, and title together.
Click a tick label to move it individually. Vertical reference lines use
Left and Right, and horizontal lines use Up and Down. Press \kbd{Ctrl+Z}
or \kbd{Cmd+Z} to undo a movement or a layer deletion during the session.

To edit a shape, text annotation, label, or reference line, select a
copy and choose its scope under \strong{Apply edits to}. Choose
\strong{This facet}, \strong{Selected facets}, or \strong{All copies}
to apply position and style edits to that set. Use the centre fields or
the line's \strong{Position on the axis} field for positions in axis
units. Dragging or nudging affects the copy you click. Deleting a layer
removes all its copies.
}
\section{Tick positions}{
Click a primary numeric axis, a tick label, or a blank part of a panel
to open the tick settings. Enter comma-separated positions such as
\code{0, 250, 500, 750} or a sequence such as \code{0 to 1100 by 100},
then click \strong{Apply facet ticks}. Breaks outside the visible range
can extend the selected panel. Clear the field and apply to restore the
original breaks and range. These controls support primary numeric
Cartesian axes in single-panel, \code{facet_wrap()}, and \code{facet_grid()}
plots.

Select an axis to change \strong{Tick label size (pt)} or
\strong{Tick label angle (degrees)}, then click
\strong{Apply display settings}.
}
\section{Export and compatibility}{
\code{figure_composition()} places several ggplots or edited projects
in one image. Open a composition with \code{edit_figure()} and use
\strong{Figure}, then \strong{Multi-plot image}, to switch between plots,
set the number of columns and the gap, or preview the combined image.
Click \strong{Edit selected plot} to work on one plot at a time.
Click \strong{Apply layout} to update the grid, then enable
\strong{Preview combined image} to see the full composition.

\code{saveRDS()} stores the edited project. Use \code{readRDS()} and
\code{edit_figure()} to reopen it. Use \code{render_figure()} to draw it
with its manual positioning and project settings.

\code{export_figure()} uses the filename extension to select the format,
for example SVG, PDF, PNG, or TIFF. Set the raster resolution with
\code{dpi}. The editor accepts ggplot objects, including plots returned
by extension packages. Click editing depends on the geom and coordinate
system. For example, you can select \code{geom_sf()} polygons and
\code{geom_tile()} cells. A standard raster layer can be changed to tiles
for cell editing. Check saved edits if the underlying layers, grouping,
or data change.
}
\seealso{
\code{\link{edit_figure}} for project and export functions,
\code{\link{example_figure}} for example galleries, and
\code{\link{list_aesthetics}} for inspecting mappings.
}
\keyword{package}
