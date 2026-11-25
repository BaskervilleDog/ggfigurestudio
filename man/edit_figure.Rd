\name{edit_figure}
\alias{edit_figure}
\alias{figure_project}
\alias{figure_composition}
\alias{render_figure}
\alias{export_figure}
\title{Create, edit, and export a figure project}
\description{
Wrap a ggplot with export settings, edit it in Shiny, or render the
saved project with its manual adjustments.
}
\usage{
figure_project(plot, width = 180, height = 110)
figure_composition(plots, width = 240, height = 150,
                   ncol = min(2L, length(plots)), gap = 4)
edit_figure(plot = example_figure(), show = interactive())
render_figure(project)
export_figure(project, filename, dpi = 300)
}
\arguments{
  \item{plot}{A ggplot object. \code{edit_figure()} also accepts a saved
  figure project, a composition, or a list of plots. When omitted, the editor starts with
  \code{example_figure()}.}
  \item{plots}{A list of ggplot objects or figure projects. Names appear
  in the plot selector.}
  \item{show}{Whether to draw the edited result on the current R graphics
  device after returning from the editor. Defaults to true in interactive
  sessions.}
  \item{ncol}{Number of columns in the combined image. Plots fill rows
  in list order.}
  \item{gap}{Space between plots in millimetres.}
  \item{width, height}{Positive figure dimensions in millimetres.}
  \item{project}{A figure project created by \code{figure_project()} or
  returned by \code{edit_figure()}, or a composition created with
  \code{figure_composition()}.}
  \item{filename}{Output file path. The extension determines the graphics
  device, for example \code{.svg}, \code{.pdf}, \code{.png}, or \code{.tiff}.}
  \item{dpi}{Resolution in dots per inch for raster output.}
}
\details{
\code{edit_figure()} runs the editor until you choose
\strong{Return edited project}. In interactive RStudio sessions, the
edited result is drawn in the Plots pane when using its default graphics
device. Save the returned object with
\code{saveRDS()} to reopen it later. Use \code{render_figure()} or
\code{export_figure()} on the returned object to include manual
positioning adjustments and other project settings. To redraw the result
in the Plots pane, call \code{grid::grid.newpage()} and then pass the
rendered project to \code{grid::grid.draw()}.
For PDF export, selected system fonts in annotations and styled text
layers use Cairo when it is available.

When editing a composition, use \strong{Figure} and
\strong{Multi-plot image} to select a plot, change the number of columns
or the gap, and preview the combined image. Each plot keeps its own
theme, positioning, and editing history. Return the edited composition
while editing one of its plots or previewing the combined image.

Use the \strong{Examples} tab to browse and open a built-in plot.
The editor keeps each figure's edits during the session. Choose
\strong{Back to starting figure} to switch back. The return button
returns the figure currently in the preview.
}
\value{
\code{figure_project()} returns a \code{figure_project}.
\code{figure_composition()} returns a \code{figure_composition}.
\code{edit_figure()} invisibly returns the edited project or composition.
\code{render_figure()} returns a grid graphical
object. \code{export_figure()} writes the figure and invisibly returns
the supplied filename.
}
\examples{
project <- figure_project(example_figure(), width = 210, height = 140)
export_figure(project, tempfile(fileext = ".svg"))
file <- tempfile(fileext = ".rds")
saveRDS(project, file)

combined <- figure_composition(
  list(Scatter = gallery_example("scatter"),
       Bars = gallery_example("stacked_bar")),
  width = 260, height = 140, ncol = 2, gap = 5
)
export_figure(combined, tempfile(fileext = ".svg"))
if (interactive()) {
  edited <- edit_figure(readRDS(file))
  edited_combined <- edit_figure(combined)
}
}
\seealso{\code{\link{ggfigurestudio}}}
