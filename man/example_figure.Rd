\name{example_figure}
\alias{example_figure}
\alias{gallery_examples}
\alias{gallery_example}
\alias{bioinfo_examples}
\alias{bioinfo_example}
\alias{breeding_examples}
\alias{breeding_example}
\alias{map_examples}
\alias{map_example}
\title{Example plots for trying the editor}
\description{
Browse example galleries and load individual ggplots to try in the editor.
}
\usage{
example_figure()
gallery_examples()
gallery_example(id)
bioinfo_examples()
bioinfo_example(id)
breeding_examples()
breeding_example(id)
map_examples()
map_example(id)
}
\arguments{
  \item{id}{An example ID from the corresponding catalogue, or its
  one-based row number.}
}
\details{
\code{example_figure()} uses the built-in \code{CO2} dataset and includes
facets, multiple axes, a legend, and labels to edit.

The galleries cover general ggplot charts, bioinformatics, plant
breeding and quantitative genetics, and maps. Each catalogue includes
an editing task. Where extra packages are needed, the catalogue lists
them in its \code{packages} column.

The general gallery also includes one-way and two-way ANOVA, linear,
quadratic, logistic, and exponential regression, a residual plot,
a randomized block experiment, and paired measurements. These examples
use simulated data. Open \code{gallery_example("anova_one_way")} or
\code{gallery_example("regression_linear")} to try selecting and editing
a plotmath equation in the preview.

Use \code{gallery_example("stacked_bar")} to try styling an individual
bar segment or the whole layer. In the reaction norms example,
\code{breeding_example("reaction_norm")}, the x-axis places site labels
at each environment's mean yield, so tick spacing reflects yield differences.
}
\value{
\code{example_figure()} and the functions ending in \code{_example}
return a ggplot object. The catalogue functions ending in
\code{_examples} return data frames.
}
\examples{
gallery_examples()
p <- gallery_example("scatter")
map_examples()
if (interactive()) {
  edit_figure(p)
}
}
\seealso{\code{\link{edit_figure}}, \code{\link{ggfigurestudio}}}
