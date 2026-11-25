\name{list_aesthetics}
\alias{list_aesthetics}
\title{Inspect the aesthetics of a ggplot or layer}
\description{
List aesthetic names and show how their values are supplied, with
guidance on accepted arguments.
}
\usage{
list_aesthetics(plot, layer = 0L)
}
\arguments{
  \item{plot}{A ggplot object.}
  \item{layer}{Use \code{0} to inspect plot-level mappings and list
  aesthetics available across layers. Use a positive layer index to
  inspect an individual layer.}
}
\value{
A data frame with columns \code{aesthetic}, \code{source}, \code{value},
\code{required_group}, and \code{arguments}.
}
\examples{
p <- example_figure()
head(list_aesthetics(p))
head(list_aesthetics(p, layer = 1L))
}
\seealso{\code{\link{ggfigurestudio}}, \code{\link{edit_figure}}}
