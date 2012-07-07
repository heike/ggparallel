\name{gghammock}
\alias{gghammock}
\title{Creating hammock plots and parallel sets}
\usage{
  gghammock(vars = list(), data, weight = NULL,
    method = "angle", alpha = 0.5, width = 0.25, order = 1,
    color = NA, label = TRUE, angle = 90,
    text.offset = NULL, ...)
}
\arguments{
  \item{vars}{list of variable names to be included in the
  plotting. order of the variables is preserved in the
  display}

  \item{data}{data frame}

  \item{weight}{weighting variable - use character string}

  \item{method}{plotting method to use - one of
  \code{parset}, \code{angle}}

  \item{alpha}{level of $\alpha$ blending for ribbons,
  value between 0 and 1, defaults to 0.5.}

  \item{width}{width of variables}

  \item{order}{flag variable with three levels -1, 0, 1 for
  levels in decreasing order, levels in increasing order
  and levels unchanged. This variable can be either a
  scalar or a vector}

  \item{color}{colors for levels. if not specified, grey is
  used}

  \item{label}{binary variable (vector), whether labels
  should be shown.}

  \item{angle}{by which text for labelling is rotated.
  Ignored if label = FALSE}

  \item{text.offset}{(vector) of values for offset the
  labels}

  \item{...}{passed on directly to all of the ggplot2
  commands}
}
\value{
  returns a ggplot2 object that can be plotted directly or
  used as base layer for additional modifications.
}
\description{
  Hammock plots and parallel set plots allow visualization
  of pairwise relationships between categorical variables.
}
\details{
  more detailed description: we need to reference Kosara's
  paper on parallel sets, we should also point to Matt
  Schonlau's paper for the hammocks
}
\examples{
data(mtcars)
gghammock(list("gear", "cyl"), data=mtcars)
# compare with method='parset'
gghammock(list("gear", "cyl"), data=mtcars, method='parset')
# flip plot and rotate text
gghammock(list("gear", "cyl"), data=mtcars, angle=0) + coord_flip()
# change colour scheme
gghammock(list("gear", "cyl"), data=mtcars, angle=0) + coord_flip() +
  scale_fill_brewer(palette="Set1")
# example with more than two variables:
titanic <- as.data.frame(Titanic)
gghammock(list("Class", "Sex", "Survived"), data=titanic, weight="Freq", angle=0) +
  coord_flip() + scale_fill_brewer(palette=6, guide="none")
# biological examples: genes and pathways
data(genes)
require(RColorBrewer)
gghammock(list("chrom", "path"), data = genes, color = "white",
  factorlevels =  c(sapply(unique(genes$chrom), as.character),
  unique(genes$path))) +
  scale_fill_manual(values = c(  rep("grey80", 24), brewer.pal("YlOrRd", n = 9)), guide="none") +
  coord_flip()
}

