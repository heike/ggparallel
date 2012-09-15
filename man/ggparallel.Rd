\name{ggparallel}
\alias{ggparallel}
\title{Variations of parallel coordinate plots}
\usage{
  ggparallel(vars = list(), data, weight = NULL,
    method = "angle", alpha = 0.5, width = 0.25, order = 1,
    ratio = 0.2, asp = NULL, label = TRUE, text.angle = 90,
    text.offset = NULL, color = "white", ...)
}
\arguments{
  \item{vars}{list of variable names to be included in the
  plotting. order of the variables is preserved in the
  display}

  \item{data}{data frame}

  \item{weight}{weighting variable - use character string}

  \item{method}{plotting method to use - one of
  \code{angle}, \code{adj.angle}, \code{parset}, or
  \code{hammock}, for a hammock plot the aspect ratio needs
  to be fixed.}

  \item{alpha}{level of alpha blending for the fill color
  in ribbons, value has to be between 0 and 1, defaults to
  0.5.}

  \item{width}{width of variables}

  \item{order}{flag variable with three levels -1, 0, 1 for
  levels in decreasing order, levels in increasing order
  and levels unchanged. This variable can be either a
  scalar or a vector}

  \item{ratio}{used for methods with angle adjustments
  (method = \code{'hammock', 'adj.angle'}): specifies the
  height (width for horizontal displays) of the widest line
  as ratio of the overall display height (width for
  horizontal displays).}

  \item{label}{binary variable (vector), whether labels
  should be shown.}

  \item{text.angle}{numeric value in degrees, by which text
  for labelling is rotated. Ignored if label = FALSE}

  \item{text.offset}{(vector) of values for offset the
  labels}

  \item{asp}{aspect ratio of the plot - it will be set to a
  default of 1 in the case of hammock plots.}

  \item{color}{value used for color of the boxes.}

  \item{...}{passed on directly to all of the ggplot2
  commands}
}
\value{
  returns a ggplot2 object that can be plotted directly or
  used as base layer for additional modifications.
}
\description{
  \code{\link{ggparallel}} implements and combines
  different types of parallel coordinate plots for
  categorical data: hammock plots, parallel sets plots,
  common angle plots, and common angle plots with a
  hammock-like adjustment for line widths.
}
\details{
  Parallel sets have been suggested by \cite{Kosara et al.
  (2006)} as a visualization technique to incorporate
  categorical variables into a parallel coordinate plot
  introduced by \cite{Wegman (1990)} and \cite{Inselberg
  (1985)}. The parallel sets implemented here are reduced
  to representations of neighboring two-dimensional
  relationships only rather than the hierarchical version
  originally suggested. Both versions, however, show
  perceptual problems with interpreting line widths,
  leading to potentially wrong conclusions about the data.
  The hammock display, introduced by \cite{Schonlau
  (2003)}, and the common angle plots are two approaches at
  fixing this problem: in Hammock plots the linewidth is
  adjusted by a factor countering the strength of the
  illusion, in the common angle plot all lines are adjusted
  to show the same angle - making line widths again
  comparable across ribbons. Additionally, we can also
  adjust ribbons in the common angle display for the angle,
  to make them appear having the same width (or height)
  across the display. We refer to this method as
  \code{adj.angle}.
}
\examples{
data(mtcars)
ggparallel(list("gear", "cyl"), data=mtcars)
ggparallel(list("gear", "cyl"), data=mtcars, method="hammock")

## combination of common angle plot and hammock adjustment:
ggparallel(list("gear", "cyl"), data=mtcars, method="adj.angle")

## compare with method='parset'
ggparallel(list("gear", "cyl"), data=mtcars, method='parset')

## flip plot and rotate text
ggparallel(list("gear", "cyl"), data=mtcars, text.angle=0) + coord_flip()

## change colour scheme
ggparallel(list("gear", "cyl"), data=mtcars, text.angle=0) + coord_flip() +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1")

## example with more than two variables:
titanic <- as.data.frame(Titanic)
ggparallel(names(titanic)[c(1,4,2,1)], order=0, titanic, weight="Freq") +
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")

## hammock plot with same width lines
ggparallel(names(titanic)[c(1,4,2,3)], titanic, weight=1, asp=0.5, method="hammock", ratio=0.2, order=c(0,0)) +
theme( legend.position="none") +
scale_fill_brewer(palette="Paired") +
scale_colour_brewer(palette="Paired")

## hammock plot with line widths adjusted by frequency
ggparallel(names(titanic)[c(1,4,2,3)], titanic, weight="Freq", asp=0.5, method="hammock", order=c(0,0)) +
theme( legend.position="none")

\dontrun{
## biological examples: genes and pathways
data(genes)
genes$chrom <- factor(genes$chrom, levels=c(paste("chr", 1:22, sep=""), "chrX", "chrY"))
ggparallel(list("path", "chrom"), text.offset=c(0.03, 0,-0.03), data = genes,  width=0.1, order=c(1,0), text.angle=0, color="white",
   factorlevels =  c(sapply(unique(genes$chrom), as.character),
     unique(genes$path))) +
   scale_fill_manual(values = c(   brewer.pal("YlOrRd", n = 9), rep("grey80", 24)), guide="none") +
   scale_colour_manual(values = c(   brewer.pal("YlOrRd", n = 9), rep("grey80", 24)), guide="none") +
   coord_flip()
}
}
\references{
  Kosara R, Bendix F and Hauser H (2006). "Parallel Sets:
  Interactive Exploration and Visual Analysis of
  Categorical Data." _IEEE Transactions on Visualization
  and Computer Graphics_, *12*(4), pp. 558-568. ISSN
  1077-2626, <URL: http://dx.doi.org/10.1109/TVCG.2006.76>,
  <URL: http://dx.doi.org/10.1109/TVCG.2006.76>.

  Wegman EJ (1990). "Hyperdimensional Data Analysis Using
  Parallel Coordinates." _Journal of the American
  Statistical Association_, *85*(411), pp. pp. 664-675.
  ISSN 01621459, <URL:
  http://www.jstor.org/stable/2290001>.

  Inselberg A (1985). "The Plane with Parallel
  Coordinates." _The Visual Computer_, *1*(2), pp. 69-91.

  Schonlau M (2003). "Visualizing Categorical Data Arising
  in the Health Sciences Using Hammock Plots." In
  _Proceedings of the Section on Statistical Graphics_.
  RAND Corporation.
}

