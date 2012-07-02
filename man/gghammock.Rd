\name{gghammock}
\alias{gghammock}
\title{Creating hammock plots and parallel sets}
\usage{
  gghammock(vars = list(), data, weight = NULL,
    alpha = 0.5, width = 0.25, order = 1, color = NA,
    label = TRUE, angle = 90, text.offset = NULL...)
}
\arguments{
  \item{vars}{list of variable names to be included in the
  plotting. order of the variables is preserved in the
  display}

  \item{data}{data frame}

  \item{weight}{weighting variable - use character string}

  \item{alpha}{level of $\alpha$ blending, value between 0
  and 1}

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
}
\description{
  quick description
}
\details{
  more detailed description
}
\examples{
data(mtcars)
gghammock(list("gear", "cyl"), data=mtcars)
gghammock(list("cyl", "gear"), data=mtcars)
titanic <- as.data.frame(Titanic)
gghammock(list("Class", "Survived"), data=titanic, weight="Freq") + scale_fill_brewer(palette=6)
}

