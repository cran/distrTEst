\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,Evaluation,missing-method}
\alias{plot,EvaluationList,missing-method}
\title{ Methods for Function plot in Package `distrTEst' }
\description{plot-methods}
\section{Methods}{\describe{
\item{plot}{\code{signature(x = "Evaluation", y="missing")}: returns a boxplot of the result }
\item{plot}{\code{signature(x = "EvaluationList", y="missing")}: regroups the list according to
the different columns/coordinates of the result of the evaluation; for each such coordinate
a boxplot is generated containing possibly several procedures and if evaluated at a \code{Contsimulation}
object also grouped into evaluations on ideal and real data. The \code{plot}-arguments \code{main}
and \code{ylim} may be transmitted coordinatewise 
(i.e.; a vector of length (result-dimension), respectively a 2 x (result-dimension) matrix)
or globally, using the usual recycling rules.}
}}
\value{An S3 object of class \code{c("plotInfo","DiagnInfo")}, i.e., a list
  containing the information needed to produce the
  respective plot, which at a later stage could be used by different
  graphic engines (like, e.g. \code{ggplot}) to produce the plot
  in a different framework. A more detailed description will follow in
  a subsequent version.
}
\concept{S4 evaluation class}
\keyword{methods}
\keyword{hplot}
\concept{boxplot}
