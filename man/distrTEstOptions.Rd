\name{distrTEstoptions}
\alias{distrTEstoptions}
\alias{getdistrTEstOption}
\alias{MaxNumberofPlottedEvaluationDims}
\alias{MaxNumberofPlottedEvaluations}
\alias{MaxNumberofSummarizedEvaluationDims}
\alias{MaxNumberofSummarizedEvaluations}

\title{
  functions to change the global variables of the package `distrTEst'
}
\description{
  With \code{distrTEstoptions} and \code{getdistrTEstOption} you may inspect and change the global
  variables used by package \pkg{distrTEst}.
}
\usage{
distrTEstoptions(...)
getdistrTEstOption(x)
}
\arguments{
  \item{\dots}{ any options can be defined, using name = value or by passing a list of such tagged values. }
  \item{x}{ a character string holding an option name.}
}
\details{Invoking \code{distrTEstoptions()} with no arguments returns a list with the current values of the options.  
 To access the value of a single option, one should use \code{getdistrTEstOption("MaxNumberofSummarizedEvaluations")}, 
 e.g., rather than \code{distroptions("MaxNumberofSummarizedEvaluations")} which is a \emph{list} of length one.}
\value{
\code{distrTEstoptions()} returns a list of the global options of \pkg{distrTEst}. \cr
\code{distrTEstoptions("MaxNumberofSummarizedEvaluations")} returns  the global option \code{MaxNumberofSummarizedEvaluations} as a list of length 1. \cr
\code{distrTEstoptions("MaxNumberofSummarizedEvaluations" = 3)} sets the value of the global option  \code{MaxNumberofSummarizedEvaluations} to 3. 
\code{getdistrTEstOption("MaxNumberofSummarizedEvaluations")} the current value set for option \code{MaxNumberofSummarizedEvaluations}. 
}

\section{Currently available options}{
\describe{
  \item{MaxNumberofPlottedEvaluations}{ maximal number of evaluations plotted; defaults to 6}
  \item{MaxNumberofPlottedEvaluationDims}{ maximal number of evaluation dimensions plotted in parallel; defaults to 6}
  \item{MaxNumberofSummarizedEvaluations}{ maximal number of evaluations summarized in parallel; defaults to 15 }
  \item{MaxNumberofPrintedEvaluations}{ maximal number of evaluations printed in parallel; defaults to 15 }
}
}
%\references{}
\author{Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}}
%\note{}
\seealso{\code{\link[base]{options}}, \code{\link[base]{getOption}},
\code{\link[distr]{distroptions}}, \code{\link[distr]{getdistrOption}}}
\examples{
distrTEstoptions()
distrTEstoptions("MaxNumberofPlottedEvaluationDims")
distrTEstoptions("MaxNumberofPlottedEvaluationDims" = 5)
# or
getdistrTEstOption("MaxNumberofPlottedEvaluationDims")
}
\keyword{misc}
\concept{global options}
\concept{options}
