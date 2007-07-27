\name{savedata-methods}
\docType{methods}
\alias{savedata-methods}
\alias{savedata}
\alias{savedata,Evaluation-method}
\title{ Methods for Function savedata in Package `distrTEst' }
\description{savedata-methods}

\section{Methods}{\describe{

\item{savedata}{\code{signature(object = "Evaluation")}: saves the object in two files in the directory of R - one with
    data ---filename = <filename>---, one without as comment file ---filename = <filename>.comment---(see example); 
    <filename>the filename can be specified in the optional argument \code{fileN}; by default it is concatenated
    from the filename of the \code{Dataclass} object and the estimatorname, which you may either pass as argument
    \code{estimatorName} or it is taken as the R-name of the corresponding R-function.}
}}
\note{For an example, see \code{\link[distrSim]{Simulation-class}}
and \code{\link[distrSim]{Contsimulation-class}}
}

\seealso{
\code{\link[distrSim]{Dataclass-class}}
\code{\link[distrSim]{Simulation-class}}
\code{\link[distrSim]{Contsimulation-class}}
\code{\link{Evaluation-class}}
}
\keyword{methods}
\keyword{file}
\concept{save}
