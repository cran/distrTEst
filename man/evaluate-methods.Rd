\name{evaluate-methods}
\docType{methods}
\alias{evaluate-methods}
\alias{evaluate}
\alias{evaluate,Dataclass,function-method}
\alias{evaluate,Contsimulation,function-method}
\title{ Methods for Function evaluate in Package `distrTEst'}
\description{evaluate-methods to produce objects of class "Evaluation"}
\section{Methods}{\describe{
    \item{evaluate}{\code{signature(object = "Dataclass", estimator = "function")}:
      creates an object of class "Evaluation", see there for further
      information}          
    \item{evaluate}{\code{signature(object = "Contsimulation", estimator = "function")}:
      creates an object of class "Evaluation", see there for further
      information}}}          
\details{\preformatted{
besides the arguments determining the method dispatch, we have:

evaluate(object, estimator, ..., resname = "res", 
                       name = as.character(substitute(object)),
                       filename = filename(object))

}}
\arguments{
\item{object}{the data set / simulation on which the evaluation takes place}
\item{estimator}{the estimation function used; should be able to deal with data in matrix 
                 form \code{samplesize} x \code{obsDim}, and, should return either a 
                 univariate result or a vector (with named coordinates, if possible).}
\item{resname}{(a vector of) character(s); the name for the univariate results or,
                in the case of multivariate results, and if the coordinates of the results
                have not yet been named, the basic name for them which is pasted to
                the coordinate number for each coordinate. }
\item{name}{character; the name for the Evaluation object; by default the (R-)name of the Data 
                set object. }
\item{filename}{character; the filename for the Evaluation object (where it is to be saved to); 
                by default the filename of the Data set object which is concatenated with
                the name of the estimator in \code{\link{savedata}}. }
}
\seealso{\code{\link{Evaluation-class}}}
\concept{S4 evaluation class}
\keyword{methods}
