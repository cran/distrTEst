\name{Evaluation-class}
\docType{class}
\alias{Evaluation-class}
\alias{initialize,Evaluation-method}
\alias{savedata,Evaluation-method}

\title{Class "Evaluation" }
\description{ When an estimator is used to data of the type "Dataclass" with the method evaluate, the result is an object
of class "Evaluation". }
\section{Objects from the Class}{
Objects could be created by calls of the form \code{new("Evaluation", name, filename, call.ev, result, estimator)}.
It does not seem to be very useful to generate a new object this way, however. Use "evaluate" with a Dataclass object!}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: the name of the Dataclass object, which was called by evaluate }
    \item{\code{filename}:}{Object of class \code{"character"}: the filename of the evaluated object }
    \item{\code{call.ev}:}{Object of class \code{"call.ev"} the call which created the object, 
      e.g. ``evaluate(Dataclassobject)'' }
    \item{\code{result}:}{Object of class \code{"vector"}: the result of the evaluation of the estimation on data }
    \item{\code{estimator}:}{Object of class \code{"OptionalFunction"}: estimation function used }
  }
}
\section{Methods}{
  \describe{
    \item{call.ev}{\code{signature(object = "Evaluation")}: returns the call which created the object }
    \item{estimator}{\code{signature(object = "Evaluation")}: returns the estimator }
    \item{filename}{\code{signature(object = "Evaluation")}: returns the filename }
    \item{initialize}{\code{signature(.Object = "Evaluation")}: initialize method }
    \item{name}{\code{signature(object = "Evaluation")}: returns the name of the data object }
    \item{result}{\code{signature(object = "Evaluation")}: returns the result }
    \item{plot}{\code{signature(object = "Evaluation")}: returns a boxplot of the result }
    \item{print}{\code{signature(object = "Evaluation")}: returns the name of the data object, its filename, the estimator used
    and the result }
    \item{savedata}{\code{signature(object = "Evaluation")}: saves the object in two files in the directory of R - one with
    data, one without as comment file (see example) }
    \item{summary}{\code{signature(object = "Evaluation")}: returns the name of the data object, its filename, the
    estimator used and a statistical summary of the result }
  }
}
\note{ The saved "evaluation" can be loaded with the usual load-command, the saved comment with the function cload. }

\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}
  }

\seealso{
\code{\link[distrSim]{Dataclass-class}}
\code{\link[distrSim]{Simulation-class}}
\code{\link[distrSim]{Contsimulation-class}}
\code{\link{load}}
\code{\link[distrSim]{cload}}
\code{\link[distrSim]{savedata-methods}}
\code{\link{plot-methods}}
\code{\link[distrSim]{simulate-methods}}
\code{\link{summary-methods}}
}
\examples{
N <- Norm() # N is a standard normal distribution.
C <- Cauchy() # C is a Cauchy distribution
cs <- Contsimulation(filename = "csim",
                     runs = 5,
                     samplesize=5000,
                     seed=setRNG(),
                     distribution.id = N,
                     distribution.c = C,
                     rate = 0.1)
simulate(cs)
# Each of the 25000 random numbers is ideal (N-distributed) with
# probability 0.9 and contaminated (C-distributed) with probability = 0.1
summary(cs)
ev1 <- evaluate(cs, mean) # estimates the data with mean
ev1 # bad results
ev2 <- evaluate(cs,median) # estimates the data with median
ev2 # better results because median is robust
savedata(ev1)
# saves the evaluation with result as "csim.mean" and without result as
# "csim.mean.comment" in the working directory # of R - "csim" is the
# filename of the Contsimulation object, mean the name of the estimator
rm(ev1)
cload("csim.mean")
# loads the evaluation without result - the object is called ev1.comment
ev1.comment
load("csim.mean") # loads the evaluation with result
ev1
}
\keyword{}
