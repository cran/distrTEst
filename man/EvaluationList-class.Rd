\name{EvaluationList-class}
\docType{class}
\alias{Elist}
\alias{EvaluationList}
\alias{EvaluationList-class}
\alias{initialize,EvaluationList-method}
\alias{savedata,EvaluationList-method}
\alias{name,EvaluationList-method}
\alias{name<-,EvaluationList-method}
\alias{Elist,EvaluationList-method}

\title{Class "EvaluationList" }
\description{Several objects of class "Evaluation" may be gathered in a list of class "EvaluationList", if
they all have the same result-format and also share the same data-set. }
\section{Objects from the Class}{
Objects may be created by the generating function \code{EvaluationList}, i.e.; 
\code{EvaluationList(...,name0="a list of \"Evaluation\" objects")}, where all arguments passed through \dots 
      have to be objects of class "Evaluation" name, the corresponding result-slots have to contain 
      data.frames of identical dimension; the corresponding calls have to have identical \code{object}-arguments 
      (for the data set), and the corresponding \code{Data}-slots have to identical.}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}: the name of the EvaluationList object}
    \item{\code{Elist}:}{Object of class \code{"list"}: the list of Evaluation objects}
   }
}
\section{Accesor/Replacement methods}{
  \describe{
    \item{Elist}{\code{signature(object = "EvaluationList")}: returns the list with the Evaluation objects}
    \item{name}{\code{signature(object = "EvaluationList")}: returns/modifies the name of the EvaluationList object }
  }
}
\section{Methods}{
  \describe{
    \item{Data}{\code{signature(object = "EvaluationList")}: returns the common \code{Data}-slot 
                 of one of the Evaluation objects}
    \item{plot}{\code{signature(object = "EvaluationList")}: returns grouped boxplots of the results }
    \item{print}{\code{signature(object = "EvaluationList")}: for each list element returns 
    the name of the data object, its filename, the estimator used and the result }
    \item{show}{\code{signature(object = "EvaluationList")}: as print}
    \item{summary}{\code{signature(object = "EvaluationList")}: returns the name of the data object, its filename, the
    estimator used and a statistical summary of the result }
  }
}

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
\code{\link{Evaluation-class}}
\code{\link{print-methods}}
\code{\link{plot-methods}}
\code{\link[distrSim]{simulate-methods}}
\code{\link{summary-methods}}
}
\examples{
N <- Norm() # N is a standard normal distribution.
C <- Cauchy() # C is a Cauchy distribution
cs <- Contsimulation(filename = "csim",
                     runs = 15,
                     samplesize=500,
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
# saves the EvaluationList with result as "csim.mean" and without result as
# "csim.mean.comment" in the working directory # of R - "csim" is the
# filename of the Contsimulation object, mean the name of the estimator
rm(ev1)
cload("csim.mean")
# loads the EvaluationList without result - the object is called ev1.comment
ev1.comment
load("csim.mean") # loads the EvaluationList with result
ev1
ElistObj <- EvaluationList(ev1,ev2,name0="myEvalList")
plot(ElistObj,ylim=matrix(c(-0.5,0.5,0.5,4),nrow=2),main=c("location","scale"))
plot(ElistObj,ylim=c(-0.5,0.5),main=c("location"),runs0=3:12,dims0=1,evals0=2)
ElistObj
summary(ElistObj)
}
\keyword{list}
\keyword{manip}
\concept{S4 evaluation class}
