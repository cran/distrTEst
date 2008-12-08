\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,Evaluation-method}
\alias{print,EvaluationList-method}
\alias{show-methods}
\alias{show,Evaluation-method}
\alias{show,EvaluationList-method}
\title{ Methods for Functions print and show in Package `distrTEst' }
\description{print/show-methods}

\section{Methods}{\describe{

\item{print}{\code{signature(x = "Evaluation")}: returns the name of the data object, its filename, the estimator used and the result;
optional arguments: 
   \describe{\item{runs0}{the indices of runs to be summarized;}
   \item{dims0}{the indices of result dimensions to be summarized;}} 
internal argument:
      \describe{\item{inList}{decides if name of Dataobject and Datafile are printed out 
      (which is done if \code{inLIST==FALSE}); defaults to \code{FALSE} but is
      TRUE when \code{summary} is called from \code{summary}-method for \code{signature(object = "EvaluationList")}}}
   }
\item{print}{\code{signature(object = "EvaluationList")}: after printing the name of the list, 
for each member of the list print is executed;
   optional arguments: 
   \describe{
   \item{eval0}{the indices of evaluations to be summarized;--- of this vector \code{eval0} 
   maximally \code{MaxNumberofSummarizedEvaluations} evaluations are summarized where \code{MaxNumberofPrintedEvaluations} 
   is a global option, see \code{\link[distrTEst]{distrTEstoptions}}}
   \item{runs0}{the indices of runs to be summarized;}
   \item{dims0}{the indices of observation dimensions to be summarized;}}
      }
\item{show}{\code{signature(x = "Evaluation")}: the same as \code{print} (without optional arguments)}
\item{show}{\code{signature(x = "EvaluationList")}: the same as \code{print} (without optional arguments)}
}}
\concept{S4 evaluation class}
\keyword{methods}
\keyword{print}
