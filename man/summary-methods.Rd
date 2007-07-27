\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,Evaluation-method}
\alias{summary,EvaluationList-method}
\title{ Methods for Function summary in Package `distrTEst' }
\description{summary-methods}

\section{Methods}{\describe{
   \item{summary}{\code{signature(object = "Evaluation")}: returns the name of the data object, its filename, the estimator used and
                  a statistical summary of the result;optional arguments: 
   \describe{\item{runs0}{the indices of runs to be summarized;}
   \item{dims0}{the indices of result dimensions to be summarized;}
   }
   internal argument:
      \describe{\item{inList}{decides if name of Dataobject and Datafile are printed out 
      (which is done if \code{inLIST==FALSE}); defaults to \code{FALSE} but is
      TRUE when \code{summary} is called from \code{summary}-method for \code{signature(object = "EvaluationList")}}
}}                
   \item{summary}{\code{signature(object = "EvaluationList")}: returns, for each member of the list a summary of 
   the corresponding Evaluation object;
   optional arguments: 
   \describe{
   \item{eval0}{the indices of evaluations to be summarized;--- of this vector \code{eval0} 
   maximally \code{MaxNumberofSummarizedEvaluations} evaluations are summarized where \code{MaxNumberofSummarizedEvaluations} 
   is a global option, see \code{\link[distrTEst]{distrTEstoptions}}}
   \item{runs0}{the indices of runs to be summarized;}
   \item{dims0}{the indices of observation dimensions to be summarized;}}
   }
}}
\concept{summary}
\keyword{methods}
\concept{S4 evaluation class}
