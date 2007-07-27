.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE) 
}

.distrTEstoptions <- list(
                      MaxNumberofPlottedEvaluationDims = 6,
                      MaxNumberofPlottedEvaluations = 6,
                      MaxNumberofSummarizedEvaluations = 15,
                      MaxNumberofPrintedEvaluations = 15)
  

.onAttach <- function(library, pkg)
{
  unlockBinding(".distrTEstoptions", asNamespace("distrTEst"))
buildStartupMessage(pkg="distrTEst", packageHelp=TRUE, library=library, 
# MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
VIGNETTE = gettext("Package \"distrDoc\" provides a vignette to this package as well as\nto several related packages; try vignette(\"distr\")."))
###
  invisible()
}
