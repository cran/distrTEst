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
# next lines are taken from Valentin Todorov's package "rrcov"
#    ver <- read.dcf(file.path(library, pkg, "DESCRIPTION"), "Version")
#    ver <- as.character(ver)
#    title <- read.dcf(file.path(library, pkg, "DESCRIPTION"), "Title")
#    title <- as.character(title)
#    if((!getOption("StartupBanner")=="off")||is.null(getOption("StartupBanner"))) 
#       message(paste(title, " (version ", ver, ")\n", sep = ""))
#    msga <- gettext("For more information see ?\"distrTEst\", NEWS(\"distrTEst\"), and \n")
#    msgb <- gettext("    http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf .\n")
#    if((getOption("StartupBanner")=="complete")||is.null(getOption("StartupBanner"))) 
#       message(msga,msgb,sep=""); 
buildStartupMessage(pkg="distrTEst", packageHelp=TRUE, library=library, 
                    # MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
                    VIGNETTE=gettext("Package \"distrDoc\" provides a vignette to this package as well as\nto several related packages; try vignette(\"distr\")."))
###
  invisible()
}
