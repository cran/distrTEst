pkgname <- "distrTEst"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('distrTEst')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Evaluation-class")
### * Evaluation-class

flush(stderr()); flush(stdout())

### Name: Evaluation-class
### Title: Class "Evaluation"
### Aliases: Evaluation-class initialize,Evaluation-method
### Keywords: manip

### ** Examples

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
ev1 <- evaluate(cs, mean, resname="mean") # estimates the data with mean
ev1 # bad results
ev2 <- evaluate(cs,median, resname="median") # estimates the data with median
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
plot(ev1)
#
#another function to be evaluated:
severalThings<- function(x) {list("mean"=mean(x),"sd"=sd(as.vector(x)), "mad"=mad(x))}
ev3 <- evaluate(cs, severalThings, resname="several") 
plot(ev3)
plot(ev3, ylim=c(0,10), col=c("blue","green", "red"))



cleanEx()
nameEx("EvaluationList-class")
### * EvaluationList-class

flush(stderr()); flush(stdout())

### Name: EvaluationList-class
### Title: Class "EvaluationList"
### Aliases: Elist EvaluationList EvaluationList-class
###   initialize,EvaluationList-method savedata,EvaluationList-method
###   name,EvaluationList-method name<-,EvaluationList-method
###   Elist,EvaluationList-method
### Keywords: list manip

### ** Examples

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



cleanEx()
nameEx("distrTEstoptions")
### * distrTEstoptions

flush(stderr()); flush(stdout())

### Name: distrTEstoptions
### Title: functions to change the global variables of the package
###   'distrTEst'
### Aliases: distrTEstoptions getdistrTEstOption
###   MaxNumberofPlottedEvaluationDims MaxNumberofPlottedEvaluations
###   MaxNumberofSummarizedEvaluationDims MaxNumberofSummarizedEvaluations
### Keywords: misc

### ** Examples

distrTEstoptions()
distrTEstoptions("MaxNumberofPlottedEvaluationDims")
distrTEstoptions("MaxNumberofPlottedEvaluationDims" = 5)
# or
getdistrTEstOption("MaxNumberofPlottedEvaluationDims")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
