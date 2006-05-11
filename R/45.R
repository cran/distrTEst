## Slots in Evaluation

## name        - name of Dataclass object, which was called by evaluate
## filename    - filename of this object
## call.ev     - call which created the object, e.g.evalate(contsim, mean)
## result      - result of estimation on data
## estimator   - used estimation function

setClassUnion("numericorNULL", c("numeric","NULL"))


setClass("Evaluation",
         representation(name="character",
                        filename="character",
                        call.ev="call",result="numericorNULL",
                        estimator="OptionalFunction")
         )

## Access methods
if(!isGeneric("name")) setGeneric("name", function(object) standardGeneric("name"))
if(!isGeneric("filename")) setGeneric("filename", function(object) standardGeneric("filename"))
if(!isGeneric("call.ev")) setGeneric("call.ev", function(object) standardGeneric("call.ev"))
if(!isGeneric("result")) setGeneric("result", function(object) standardGeneric("result"))
if(!isGeneric("estimator")) setGeneric("estimator", function(object) standardGeneric("estimator"))
setMethod("name", "Evaluation", function(object) object@name)
setMethod("filename", "Evaluation", function(object) object@filename)
setMethod("call.ev", "Evaluation", function(object) object@call.ev)
setMethod("result", "Evaluation", function(object) object@result)
setMethod("estimator", "Evaluation", function(object) object@estimator)

## Initialization method
setMethod("initialize", "Evaluation",
          function(.Object, name, filename, call.ev, result, estimator) {
            .Object@name <- name
            .Object@filename <- filename
            .Object@call.ev <- call.ev
            .Object@result <- result
            .Object@estimator <- estimator        
            .Object
          })

if(!isGeneric("evaluate")) setGeneric("evaluate", function(object, estimator, ...) standardGeneric("evaluate"))

## different runs in different lines of data matrix

setMethod("evaluate", signature(object = "Dataclass", estimator = "function"), function(object, estimator, ...){
  if(is.null(Data(object)))
    stop("No Data found -> simulate first")
  if(is.null(filename(object)))
    stop("filename of Dataclass object is NULL, hence results couldn't be saved to harddisk")
  if(filename(object) == "")
    stop("filename of Dataclass object is an empty string, hence results couldn't be saved to harddisk")
  call.ev <- sys.call()
  name <- as.character(substitute(object))
  filename <- filename(object)
  Data <- Data(object)
  if(!is.vector(Data)) result <- apply(X = Data, MARGIN = 1, FUN = estimator, ...)
  else result <- apply(X = as.matrix(Data), MARGIN = 2, FUN = estimator, ...)
  new("Evaluation", name = name, call.ev = call.ev, filename = filename,
      result = result, estimator = estimator)
})


###Plot

setMethod("plot","Evaluation",
          function(x,y=NULL,...){
            boxplot(result(x))
          }
          )


###summary

setMethod("summary","Evaluation",
          function(object,...){
            cat("name of Dataobject: ",name(object),"\n")
            cat("name of Datafile: ",filename(object),"\n")
            cat("estimator: ",as.character(call.ev(object))[3],"\n")
            cat("Result:\n")
            summary(result(object))
          }
          )

## Save method
#if(!isGeneric("savedata")) setGeneric("savedata", function(object) standardGeneric("savedata"))

setMethod("savedata", "Evaluation", 
          function(object){
            estimator <- as.character(call.ev(object))[3]
            file <- paste(filename(object), ".", estimator, sep="")
  
            eval.parent(substitute(save(object, file = file)))
  
            name <- as.character(substitute(object))
            namecomment <- paste(name,".comment",sep="")  
            commentfile <- paste(filename(object), ".", estimator, ".comment", sep="")
  
            eval.parent(parse(text=paste(namecomment," = ",name,sep=""))) 
            eval.parent(parse(text=paste(namecomment,"@result <- NULL",sep=""))) 
            eval.parent(parse(text=paste("save(",namecomment,", file = \"",commentfile,"\")",sep=""))) 
            eval.parent(parse(text=paste("rm(",namecomment,")",sep="")))
          })

## print Methode
setMethod("print","Evaluation",
          function(x,...){
            cat("An Evaluation Object\n")
            cat("name of Dataobject: ",name(x),"\n")
            cat("name of Datafile: ",filename(x),"\n")
            cat("estimator: ",as.character(call.ev(x))[3],"\n")
            cat("Result: ")
            cat(str(result(x)))
          })
