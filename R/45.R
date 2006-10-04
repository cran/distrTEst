## Slots in Evaluation

## name        - name of Dataclass object, which was called by evaluate
## filename    - filename of this object
## call.ev     - call which created the object, e.g.evalate(contsim, mean)
## result      - result of estimation on data
## estimator   - used estimation function

setClassUnion("numericorNULL", c("numeric","NULL"))
setClassUnion("DataframeorNULL", c("NULL","data.frame"))
setClassUnion("CallorNULL", c("NULL","call"))


setClass("Evaluation",
         representation(name = "character",
                        filename = "character",
                        call.ev = "CallorNULL",
                        Data = "Dataclass", ### new 041006
                        result = "DataframeorNULL", ### new 041006
                        estimator = "OptionalFunction"),
         )

setClass("EvaluationList",
         representation(name = "character", Elist = "list"),
         prototype = prototype(name = "a list of Evaluation objects",
                               Elist = list(new("Evaluation"))),
         validity = function(object){
             len <- length(object@Elist)
             if (len > 1)
             { classes <- unlist(lapply(object@Elist, function(x)class(x)[[1]]))
               if(!all(classes=="Evaluation"))
                 stop("all list elements have to be objects of class \"Evaluation\"")      
               dimes <- matrix(unlist(lapply(object@Elist,function(x)dim(result(x)))),ncol=2,byrow=TRUE)
               if(!all(apply(dimes,2,function(x) all(x==x[1]))))
                  stop("the result slots of all list elements have to be of the same dimension")      
               if(!all(lapply(object@Elist,function(x)identical(x@call.ev$object,object@Elist[[1]]@call.ev$object))))
                  stop("the call slots of all list elements have to have the same object[=Data]-argument")      
               if((is(object@Elist[[1]]@Data,"Simulation"))|(is(object@Elist[[1]]@Data,"Contsimulation")))
                  {if(!all(lapply(object@Elist,function(x)identical(x@Data@seed,object@Elist[[1]]@Data@seed))))                 
                       stop("the seeds of the Data slots of all list elements have to coincide")      
                   if(is(object@Elist[[1]]@Data,"Contsimulation"))
                       {if(!all(lapply(object@Elist,
                                       function(x)identical(body(x@Data@distribution.id@p),
                                                            body(object@Elist[[1]]@Data@distribution.id@p)))))                 
                            stop("the ideal distribution of the Data slots of all list elements have to coincide")
                        if(!all(lapply(object@Elist,
                                       function(x)identical(body(x@Data@distribution.c@p),
                                                            body(object@Elist[[1]]@Data@distribution.c@p)))))                 
                            stop("the contaminating distribution of the Data slots of all list elements have to coincide")
                        }      
                   else
                        if(!all(lapply(object@Elist,
                                       function(x)identical(body(x@Data@distribution@p),
                                                            body(object@Elist[[1]]@Data@distribution@p)))))                 
                            stop("the distribution of the Data slots of all list elements have to coincide")      
                  
                 }
               else   
                  {if(!all(lapply(object@Elist,function(x)identical(x@Data,object@Elist[[1]]@Data))))
                       stop("the Data slots of all list elements have to coincide")}
              }               
              return(TRUE) }
         )


EvaluationList <- function(...,name0="a list of \"Evaluation\" objects")
        return(new("EvaluationList",Elist=list(...),name=name0))

## Access methods
if(!isGeneric("call.ev")) setGeneric("call.ev", function(object) standardGeneric("call.ev"))
if(!isGeneric("result")) setGeneric("result", function(object) standardGeneric("result"))
if(!isGeneric("estimator")) setGeneric("estimator", function(object) standardGeneric("estimator"))
if(!isGeneric("Elist")) setGeneric("Elist", function(object) standardGeneric("Elist"))

setMethod("name", "Evaluation", function(object) object@name)
setMethod("Data", "Evaluation", function(object) object@Data)
setMethod("filename", "Evaluation", function(object) object@filename)
setMethod("call.ev", "Evaluation", function(object) object@call.ev)
setMethod("result", "Evaluation", function(object) object@result)
setMethod("estimator", "Evaluation", function(object) object@estimator)

setMethod("Elist", "EvaluationList", function(object) object@Elist)
setMethod("name", "EvaluationList", function(object) object@name)
setMethod("Data", "EvaluationList", function(object) (object@Elist[[1]])@Data)

setReplaceMethod("name", "Evaluation", function(object, value){ object@name <- value; object}) ### new 1.8
setReplaceMethod("filename", "Evaluation", function(object, value){ object@filename <- value; object}) ### new 1.8

setReplaceMethod("name", "EvaluationList", function(object, value){ object@name <- value; object}) ### new 1.8

## Initialization method
setMethod("initialize", "Evaluation",
          function(.Object, name = NULL, filename = NULL, call.ev = NULL, result = NULL, 
                    estimator, Data) {
             if(missing(Data)) Data <- Simulation(); 
             if(missing(estimator)) estimator <- mean; 
             if(is.null(name)) 
                {if(!is.null(name(Data))) 
                    name <- name(Data)
                 else
                    name <- "Default-name"}    
             if(is.null(filename)) 
                {if(!is.null(filename(Data))) 
                     filename <- filename(Data)
                 else     
                     filename <- name}
            .Object@name <- name
            .Object@filename <- filename
            .Object@call.ev <- call.ev
            .Object@result <- result
            .Object@estimator <- estimator
            .Object@Data <- Data        
            .Object
          })


if(!isGeneric("evaluate")) setGeneric("evaluate", function(object, estimator, ...) standardGeneric("evaluate"))

setMethod("evaluate", signature(object = "Dataclass", estimator = "function"), 
              function(object, estimator, ..., resname = NULL,  
                   name = NULL, filename = NULL){
  if(is.null(Data(object)))
    stop("No Data found -> simulate first")
  ## is it a Simulation?
  object0 <- object
  if(!is(try(slot(object,"rate"),silent=TRUE),"try-error"))
     {object0@Data <- NULL}
  if(is.null(filename(object)))
    stop("filename of Dataclass object is NULL, hence results couldn't be saved to harddisk")
  if(filename(object) == "")
    stop("filename of Dataclass object is an empty string, hence results couldn't be saved to harddisk")
  Data <- Data(object)
  if (is.null(name)) name <- as.character(substitute(object))
  if (is.null(filename)) filename <- filename(object)
  if (is.null(resname) && !is(object,"Simulation")) resname <- "res"
  if (is.null(resname)) resname <- as.character(match.call(call=sys.call(-1))$estimator)
  ### new 04-10-06
  result <- .convert.result.format(apply(X = Data, MARGIN = 3, FUN = estimator, ...), resname=resname)
  # old
  # old  if(!is.vector(Data)) result <- apply(X = Data, MARGIN = 1, FUN = estimator, ...)
  # old     else result <- apply(X = as.matrix(Data), MARGIN = 2, FUN = estimator, ...)
  new("Evaluation", name = name, call.ev = match.call(call=sys.call(-1)), filename = filename,
      result = result, estimator = estimator, Data = object0)
}) 
           
           


### new 04-10-06
setMethod("evaluate", signature(object = "Contsimulation", estimator = "function"), 
          function(object, estimator, ..., resname = NULL,  
                   name = NULL, filename = NULL){
  if(is.null(Data.id(object)))
    stop("No Data found -> simulate first")
  ## is it a Simulation?
  object0 <- object
  if(!is(try(slot(object,"rate"),silent=TRUE),"try-error"))
     {object0@Data.id <- NULL
      object0@Data.c <- NULL
      object0@Data <- NULL
      object0@ind <- NULL}
  if(is.null(filename(object)))
    stop("filename of Dataclass object is NULL, hence results couldn't be saved to harddisk")
  if(filename(object) == "")
    stop("filename of Dataclass object is an empty string, hence results couldn't be saved to harddisk")
  if (is.null(name)) name <- as.character(substitute(object))
  if (is.null(filename)) filename <- filename(object)
  if (is.null(resname)) resname <- as.character(match.call(call=sys.call(-1))$estimator)

  Data.id <- Data.id(object); 
  Data.re <- Data(object)

  result.id <- .convert.result.format(apply(X = Data.id, MARGIN = 3, FUN = estimator, ...), resname=resname)
  result.re <- .convert.result.format(apply(X = Data.re, MARGIN = 3, FUN = estimator, ...), resname=resname)

  names(result.id) <- paste(names(result.id),"id",sep=".")
  names(result.re) <- paste(names(result.re),"re",sep=".")
  new("Evaluation", name = name, call.ev = match.call(call=sys.call(-1)), filename = filename,
       result = as.data.frame(cbind(result.id, result.re)), 
       estimator =  estimator, Data = object0)
})


.convert.result.format<-function(x, resname="res"){
  if(is.list(x))
     {rdim <- length(x)
      cnames <- names(x[[1]])
      if(is.null(cnames)) 
          cnames <- paste(abbreviate(resname),1:cdim,sep=".")
      else
          cnames <- paste(abbreviate(resname),abbreviate(names(x[[1]])),sep=".")
      x <- data.frame(matrix(unlist(x),nrow=rdim, byrow=TRUE))
      colnames(x) <- cnames
     }     
  else if(is.matrix(x))
     {x <- t(x)
      cdim <- ncol(x)
      cnames <- colnames(x)
      if(is.null(cnames)) 
          cnames <- paste(abbreviate(resname),1:cdim,sep=".")
      else
          cnames <- paste(abbreviate(resname),abbreviate(names(x[[1]])),sep=".")
      x <- data.frame(x)
      colnames(x) <- cnames
     }     
  else if (!is.data.frame(x))
     {x <- data.frame(x)
      names(x) <- abbreviate(resname)
     }     
  return(x)
}

###Plot

setMethod("plot","Evaluation",
          function(x,y=NULL, 
                   runs0=1:nrow(result(x)), dims0=1:ncol(result(x)),  ...        
          ){
            dots <- list(...)
            ldim0 <- min(getdistrTEstOption("MaxNumberofPlottedEvaluationDims"), length(dims0))           
            if(ldim0<length(dims0))
                warning(paste("your evaluation is too big; only ", lobs, 
                              "evaluation dimensions are plotted"))                        
            boxplot(result(x)[runs0,dims0[1:ldim0]],...)
          }
          )

setMethod("plot","EvaluationList", 
           function(x,y=NULL,                    
                    runs0=1:nrow(result(Elist(x)[[1]])), 
                    dims0= 1: ifelse(sum(grep("\\.id($|\\.)",colnames(result(Elist(x)[[1]]))))>0,
                                     ### contaminated data or not? 
                                     ncol(result(Elist(x)[[1]]))/2, ncol(result(Elist(x)[[1]]))),
                    evals0=1:length(Elist(x)), ... ) 
{ dots <- list(...)

  ldim0 <- min(getdistrTEstOption("MaxNumberofPlottedEvaluationDims"), length(dims0))           
  levals0 <- min(getdistrTEstOption("MaxNumberofPlottedEvaluations"), length(evals0))           
  
  if((ldim0<length(dims0))||(levals0<length(evals0)))
                warning(paste("your evaluation list is too big; only ", levals, "x" ,lobs, 
                              "evaluations x dimensions are plotted"))                        

  evallist <- Elist(x)
  len <- length(evallist)
  resdim <- ncol(result(evallist[[1]]))
  nl <- nrow(result(evallist[[1]]))

  # the names for the different columns[1..resdim] of the different Evaluations[1..len]
  resnames <- matrix(0,len,resdim)
  for(i in 1:len)
      resnames[i,] <- colnames(result(evallist[[i]]))

  ### is the data split into ideal/contaminated data?
  id0 <-grep("\\.id($|\\.)",resnames[1,])

  evallist0 <- Elist(x)[evals0[1:levals0]]
  len0 <- length(evallist0)
    
  if (sum(id0 > 0))
  
     {dims1 <- c(dims0[1:ldim0],dims0[1:ldim0]+resdim/2)
      resdim0 <- ncol(result(evallist0[[1]])[runs0,dims1])
      nl0 <- nrow(result(evallist0[[1]])[runs0,dims1])
     }
  
  else

     {dims1 <- dims0[1:ldim0]
      resdim0 <- ncol(result(evallist0[[1]])[runs0,dims1])
      nl0 <- nrow(result(evallist0[[1]])[runs0,dims1])
     }
       
  ma <- data.frame(matrix(0,nl0,len0*resdim0))
  
  ## reorganize the frames 
  ### --- grouped by result-dimension (e.g. coordinates of an estimator)
  ###     and within this --- if possible --- grouped by ideal/real Data
  if( sum(id0 > 0)) 
     { for(i in 1:(resdim0/2))
           for(j in 1:len0)
             
              {ma[,(i-1)*2*len0+j] <- result(evallist0[[j]])[runs0,dims1[i]]
               ma[,(2*i-1)*len0+j] <- result(evallist0[[j]])[runs0,dims1[i+resdim0/2]]
               colnames(ma)[(i-1)*2*len0+j]<-colnames(result(evallist0[[j]]))[dims1[i]]
               colnames(ma)[(2*i-1)*len0+j]<-colnames(result(evallist0[[j]]))[dims1[i+resdim0/2]]
               }
       resdim0 <- resdim0/2; len0 <- len0*2 
     }
  else
     { for(i in 1:resdim0)
           for(j in 1:len0)
              {ma[,(i-1)*len0+j] <- result(evallist0[[j]])[runs0,i]
               colnames(ma)[(i-1)*len0+j]<-colnames(result(evallist0[[j]]))[i]
               }
     }

  main0 <- character(resdim0)
  if("main" %in% names(dots)) 
      { oldwarn <- getOption("warn"); options("warn" = -1)
        main0[1:resdim0] <- dots[["main"]]
        options("warn" = oldwarn) }
  else 
      main0 <- paste(gettextf("%d. coordinate",dims1[1:resdim0]))  
  
  ylim0<-matrix(0,2,resdim0)
  if("ylim" %in% names(dots)) 
      { oldwarn <- getOption("warn"); options("warn" = -1)
        ylim1 <- as.matrix(dots[["ylim"]])
        c1 <- ncol(ylim1); c2 <- resdim0%/%c1; c3 <- resdim0%%c1
        if(c2>0)
           ylim0[,1:(c2*c1)] <- ylim1
        if(c3>0)
           ylim0[,c2*c1+(1:c3)]<- ylim1[,1:c3]
        options("warn" = oldwarn) }  
  
  op <- par()$mfrow
  par(mfrow=c(resdim0,1))
       
  
  for(i in 1:resdim0)
      {if("main" %in% names(dots)) 
          dots[["main"]] <- main0[i]
       else {
          dots[[length(dots)+1]] <- main0[i] 
          names(dots)[length(dots)] <- "main"
            }
       if("ylim" %in% names(dots)) 
          dots[["ylim"]] <- ylim0[,i]

       dots[["x"]]<- as.data.frame(ma[,(i-1)*len0+(1:len0)])
       do.call("boxplot", args=dots)
 
      } 
   par(mfrow=op)
   return(invisible())
})

###summary

setMethod("summary","Evaluation",
          function(object, runs0=1:nrow(result(object)), dims0=1:ncol(result(object)),  
                   inList=FALSE, ...){
            cat(gettextf("name of Evaluation: %s\n",name(object)))
            if(!inList)
               {cat(gettextf("name of Dataobject: %s\n",name(Data(object))))
                cat(gettextf("name of Datafile: %s\n",filename(object)))}
            cat(gettextf("estimator: %s\n",as.character(call.ev(object)$estimator)))
            cat(gettext("Result:\n")); 
            print(summary(result(object)[runs0,dims0])) ### still do not see why necessary...
          }
          )


setMethod("summary","EvaluationList",
          function(object, runs0=1:nrow(result(Elist(object)[[1]])), dims0=1:ncol(result(Elist(object)[[1]])), 
                   evals0=1:length(Elist(object)),...){

            levals0 <- min(getdistrTEstOption("MaxNumberofSummarizedEvaluations"), length(evals0))           
  
            if(levals0<length(evals0))
                warning(paste("your evaluation list is too big; only ", levals, 
                              "evaluations are printed"))                        

            cat(gettextf("name of Evaluation List: %s\n",name(object)))
            if(!is.null(name(Elist(object)[[1]]))) cat(gettextf("name of Dataobject: %s\n",name(Elist(object)[[1]])))
            if(!is.null(filename(Elist(object)[[1]]))) cat(gettextf("name of Datafile: %s\n",filename(Elist(object)[[1]])))
            len <- length(Elist(object)[evals0[1:levals0]])
            for(i in 1:len)
                {cat("----------------------------------\n")
                 summary(Elist(object)[[evals0[i]]],runs0=runs0, dims0=dims0, inList=TRUE)}
          }
          )

## Save method
#if(!isGeneric("savedata")) setGeneric("savedata", function(object) standardGeneric("savedata"))

setMethod("savedata", "Evaluation", 
          function(object, estimatorName = NULL, fileN = NULL, ...){
            
            name0 <- as.character(match.call(call=sys.call(-1))$object)
            if(is.null(estimatorName)) 
               (estimatorName <- as.character(call.ev(object)$estimator))
            if(is.null(fileN)) 
               {if(is.null(filename(object))) 
                    stop("This Dataclass object has to be given a filename before it can be saved to harddisk")
                fileN <- paste(filename(object), ".", estimatorName, sep="")}
            
#            save(object, file = fileN)
            eval.parent(parse(text=paste("save(", name0,", file = \"",fileN,"\")",sep="")))
 
            namecomment <- paste(name0, ".comment", sep="")  
            commentfile <- paste(fileN, ".comment", sep="")
            
            eval.parent(parse(text=paste(namecomment," <- ",name0,sep=""))) 
            eval.parent(parse(text=paste(namecomment,"@result <- NULL",sep=""))) 
            eval.parent(parse(text=paste("save(",namecomment,", file = \"",commentfile,"\")",sep=""))) 
            eval.parent(parse(text=paste("rm(",namecomment,")",sep="")))
            print(fileN)
            print(name0)
            print(commentfile)
            print(namecomment)
          })

## print Methode
setMethod("print","Evaluation",
          function(x, runs0=1:nrow(result(x)), dims0=1:ncol(result(x)), inList=FALSE, ...){
            cat(gettextf("An Evaluation Object\n"))
            if(!inList)
               {if(!is.null(name(x))) cat(gettextf("name of Dataobject: %s\n",name(x)))
                if(!is.null(filename(x))) cat(gettextf("name of Datafile: %s\n",filename(x)))
               } 
            if(!is.null(call.ev(x))) cat(gettextf("estimator: %s\n",as.character(call.ev(x)$estimator)))
            if(!is.null(result(x))) {cat(gettextf("Result: "))
                                      cat(str(result(x)[runs0,dims0]))}
          })

setMethod("print","EvaluationList",
          function(x, runs0=1:nrow(result(Elist(x)[[1]])), dims0=1:ncol(result(Elist(x)[[1]])), 
                   evals0=1:length(Elist(x)),...){

            levals0 <- min(getdistrTEstOption("MaxNumberofPrintedEvaluations"), length(evals0))           
  
            if(levals0<length(evals0))
                warning(paste("your evaluation list is too big; only ", levals, 
                              "evaluations are printed"))                        

            cat(gettextf("An EvaluationList Object\n"))
            if(!is.null(name(x))) cat(gettextf("name of Evaluation List: %s\n",name(x)))
            if(!is.null(name(Elist(x)[[1]]))) cat(gettextf("name of Dataobject: %s\n",name(Elist(x)[[1]])))
            if(!is.null(filename(Elist(x)[[1]]))) cat(gettextf("name of Datafile: %s\n",filename(Elist(x)[[1]])))
            
            len <- length(Elist(x)[evals0[1:levals0]])
            for(i in 1:len)
                {cat("----------------------------------\n")
                 print(Elist(x)[[evals0[i]]], runs0=runs0, dims0=dims0, inList=TRUE)}
          }
          )

setMethod("show","Evaluation", function(object)print(object))
setMethod("show","EvaluationList", function(object)print(object))
