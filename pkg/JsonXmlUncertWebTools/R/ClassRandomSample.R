setClass(
  Class="RandomSample",
  representation=representation(realisations="list", samplingMethodDescription="character"),
  contains="Uncertainty"
  )

setMethod(
  f="initialize",
  signature="RandomSample",
  definition=function(.Object, Realisations, Desc=""){
    .Object@realisations<-Realisations
    .Object@samplingMethodDescription<-Desc
    return(.Object)
  }
  )

setGeneric(
  name="getRandomSampleMethodDesc",
  def=function(.Object) {standardGeneric("getRandomSampleMethodDesc")}
  )

setMethod(
  f="getRandomSampleMethodDesc",
  signature="RandomSample",
  definition=function(.Object){
    return(.Object@samplingMethodDescription)
  }
  )

setGeneric(
  name="getRandomSampleRealisations",
  def=function(.Object) {standardGeneric("getRandomSampleRealisations")}
  )

setMethod(
  f="getRandomSampleRealisations",
  signature="RandomSample",
  definition=function(.Object){
    return(.Object@realisations)
  }
  )

setGeneric(
  name="setRandomSampleMethodDesc<-",
  def=function(.Object, value) {standardGeneric("setRandomSampleMethodDesc<-")}
  )

setReplaceMethod(
  f="setRandomSampleMethodDesc",
  signature="RandomSample",
  definition=function(.Object, value){
    .Object@samplingMethodDescription<-value
    return(.Object)
  }
  )


setGeneric(
  name="addRandomSampleRealisations<-",
  def=function(.Object,value) {standardGeneric("addRandomSampleRealisations<-")}
  )

setReplaceMethod(
  f="addRandomSampleRealisations",
  signature="RandomSample",
  definition=function(.Object, value){
    .Object@realisations<-c(.Object@realisations, value)
    return(.Object)
  }
  )

setGeneric(
  name="getRandomSampleCount",
  def=function(.Object) {standardGeneric("getRandomSampleCount")}
  )

setMethod(
  f="getRandomSampleCount",
  signature="RandomSample",
  definition=function(.Object){
    return(length(.Object@realisations))
  }
  )

setGeneric(
  name="getRandomSampleMean",
  def=function(.Object) {standardGeneric("getRandomSampleMean")}
  )

setMethod(
  f="getRandomSampleMean",
  signature="RandomSample",
  definition=function(.Object){
    values<-c()
    count<-getRandomSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    return(mean(values))
  }
  )

setGeneric(
  name="getRandomSampleVariance",
  def=function(.Object) {standardGeneric("getRandomSampleVariance")}
  )

setMethod(
  f="getRandomSampleVariance",
  signature="RandomSample",
  definition=function(.Object){
    values<-c()
    count<-getRandomSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    return(var(values))  
  }
  )

setGeneric(
  name="getRandomSampleStandardDeviation",
  def=function(.Object) {standardGeneric("getRandomSampleStandardDeviation")}
  )

setMethod(
  f="getRandomSampleStandardDeviation",
  signature="RandomSample",
  definition=function(.Object){
    return(sqrt(getRandomSampleVariance(.Object)))  
  }
  )

setGeneric(
  name="getRandomSampleMedian",
  def=function(.Object) {standardGeneric("getRandomSampleMedian")}
  )

setMethod(
  f="getRandomSampleMedian",
  signature="RandomSample",
  definition=function(.Object){
    values<-c()
    count<-getRandomSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    return(median(values))  
  }
  )

setGeneric(
  name="getRandomSampleMode",
  def=function(.Object) {standardGeneric("getRandomSampleMode")}
  )

setMethod(
  f="getRandomSampleMode",
  signature="RandomSample",
  definition=function(.Object){
    values<-c()
    count<-getRandomSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    freq<-table(values)
    maxFreq<-max(freq)
    index<-which(freq==maxFreq)
    return(names(freq[index[[1]]]))
  }
  )

setGeneric(
  name="getRandomSampleSkewness",
  def=function(.Object) {standardGeneric("getRandomSampleSkewness")}
  )

setMethod(
  f="getRandomSampleSkewness",
  signature="RandomSample",
  definition=function(.Object){
    values<-c()
    m<-0
    n<-0
    count<-getRandomSampleCount(.Object)
    mean<-getRandomSampleMean(.Object)
    for(i in 1:count){
      t<-.Object@realisations[[i]]@value
      values<-c(values,t)
      m<-m+((t-mean)^3)
      n<-n+((t-mean)^2)
    }
    return((m/count)/((n/count)^(3/2)))  
  }
  )

setGeneric(
  name="getRandomSampleKurtosis",
  def=function(.Object) {standardGeneric("getRandomSampleKurtosis")}
  )

setMethod(
  f="getRandomSampleKurtosis",
  signature="RandomSample",
  definition=function(.Object){
    values<-c()
    m<-0
    n<-0
    count<-getRandomSampleCount(.Object)
    mean<-getRandomSampleMean(.Object)
    for(i in 1:count){
      t<-.Object@realisations[[i]]@value
      values<-c(values,t)
      m<-m+((t-mean)^4)
      n<-n+((t-mean)^2)
    }
    return(((m/count)/((n/count)^2))-3)  
  }
  )