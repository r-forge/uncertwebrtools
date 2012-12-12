setClass(
  Class="UnknownSample",
  representation=representation(realisations="list", samplingMethodDescription="character")
  )

setMethod(
  f="initialize",
  signature="UnknownSample",
  definition=function(.Object, Realisations, Desc=""){
    .Object@realisations<-Realisations
    .Object@samplingMethodDescription<-Desc
    return(.Object)
  }
  )

setGeneric(
  name="getUnknownSampleMethodDesc",
  def=function(.Object) {standardGeneric("getUnknownSampleMethodDesc")}
  )

setMethod(
  f="getUnknownSampleMethodDesc",
  signature="UnknownSample",
  definition=function(.Object){
    return(.Object@samplingMethodDescription)
  }
  )

setGeneric(
  name="getUnknownSampleRealisations",
  def=function(.Object) {standardGeneric("getUnknownSampleRealisations")}
  )

setMethod(
  f="getUnknownSampleRealisations",
  signature="UnknownSample",
  definition=function(.Object){
    return(.Object@realisations)
  }
  )

setGeneric(
  name="setUnknownSampleMethodDesc<-",
  def=function(.Object, value) {standardGeneric("setUnknownSampleMethodDesc<-")}
  )

setReplaceMethod(
  f="setUnknownSampleMethodDesc",
  signature="UnknownSample",
  definition=function(.Object, value){
    .Object@samplingMethodDescription<-value
    return(.Object)
  }
  )


setGeneric(
  name="addUnknownSampleRealisations<-",
  def=function(.Object,value) {standardGeneric("addUnknownSampleRealisations<-")}
  )

setReplaceMethod(
  f="addUnknownSampleRealisations",
  signature="UnknownSample",
  definition=function(.Object, value){
    .Object@realisations<-c(.Object@realisations, value)
    return(.Object)
  }
  )

setGeneric(
  name="getUnknownSampleCount",
  def=function(.Object) {standardGeneric("getUnknownSampleCount")}
  )

setMethod(
  f="getUnknownSampleCount",
  signature="UnknownSample",
  definition=function(.Object){
    return(length(.Object@realisations))
  }
  )

setGeneric(
  name="getUnknownSampleMean",
  def=function(.Object) {standardGeneric("getUnknownSampleMean")}
  )

setMethod(
  f="getUnknownSampleMean",
  signature="UnknownSample",
  definition=function(.Object){
    values<-c()
    count<-getUnknownSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    return(mean(values))
  }
  )

setGeneric(
  name="getUnknownSampleVariance",
  def=function(.Object) {standardGeneric("getUnknownSampleVariance")}
  )

setMethod(
  f="getUnknownSampleVariance",
  signature="UnknownSample",
  definition=function(.Object){
    values<-c()
    count<-getUnknownSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    return(var(values))  
  }
  )

setGeneric(
  name="getUnknownSampleStandardDeviation",
  def=function(.Object) {standardGeneric("getUnknownSampleStandardDeviation")}
  )

setMethod(
  f="getUnknownSampleStandardDeviation",
  signature="UnknownSample",
  definition=function(.Object){
    return(sqrt(getUnknownSampleVariance(.Object)))  
  }
  )

setGeneric(
  name="getUnknownSampleMedian",
  def=function(.Object) {standardGeneric("getUnknownSampleMedian")}
  )

setMethod(
  f="getUnknownSampleMedian",
  signature="UnknownSample",
  definition=function(.Object){
    values<-c()
    count<-getUnknownSampleCount(.Object)
    for(i in 1:count){
      values<-c(values,.Object@realisations[[i]]@value)
    }
    return(median(values))  
  }
  )

setGeneric(
  name="getUnknownSampleMode",
  def=function(.Object) {standardGeneric("getUnknownSampleMode")}
  )

setMethod(
  f="getUnknownSampleMode",
  signature="UnknownSample",
  definition=function(.Object){
    values<-c()
    count<-getUnknownSampleCount(.Object)
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
  name="getUnknownSampleSkewness",
  def=function(.Object) {standardGeneric("getUnknownSampleSkewness")}
  )

setMethod(
  f="getUnknownSampleSkewness",
  signature="UnknownSample",
  definition=function(.Object){
    values<-c()
    m<-0
    n<-0
    count<-getUnknownSampleCount(.Object)
    mean<-getUnknownSampleMean(.Object)
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
  name="getUnknownSampleKurtosis",
  def=function(.Object) {standardGeneric("getUnknownSampleKurtosis")}
  )

setMethod(
  f="getUnknownSampleKurtosis",
  signature="UnknownSample",
  definition=function(.Object){
    values<-c()
    m<-0
    n<-0
    count<-getUnknownSampleCount(.Object)
    mean<-getUnknownSampleMean(.Object)
    for(i in 1:count){
      t<-.Object@realisations[[i]]@value
      values<-c(values,t)
      m<-m+((t-mean)^4)
      n<-n+((t-mean)^2)
    }
    return(((m/count)/((n/count)^2))-3)  
  }
  )