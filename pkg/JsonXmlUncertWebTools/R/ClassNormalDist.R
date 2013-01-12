#source('~/R/ClassDistributions.R')
source('~/R/ClassRealisation.R')
source('~/R/ClassSamples.R')

setClass(
  Class="NormalDist",
  representation=representation(mean="numeric", variance="numeric"),
  contains="Distributions"
  #contains="UncertML"
  )

setMethod(
  f="initialize",
  signature="NormalDist",
  definition=function(.Object, file){
    .Object<-readFromJSON(.Object,file)
    .Object@mean<-.Object@rdata$N$m
    .Object@variance<-.Object@rdata$N$v
    return(.Object)
  }
  )

setGeneric(
  name="getMean",
  def=function(.Object) {standardGeneric("getMean")}
  )

setMethod(
  f="getMean",
  signature="NormalDist",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getVariance",
  def=function(.Object) {standardGeneric("getVariance")}
  )

setMethod(
  f="getVariance",
  signature="NormalDist",
  definition=function(.Object){
    return(.Object@variance)
  }
  )

setGeneric(
  name="setMean<-",
  def=function(.Object,Value){standardGeneric("setMean<-")}
  )

setReplaceMethod(
  f="setMean",
  signature="NormalDist",
  definition=function(.Object,Value){
    .Object@mean<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setVariance<-",
  def=function(.Object,Value){standardGeneric("setVariance<-")}
  )

setReplaceMethod(
  f="setVariance",
  signature="NormalDist",
  definition=function(.Object,Value){
    .Object@variance<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getSamples",
  signature="NormalDist",
  definition=function(.Object, number){
    sample<-rnorm(number, .Object@mean, .Object@variance)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )