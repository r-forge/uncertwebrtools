source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassParseJSON.R')
source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassParseXML.R')
source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassUncertainty.R')

setClass(
  Class="NormalDistribution",
  representation=representation(mean="numeric", variance="numeric"),
  #validity=validND,
  contains="Uncertainty"
  )

# setValidity(
#   Class="NormalDistribution",
#   #validReal(.Object@variance)
#   validND()
#   )

setGeneric(
  name="getMean",
  def=function(.Object) {standardGeneric("getMean")}
  )

setMethod(
  f="getMean",
  signature="NormalDistribution",
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
  signature="NormalDistribution",
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
  signature="NormalDistribution",
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
  signature="NormalDistribution",
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
  signature="NormalDistribution",
  definition=function(.Object, number){
    sample<-rnorm(number, .Object@mean, sqrt(.Object@variance))
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
