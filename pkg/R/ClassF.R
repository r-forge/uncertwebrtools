setClass(
  Class="FDistribution",
  representation=representation(denominator="integer", numerator="integer")
  )

setMethod(
  f="initialize",
  signature="FDistribution",
  definition=function(.Object, denominator, numerator){
    .Object@denominator<-denominator
    .Object@numerator<-numerator
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getFDenominator",
  def=function(.Object) {standardGeneric("getDenominator")}
  )

setMethod(
  f="getFDenominator",
  signature="FDistribution",
  definition=function(.Object){
    return(.Object@denominator)
  }
  )

setGeneric(
  name="getFNumerator",
  def=function(.Object) {standardGeneric("getNumerator")}
  )

setMethod(
  f="getFNumerator",
  signature="FDistribution",
  definition=function(.Object){
    return(.Object@numerator)
  }
  )

setGeneric(
  name="setFDenominator<-",
  def=function(.Object,Value){standardGeneric("setDenominator<-")}
  )

setReplaceMethod(
  f="setFDenominator",
  signature="FDistribution",
  definition=function(.Object,Value){
    .Object@denominator<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setFNumerator<-",
  def=function(.Object,Value){standardGeneric("setNumerator<-")}
  )

setReplaceMethod(
  f="setFNumerator",
  signature="FDistribution",
  definition=function(.Object,Value){
    .Object@numerator<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getFSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getFSamples",
  signature="FDistribution",
  definition=function(.Object, number){
    sample<-rf(number, .Object@denominator, .Object@numerator)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
