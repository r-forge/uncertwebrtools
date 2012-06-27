setClass(
  Class="HypergeometricDistribution",
  representation=representation(numberOfSuccesses="integer", 
                                numberOfTrials="integer", 
                                populationSize="integer")
  )

setMethod(
  f="initialize",
  signature="HypergeometricDistribution",
  definition=function(.Object, successes, trials, population){
    .Object@numberOfSuccesses<-successes
    .Object@numberOfTrials<-trials
    .Object@populationSize<-population
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getHypergeometricNumberOfSuccesses",
  def=function(.Object) {standardGeneric("getNumberOfSuccesses")}
  )

setMethod(
  f="getHypergeometricNumberOfSuccesses",
  signature="HypergeometricDistribution",
  definition=function(.Object){
    return(.Object@numberOfSuccesses)
  }
  )

setGeneric(
  name="getHypergeometricNumberOfTrials",
  def=function(.Object) {standardGeneric("getNumberOfTrials")}
  )

setMethod(
  f="getHypergeometricNumberOfTrials",
  signature="HypergeometricDistribution",
  definition=function(.Object){
    return(.Object@numberOfTrials)
  }
  )

setGeneric(
  name="getHypergeometricPopulationSize",
  def=function(.Object) {standardGeneric("getPopulationSize")}
  )

setMethod(
  f="getHypergeometricPopulationSize",
  signature="HypergeometricDistribution",
  definition=function(.Object){
    return(.Object@populationSize)
  }
  )

setGeneric(
  name="setHypergeometricNumberOfSuccesses<-",
  def=function(.Object,Value){standardGeneric("setNumberOfSuccesses<-")}
  )

setReplaceMethod(
  f="setHypergeometricNumberOfSuccesses",
  signature="HypergeometricDistribution",
  definition=function(.Object,Value){
    .Object@numberOfSuccesses<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setHypergeometricNumberOfTrials<-",
  def=function(.Object,Value){standardGeneric("setNumberOfTrials<-")}
  )

setReplaceMethod(
  f="setHypergeometricNumberOfTrials",
  signature="HypergeometricDistribution",
  definition=function(.Object,Value){
    .Object@numberOfTrials<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setHypergeometricPopulationSize<-",
  def=function(.Object,Value){standardGeneric("setPopulationSize<-")}
  )

setReplaceMethod(
  f="setHypergeometricPopulationSize",
  signature="HypergeometricDistribution",
  definition=function(.Object,Value){
    .Object@populationSize<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getHypergeometricSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getHypergeometricSamples",
  signature="HypergeometricDistribution",
  definition=function(.Object, number){
    sample<-rhyper(number, .Object@numberOfSuccesses, .Object@populationSize-.Object@numberOfSuccesses , .Object@numberOfTrials)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
