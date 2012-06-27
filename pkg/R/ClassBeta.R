setClass(
  Class="BetaDistribution",
  representation=representation(alpha="numeric", beta="numeric")
  )

setMethod(
  f="initialize",
  signature="BetaDistribution",
  definition=function(.Object, alpha, beta){
    .Object@alpha<-alpha
    .Object@beta<-beta
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getBetaDistributionAlpha",
  def=function(.Object) {standardGeneric("getAlpha")}
  )

setMethod(
  f="getBetaDistributionAlpha",
  signature="BetaDistribution",
  definition=function(.Object){
    return(.Object@alpha)
  }
  )

setGeneric(
  name="getBetaDistributionBeta",
  def=function(.Object) {standardGeneric("getBeta")}
  )

setMethod(
  f="getBetaDistributionBeta",
  signature="BetaDistribution",
  definition=function(.Object){
    return(.Object@beta)
  }
  )

setGeneric(
  name="setBetaDistributionAlpha<-",
  def=function(.Object,Value){standardGeneric("setAlpha<-")}
  )

setReplaceMethod(
  f="setBetaDistributionAlpha",
  signature="BetaDistribution",
  definition=function(.Object,Value){
    .Object@alpha<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setBetaDistributionBeta<-",
  def=function(.Object,Value){standardGeneric("setBeta<-")}
  )

setReplaceMethod(
  f="setBetaDistributionBeta",
  signature="BetaDistribution",
  definition=function(.Object,Value){
    .Object@beta<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getBetaDistributionSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getBetaDistributionSamples",
  signature="BetaDistribution",
  definition=function(.Object, number){
    sample<-rbeta(number, .Object@alpha, .Object@beta)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
