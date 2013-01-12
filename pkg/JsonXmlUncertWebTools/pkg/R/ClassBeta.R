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
    return(.Object)
  }
  )

setGeneric(
  name="getBetaDistributionAlpha",
  def=function(.Object) {standardGeneric("getBetaDistributionAlpha")}
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
  def=function(.Object) {standardGeneric("getBetaDistributionBeta")}
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
  def=function(.Object,value){standardGeneric("setBetaDistributionAlpha<-")}
  )

setReplaceMethod(
  f="setBetaDistributionAlpha",
  signature="BetaDistribution",
  definition=function(.Object,value){
    .Object@alpha<-value
    return(.Object)
  }
  )

setGeneric(
  name="setBetaDistributionBeta<-",
  def=function(.Object,value){standardGeneric("setBetaDistributionBeta<-")}
  )

setReplaceMethod(
  f="setBetaDistributionBeta",
  signature="BetaDistribution",
  definition=function(.Object,value){
    .Object@beta<-value
    return(.Object)
  }
  )

setGeneric(
  name="getBetaDistributionSamples",
  def=function(.Object,number) {standardGeneric("getBetaDistributionSamples")}
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

setGeneric(
  name="getBetaDistributionMode",
  def=function(.Object){standardGeneric("getBetaDistributionMode")}
  )

setMethod(
  f="getBetaDistributionMode",
  signature="BetaDistribution",
  definition=function(.Object)
  {
    if(.Object@alpha>1 & .Object@beta>1)
    {
      return((.Object@alpha-1)/(.Object@alpha+.Object@beta-2))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getBetaDistributionMean",
  def=function(.Object){standardGeneric("getBetaDistributionMean")}
  )

setMethod(
  f="getBetaDistributionMean",
  signature="BetaDistribution",
  definition=function(.Object)
  {
    return(.Object@alpha/(.Object@alpha+.Object@beta))
  }
  )

setGeneric(
  name="getBetaDistributionVariance",
  def=function(.Object){standardGeneric("getBetaDistributionVariance")}
  )

setMethod(
  f="getBetaDistributionVariance",
  signature="BetaDistribution",
  definition=function(.Object)
  {
    return((.Object@alpha*.Object@beta)/(((.Object@alpha+.Object@beta)^2)*(.Object@alpha+.Object@beta+1)))
  }
  )

setGeneric(
  name="getBetaDistributionStandardDeviation",
  def=function(.Object){standardGeneric("getBetaDistributionStandardDeviation")}
  )

setMethod(
  f="getBetaDistributionStandardDeviation",
  signature="BetaDistribution",
  definition=function(.Object)
  {
    var<-getBetaDistributionVariance(.Object)
    return(sqrt(var))
  }
  )

setGeneric(
  name="getBetaDistributionKurtosis",
  def=function(.Object){standardGeneric("getBetaDistributionKurtosis")}
  )

setMethod(
  f="getBetaDistributionKurtosis",
  signature="BetaDistribution",
  definition=function(.Object)
  {
    s<-.Object@alpha+.Object@beta
    m<-.Object@alpha*.Object@beta
    d<-.Object@alpha-.Object@beta
    return((6*(d^2*(s+1)-(m*(s+2))))/(m*(s+2)*(s+3)))
  }
  )

setGeneric(
  name="getBetaDistributionSkewness",
  def=function(.Object){standardGeneric("getBetaDistributionSkewness")}
  )

setMethod(
  f="getBetaDistributionSkewness",
  signature="BetaDistribution",
  definition=function(.Object)
  {
    s<-.Object@alpha+.Object@beta
    m<-.Object@alpha*.Object@beta
    d<-.Object@beta-.Object@alpha
    return((2*sqrt(s+1)*d)/((s+2)*sqrt(m)))
  }
  )
