setClass(
  Class="HypergeometricDistribution",
  representation=representation(numberOfSuccesses="numeric", 
                                numberOfTrials="numeric", 
                                populationSize="numeric")
  )

setMethod(
  f="initialize",
  signature="HypergeometricDistribution",
  definition=function(.Object, successes, trials, population){
    .Object@numberOfSuccesses<-successes
    .Object@numberOfTrials<-trials
    .Object@populationSize<-population
    return(.Object)
  }
  )

setGeneric(
  name="getHypergeomNumberOfSuccesses",
  def=function(.Object) {standardGeneric("getHypergeomNumberOfSuccesses")}
  )

setMethod(
  f="getHypergeomNumberOfSuccesses",
  signature="HypergeometricDistribution",
  definition=function(.Object){
    return(.Object@numberOfSuccesses)
  }
  )

setGeneric(
  name="getHypergeomNumberOfTrials",
  def=function(.Object) {standardGeneric("getHypergeomNumberOfTrials")}
  )

setMethod(
  f="getHypergeomNumberOfTrials",
  signature="HypergeometricDistribution",
  definition=function(.Object){
    return(.Object@numberOfTrials)
  }
  )

setGeneric(
  name="getHypergeomPopulationSize",
  def=function(.Object) {standardGeneric("getHypergeomPopulationSize")}
  )

setMethod(
  f="getHypergeomPopulationSize",
  signature="HypergeometricDistribution",
  definition=function(.Object){
    return(.Object@populationSize)
  }
  )

setGeneric(
  name="setHypergeomNumberOfSuccesses<-",
  def=function(.Object,value){standardGeneric("setHypergeomNumberOfSuccesses<-")}
  )

setReplaceMethod(
  f="setHypergeomNumberOfSuccesses",
  signature="HypergeometricDistribution",
  definition=function(.Object,value){
    .Object@numberOfSuccesses<-value
    return(.Object)
  }
  )

setGeneric(
  name="setHypergeomNumberOfTrials<-",
  def=function(.Object,value){standardGeneric("setHypergeomNumberOfTrials<-")}
  )

setReplaceMethod(
  f="setHypergeomNumberOfTrials",
  signature="HypergeometricDistribution",
  definition=function(.Object,value){
    .Object@numberOfTrials<-value
    return(.Object)
  }
  )

setGeneric(
  name="setHypergeomPopulationSize<-",
  def=function(.Object,value){standardGeneric("setHypergeomPopulationSize<-")}
  )

setReplaceMethod(
  f="setHypergeomPopulationSize",
  signature="HypergeometricDistribution",
  definition=function(.Object,value){
    .Object@populationSize<-value
    return(.Object)
  }
  )

setGeneric(
  name="getHypergeomSamples",
  def=function(.Object,number) {standardGeneric("getHypergeomSamples")}
  )

setMethod(
  f="getHypergeomSamples",
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

setGeneric(
  name="getHypergeomMean",
  def=function(.Object){standardGeneric("getHypergeomMean")}
  )

setMethod(
  f="getHypergeomMean",
  signature="HypergeometricDistribution",
  definition=function(.Object)
  {
    N<-.Object@populationSize
    n<-.Object@numberOfTrials
    m<-.Object@numberOfSuccesses
    return(n*m/N)  
  }
  )

setGeneric(
  name="getHypergeomVariance",
  def=function(.Object){standardGeneric("getHypergeomVariance")}
  )

setMethod(
  f="getHypergeomVariance",
  signature="HypergeometricDistribution",
  definition=function(.Object)
  {
    N<-.Object@populationSize
    n<-.Object@numberOfTrials
    m<-.Object@numberOfSuccesses
    return((n*m*(N-m)*(N-n))/((N^2)*(N-1)))
  }
  )

setGeneric(
  name="getHypergeomStandardDeviation",
  def=function(.Object){standardGeneric("getHypergeomStandardDeviation")}
  )

setMethod(
  f="getHypergeomStandardDeviation",
  signature="HypergeometricDistribution",
  definition=function(.Object)
  {
    var<-getHypergeometricVariance(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getHypergeomMode",
  def=function(.Object){standardGeneric("getHypergeomMode")}
  )

setMethod(
  f="getHypergeomMode",
  signature="HypergeometricDistribution",
  definition=function(.Object)
  {
    N<-.Object@populationSize
    n<-.Object@numberOfTrials
    m<-.Object@numberOfSuccesses
    return(floor((n+1)*(m+1)/(N+2)))  
  }
  )

setGeneric(
  name="getHypergeomSkewness",
  def=function(.Object){standardGeneric("getHypergeomSkewness")}
  )

setMethod(
  f="getHypergeomSkewness",
  signature="HypergeometricDistribution",
  definition=function(.Object)
  {
    N<-.Object@populationSize
    n<-.Object@numberOfTrials
    m<-.Object@numberOfSuccesses
    return((sqrt(N-1)*(N-2*m)*(N-2*n))/(sqrt(n*m*(N-m)*(N-n))*(N-2)))  
  }
  )

setGeneric(
  name="getHypergeomKurtosis",
  def=function(.Object){standardGeneric("getHypergeomKurtosis")}
  )

setMethod(
  f="getHypergeomKurtosis",
  signature="HypergeometricDistribution",
  definition=function(.Object)
  {
    N<-.Object@populationSize
    n<-.Object@numberOfTrials
    m<-.Object@numberOfSuccesses
    t1<-((N-1)*(N^2)*(N*(N+1)-(6*m*(N-m))-(6*n*(N-n))))+(6*n*m*(N-m)*(N-n)*(5*N-6))
    t2<-n*m*(N-m)*(N-n)*(N-2)*(N-3)
    return(t1/t2) 
  }
  )