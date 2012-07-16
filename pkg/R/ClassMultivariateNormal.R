setClass(
  Class="MultivariateNormalDistribution",
  representation=representation(mean="vector",
                                covarianceMatrix="matrix")
  )

setMethod(
  f="initialize",
  signature="MultivariateNormalDistribution",
  definition=function(.Object, mean, covariance){
    .Object@mean<-mean
    .Object@covarianceMatrix<-covariance
    return(.Object)
  }
  )

setGeneric(
  name="getMVNormalMean",
  def=function(.Object) {standardGeneric("getMVNormalMean")}
  )

setMethod(
  f="getMVNormalMean",
  signature="MultivariateNormalDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getMVNormalCovarianceMatrix",
  def=function(.Object) {standardGeneric("getMVNormalCovarianceMatrix")}
  )

setMethod(
  f="getMVNormalCovarianceMatrix",
  signature="MultivariateNormalDistribution",
  definition=function(.Object){
    return(.Object@covarianceMatrix)
  }
  )

setGeneric(
  name="setMVNormalMean<-",
  def=function(.Object,value){standardGeneric("setMVNormalMean<-")}
  )

setReplaceMethod(
  f="setMVNormalMean",
  signature="MultivariateNormalDistribution",
  definition=function(.Object,value){
    .Object@mean<-value
    return(.Object)
  }
  )

setGeneric(
  name="setMVNormalCovarianceMatrix<-",
  def=function(.Object,value){standardGeneric("setMVNormalCovarianceMatrix<-")}
  )

setReplaceMethod(
  f="setMVNormalCovarianceMatrix",
  signature="MultivariateNormalDistribution",
  definition=function(.Object,value){
    .Object@covarianceMatrix<-value
    return(.Object)
  }
  )

setGeneric(
  name="getMVNormalSamples",
  def=function(.Object,number) {standardGeneric("getMVNormalSamples")}
  )

setMethod(
  f="getMVNormalSamples",
  signature="MultivariateNormalDistribution",
  definition=function(.Object, number){
      library("mvtnorm")
    sample<-rmvnorm(number, mean=.Object@mean, .Object@covarianceMatrix, )
    temp<-list()
    for(i in 1:number){
        a<-c()
        for(j in 1:length(.Object@mean))
        {
           a<-c(a, new(Class="Realisation", Value=as.double(sample[i,j]), 
                       Id=i, Weight=1/number))
          
        }
        temp[[i]]<-a
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getMVNormalStandardDeviation",
  def=function(.Object){standardGeneric("getMVNormalStandardDeviation")}
  )

setMethod(
  f="getMVNormalStandardDeviation",
  signature="MultivariateNormalDistribution",
  definition=function(.Object)
  {
    var<-getMVNormalCovarianceMatrix(.Object)
    return(sqrt(var))  
  }
  )

setGeneric(
  name="getMVNormalMode",
  def=function(.Object){standardGeneric("getMVNormalMode")}
  )

setMethod(
  f="getMVNormalMode",
  signature="MultivariateNormalDistribution",
  definition=function(.Object)
  {
    return(.Object@mean)  
  }
  )