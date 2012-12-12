setClass(
  Class="MultivariateStudentTDistribution",
  representation=representation(mean="vector", 
                                covarianceMatrix="matrix", 
                                degreesOfFreedom="numeric")
  )

setMethod(
  f="initialize",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object, mean, covariance, degrees){
    .Object@mean<-mean
    .Object@covarianceMatrix<-covariance
    .Object@degreesOfFreedom<-degrees
    return(.Object)
  }
  )

setGeneric(
  name="getMVTMean",
  def=function(.Object) {standardGeneric("getMVTMean")}
  )

setMethod(
  f="getMVTMean",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getMVTCovarianceMatrix",
  def=function(.Object) {standardGeneric("getMVTCovarianceMatrix")}
  )

setMethod(
  f="getMVTCovarianceMatrix",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object){
    return(.Object@covarianceMatrix)
  }
  )

setGeneric(
  name="getMVTDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getMVTDegreesOfFreedom")}
  )

setMethod(
  f="getMVTDegreesOfFreedom",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object){
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="setMVTMean<-",
  def=function(.Object,value){standardGeneric("setMVTMean<-")}
  )

setReplaceMethod(
  f="setMVTMean",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object,value){
    .Object@mean<-value
    return(.Object)
  }
  )

setGeneric(
  name="setMVTCovarianceMatrix<-",
  def=function(.Object,value){standardGeneric("setMVTCovarianceMatrix<-")}
  )

setReplaceMethod(
  f="setMVTCovarianceMatrix",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object,value){
    .Object@covarianceMatrix<-value
    return(.Object)
  }
  )

setGeneric(
  name="setMVTDegreesOfFreedom<-",
  def=function(.Object,value){standardGeneric("setMVTDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setMVTDegreesOfFreedom",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object,value){
    .Object@degreesOfFreedom<-value
    return(.Object)
  }
  )

setGeneric(
  name="getMVTSamples",
  def=function(.Object,number) {standardGeneric("getMVTSamples")}
  )

setMethod(
  f="getMVTSamples",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object, number){
    sample<-rmvt(number, .Object@covarianceMatrix, .Object@degreesOfFreedom, .Object@mean)
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
  name="getMVTVariance",
  def=function(.Object){standardGeneric("getMVTVariance")}
  )

setMethod(
  f="getMVTVariance",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object)
  {
    v<-.Object@degreesOfFreedom[1]
    if(v>2)
    {
      return((v/(v-2))*.Object@covarianceMatrix)
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getMVTStandardDeviation",
  def=function(.Object){standardGeneric("getMVTStandardDeviation")}
  )

setMethod(
  f="getMVTStandardDeviation",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object)
  {
    var<-getMVTVariance(.Object)
    if(!is.null(var))
    {
      return(sqrt(var))  
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getMVTMode",
  def=function(.Object){standardGeneric("getMVTMode")}
  )

setMethod(
  f="getMVTMode",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object)
  {
    return(.Object@mean)  
  }
  )

setGeneric(
  name="getMVTMedian",
  def=function(.Object){standardGeneric("getMVTMedian")}
  )

setMethod(
  f="getMVTMedian",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object)
  {
    return(.Object@mean)  
  }
  )

setGeneric(
  name="getMVTSkewness",
  def=function(.Object){standardGeneric("getMVTSkewness")}
  )

setMethod(
  f="getMVTSkewness",
  signature="MultivariateStudentTDistribution",
  definition=function(.Object)
  {
    return(0)  
  }
  )