setClass(
  Class="InverseGammaDistribution",
  representation=representation(shape="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="InverseGammaDistribution",
  definition=function(.Object, shape, scale){
    .Object@shape<-shape
    .Object@scale<-scale
    return(.Object)
  }
  )

setGeneric(
  name="getInverseGammaShape",
  def=function(.Object) {standardGeneric("getInverseGammaShape")}
  )

setMethod(
  f="getInverseGammaShape",
  signature="InverseGammaDistribution",
  definition=function(.Object){
    return(.Object@shape)
  }
  )

setGeneric(
  name="getInverseGammaScale",
  def=function(.Object) {standardGeneric("getInverseGammaScale")}
  )

setMethod(
  f="getInverseGammaScale",
  signature="InverseGammaDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setInverseGammaShape<-",
  def=function(.Object,value){standardGeneric("setInverseGammaShape<-")}
  )

setReplaceMethod(
  f="setInverseGammaShape",
  signature="InverseGammaDistribution",
  definition=function(.Object,value){
    .Object@shape<-value
    return(.Object)
  }
  )

setGeneric(
  name="setInverseGammaScale<-",
  def=function(.Object,value){standardGeneric("setInverseGammaScale<-")}
  )

setReplaceMethod(
  f="setInverseGammaScale",
  signature="InverseGammaDistribution",
  definition=function(.Object,value){
    .Object@scale<-value
    return(.Object)
  }
  )

setGeneric(
  name="getInverseGammaSamples",
  def=function(.Object,number) {standardGeneric("getInverseGammaSamples")}
  )

setMethod(
  f="getInverseGammaSamples",
  signature="InverseGammaDistribution",
  definition=function(.Object, number){
    iScale<-1/.Object@scale
    gSample<-rgamma(number, shape=.Object@shape, scale=iScale)
    sample<-c()
    temp<-c()
    for(i in 1:length(gSample)){
      sample[i]<-1/gSample[i]
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )

setGeneric(
  name="getInverseGammaMean",
  def=function(.Object){standardGeneric("getInverseGammaMean")}
  )

setMethod(
  f="getInverseGammaMean",
  signature="InverseGammaDistribution",
  definition=function(.Object)
  {
    if(.Object@shape>1)
    {
      return(.Object@scale/(.Object@shape-1))  
    }
    else
    {
      return(NULL)
    }    
  }
  )

setGeneric(
  name="getInverseGammaVariance",
  def=function(.Object){standardGeneric("getInverseGammaVariance")}
  )

setMethod(
  f="getInverseGammaVariance",
  signature="InverseGammaDistribution",
  definition=function(.Object)
  {
    b<-.Object@scale
    a<-.Object@shape
    if(a>2)
    {
      return((b^2)/(((a-1)^2)*(a-2)))  
    }
    else
    {
      return(NULL)
    }    
  }
  )

setGeneric(
  name="getInverseGammaStandardDeviation",
  def=function(.Object){standardGeneric("getInverseGammaStandardDeviation")}
  )

setMethod(
  f="getInverseGammaStandardDeviation",
  signature="InverseGammaDistribution",
  definition=function(.Object)
  {
    var<-getInverseGammaVariance(.Object)
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
  name="getInverseGammaMode",
  def=function(.Object){standardGeneric("getInverseGammaMode")}
  )

setMethod(
  f="getInverseGammaMode",
  signature="InverseGammaDistribution",
  definition=function(.Object)
  {
    return(.Object@scale/(.Object@shape+1))  
  }
  )

setGeneric(
  name="getInverseGammaSkewness",
  def=function(.Object){standardGeneric("getInverseGammaSkewness")}
  )

setMethod(
  f="getInverseGammaSkewness",
  signature="InverseGammaDistribution",
  definition=function(.Object)
  {
    a<-.Object@shape
    if(a>3)
    {
      return((4*sqrt(a-2))/(a-3))
    }
    else
    {
      return(NULL)
    }
  }
  )

setGeneric(
  name="getInverseGammaKurtosis",
  def=function(.Object){standardGeneric("getInverseGammaKurtosis")}
  )

setMethod(
  f="getInverseGammaKurtosis",
  signature="InverseGammaDistribution",
  definition=function(.Object)
  {
    a<-.Object@shape
    if(a>4)
    {
      return((30*a-66)/((a-3)*(a-4)))
    }
    else
    {
      return(NULL)
    }
  }
  )