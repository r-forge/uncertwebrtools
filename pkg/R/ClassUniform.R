setClass(
  Class="UniformDistribution",
  representation=representation(minimum="numeric", maximum="numeric")
  )

setMethod(
  f="initialize",
  signature="UniformDistribution",
  definition=function(.Object, min, max){
    .Object@minimum<-min
    .Object@maximum<-max
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getUniformMinimum",
  def=function(.Object) {standardGeneric("getMinimum")}
  )

setMethod(
  f="getUniformMinimum",
  signature="UniformDistribution",
  definition=function(.Object){
    return(.Object@minimum)
  }
  )

setGeneric(
  name="getUniformMaximum",
  def=function(.Object) {standardGeneric("getMaximum")}
  )

setMethod(
  f="getUniformMaximum",
  signature="UniformDistribution",
  definition=function(.Object){
    return(.Object@maximum)
  }
  )

setGeneric(
  name="setUniformMinimum<-",
  def=function(.Object,Value){standardGeneric("setMinimum<-")}
  )

setReplaceMethod(
  f="setUniformMinimum",
  signature="UniformDistribution",
  definition=function(.Object,Value){
    .Object@minimum<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setUniformMaximum<-",
  def=function(.Object,Value){standardGeneric("setMaximum<-")}
  )

setReplaceMethod(
  f="setUniformMaximum",
  signature="UniformDistribution",
  definition=function(.Object,Value){
    .Object@maximum<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getUniformSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getUniformSamples",
  signature="UniformDistribution",
  definition=function(.Object, number){
    sample<-runif(number, min=.Object@minimum, max=.Object@maximum)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
