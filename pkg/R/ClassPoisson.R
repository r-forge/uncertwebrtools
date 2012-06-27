setClass(
  Class="PoissonDistribution",
  representation=representation(rate="numeric")
  )

setMethod(
  f="initialize",
  signature="PoissonDistribution",
  definition=function(.Object, rate){
    .Object@rate<-rate
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getPoissonRate",
  def=function(.Object) {standardGeneric("getRate")}
  )

setMethod(
  f="getPoissonRate",
  signature="PoissonDistribution",
  definition=function(.Object){
    return(.Object@rate)
  }
  )

setGeneric(
  name="setPoissonRate<-",
  def=function(.Object,Value){standardGeneric("setRate<-")}
  )

setReplaceMethod(
  f="setPoissonRate",
  signature="PoissonDistribution",
  definition=function(.Object,Value){
    .Object@rate<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getPoissonSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getPoissonSamples",
  signature="PoissonDistribution",
  definition=function(.Object, number){
    sample<-rpois(number, .Object@rate)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
