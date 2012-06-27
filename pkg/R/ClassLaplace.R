setClass(
  Class="LaplaceDistribution",
  representation=representation(location="numeric", scale="numeric")
  )

setMethod(
  f="initialize",
  signature="LaplaceDistribution",
  definition=function(.Object, location, scale){
    .Object@location<-location
    .Object@scale<-scale
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getLaplaceLocation",
  def=function(.Object) {standardGeneric("getLocation")}
  )

setMethod(
  f="getLaplaceLocation",
  signature="LaplaceDistribution",
  definition=function(.Object){
    return(.Object@location)
  }
  )

setGeneric(
  name="getLaplaceScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getLaplaceScale",
  signature="LaplaceDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="setLaplaceLocation<-",
  def=function(.Object,Value){standardGeneric("setLocation<-")}
  )

setReplaceMethod(
  f="setLaplaceLocation",
  signature="LaplaceDistribution",
  definition=function(.Object,Value){
    .Object@location<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setLaplaceScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setLaplaceScale",
  signature="LaplaceDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

# setGeneric(
#   name="getLaplaceSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getLaplaceSamples",
#   signature="LaplaceDistribution",
#   definition=function(.Object, number){
#     sample<-rlogis(number, location=.Object@location, scale=.Object@scale)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
