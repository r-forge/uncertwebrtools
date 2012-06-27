setClass(
  Class="DirichletDistribution",
  representation=representation(concentration="vector")
  )

setMethod(
  f="initialize",
  signature="DirichletDistribution",
  definition=function(.Object, concentration){
    .Object@concentration<-concentration
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getDirichletConcentration",
  def=function(.Object) {standardGeneric("getConcentration")}
  )

setMethod(
  f="getDirichletConcentration",
  signature="DirichletDistribution",
  definition=function(.Object){
    return(.Object@concentration)
  }
  )

setGeneric(
  name="setDirichletConcentration<-",
  def=function(.Object,Value){standardGeneric("setConcentration<-")}
  )

setReplaceMethod(
  f="setDirichletConcentration",
  signature="DirichletDistribution",
  definition=function(.Object,Value){
    .Object@concentration<-Value
    return(.Object)
  }
  )
# 
# setGeneric(
#   name="getDirichletSamples",
#   def=function(.Object,number) {standardGeneric("getSamples")}
#   )
# 
# setMethod(
#   f="getDirichletSamples",
#   signature="DirichletDistribution",
#   definition=function(.Object, number){
#     sample<-rchisq(number, .Object@concentration)
#     temp<-c()
#     for(i in 1:length(sample)){
#       temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
#     }
#     rsample<-new(Class="RandomSample", temp)
#     return(rsample)
#   }
#   )
