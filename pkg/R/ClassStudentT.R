setClass(
  Class="StudentTDistribution",
  representation=representation(location="numeric", 
                                scale="numeric", 
                                degreesOfFreedom="integer")
  )

setMethod(
  f="initialize",
  signature="StudentTDistribution",
  definition=function(.Object, location, scale, degrees){
    .Object@location<-location
    .Object@scale<-scale
    .Object@degreesOfFreedom<-degrees
    #validObject(.Object)
    return(.Object)
  }
  )

setGeneric(
  name="getStudentTLocation",
  def=function(.Object) {standardGeneric("getLocation")}
  )

setMethod(
  f="getStudentTLocation",
  signature="StudentTDistribution",
  definition=function(.Object){
    return(.Object@location)
  }
  )

setGeneric(
  name="getStudentTScale",
  def=function(.Object) {standardGeneric("getScale")}
  )

setMethod(
  f="getStudentTScale",
  signature="StudentTDistribution",
  definition=function(.Object){
    return(.Object@scale)
  }
  )

setGeneric(
  name="getStudentTDegreesOfFreedom",
  def=function(.Object) {standardGeneric("getDegreesOfFreedom")}
  )

setMethod(
  f="getStudentTDegreesOfFreedom",
  signature="StudentTDistribution",
  definition=function(.Object){
    return(.Object@degreesOfFreedom)
  }
  )

setGeneric(
  name="setStudentTLocation<-",
  def=function(.Object,Value){standardGeneric("setLocation<-")}
  )

setReplaceMethod(
  f="setStudentTLocation",
  signature="StudentTDistribution",
  definition=function(.Object,Value){
    .Object@location<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setStudentTScale<-",
  def=function(.Object,Value){standardGeneric("setScale<-")}
  )

setReplaceMethod(
  f="setStudentTScale",
  signature="StudentTDistribution",
  definition=function(.Object,Value){
    .Object@scale<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setStudentTDegreesOfFreedom<-",
  def=function(.Object,Value){standardGeneric("setDegreesOfFreedom<-")}
  )

setReplaceMethod(
  f="setStudentTDegreesOfFreedom",
  signature="StudentTDistribution",
  definition=function(.Object,Value){
    .Object@degreesOfFreedom<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getStudentTSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getStudentTSamples",
  signature="StudentTDistribution",
  definition=function(.Object, number){
    sample<-rt(number, .Object@degreesOfFreedom)
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
