source('~/R/validate.R')
setClass(
  Class="NormalDistribution",
  representation=representation(mean="numeric", variance="numeric"),
  validity=validND,
#  contains="UncertML"
  )

setMethod(
  f="initialize",
  signature="NormalDistribution",
  definition=function(.Object, mean, variance){
    .Object@mean<-mean
    .Object@variance<-variance
    validObject(.Object)
    return(.Object)
  }
  )

# setValidity(
#   Class="NormalDistribution",
#   #validReal(.Object@variance)
#   validND()
#   )

setGeneric(
  name="getNormalMean",
  def=function(.Object) {standardGeneric("getMean")}
  )

setMethod(
  f="getNormalMean",
  signature="NormalDistribution",
  definition=function(.Object){
    return(.Object@mean)
  }
  )

setGeneric(
  name="getNormalVariance",
  def=function(.Object) {standardGeneric("getVariance")}
  )

setMethod(
  f="getNormalVariance",
  signature="NormalDistribution",
  definition=function(.Object){
    return(.Object@variance)
  }
  )

setGeneric(
  name="setNormalMean<-",
  def=function(.Object,Value){standardGeneric("setMean<-")}
  )

setReplaceMethod(
  f="setNormalMean",
  signature="NormalDistribution",
  definition=function(.Object,Value){
    .Object@mean<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setNormalVariance<-",
  def=function(.Object,Value){standardGeneric("setVariance<-")}
  )

setReplaceMethod(
  f="setNormalVariance",
  signature="NormalDistribution",
  definition=function(.Object,Value){
    .Object@variance<-Value
    return(.Object)
  }
  )

setGeneric(
  name="getNormalSamples",
  def=function(.Object,number) {standardGeneric("getSamples")}
  )

setMethod(
  f="getNormalSamples",
  signature="NormalDistribution",
  definition=function(.Object, number){
    sample<-rnorm(number, .Object@mean, sqrt(.Object@variance))
    temp<-c()
    for(i in 1:length(sample)){
      temp<-c(temp, new(Class="Realisation", Value=as.double(sample[i]), Id=i, Weight=1/number))
    }
    rsample<-new(Class="RandomSample", temp)
    return(rsample)
  }
  )
# 
# setGeneric(
#   name="writeToJSON",
#   def=function(.Object, file){standardGeneric("writeToJSON")}
#   )
# 
# setMethod(
#   f="writeToJSON",
#   signature="NormalDistribution",
#   definition=function(.Object, file){
#     Class=class(.Object)[1]
#     Slots=slotNames(Class)
#     obj<-list()
#     for(i in 1:length(Slots)){
#       obj[[Slots[[i]]]]<-slot(.Object, Slots[[i]])
#     }
#     rdata<-list()
#     rdata[[Class]]=obj
#     #return(obj)
#     library("rjson")
#     write(toJSON(rdata), file)
#   }
#   )

setGeneric(
  name="readFromXML",
  def=function(.Object, file){standardGeneric("readFromXML")}
  )

setMethod(
  f="readFromXML",
  signature="NormalDistribution",
  definition=function(.Object, file){
    library("XML")
    xdata<-xmlTreeParse(file, getDTD=FALSE)
    root<-xmlRoot(xdata)
    className<-xmlName(root)
    mean<-as.numeric(xmlValue(root[[1]]))
    variance<-as.numeric(xmlValue(root[[2]]))
    .Object@mean<-mean
    .Object@variance<-variance
    detach("package:XML")
    return(.Object)
#     slots<-list()
#     for (i in 1:xmlSize(root)){
#       slots[xmlName(root[[i]])]<-root[[i]][1]
#     }
#     rdata<-list()
#     rdata[[className]]=slots
#     return(rdata)
  }  
  )

setGeneric(
  name="writeToXML",
  def=function(.Object, file){standardGeneric("writeToXML")}
  )

setMethod(
  f="writeToXML",
  signature="NormalDistribution",
  definition=function(.Object, file){
    library("XML")
    data<-xmlNode("un:NormalDistribution", 
                  attrs=c(xmlns="http://www.uncertml.org/2.0"), 
                  xmlNode("un:mean", .Object@mean), 
                  xmlNode("un:variance", .Object@variance))
    write(toString(data), file)    
    detach("package:XML")
  }
  )