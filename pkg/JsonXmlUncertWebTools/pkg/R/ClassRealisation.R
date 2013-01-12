setClass(
  Class="Realisation",
  representation=representation(value="numeric", id="numeric", weight="numeric")
  )

setMethod(
  f="initialize",
  signature="Realisation",
  definition=function(.Object, Value, Id, Weight){
    .Object@value=Value
    .Object@id=Id
    .Object@weight=Weight
    return(.Object)
  }
  )

setGeneric(
  name="getValue",
  def=function(.Object) {standardGeneric("getValue")}
  )

setMethod(
  f="getValue",
  signature="Realisation",
  definition=function(.Object){
    return(.Objetc@value)
  }
  )

setGeneric(
  name="getId",
  def=function(.Object) {standardGeneric("getId")}
  )

setMethod(
  f="getId",
  signature="Realisation",
  definition=function(.Object){
    return(.Objetc@id)
  }
  )

setGeneric(
  name="getWeight",
  def=function(.Object) {standardGeneric("getWeight")}
  )

setMethod(
  f="getWeight",
  signature="Realisation",
  definition=function(.Object){
    return(.Objetc@weight)
  }
  )

setGeneric(
  name="setValue<-",
  def=function(.Object,Value){standardGeneric("setValue<-")}
  )

setReplaceMethod(
  f="setValue",
  signature="Realisation",
  definition=function(.Object,Value){
    .Object@value<-Value
    return(.Object)
  }
  )

setGeneric(
  name="setId<-",
  def=function(.Object,Id){standardGeneric("setId<-")}
  )

setReplaceMethod(
  f="setId",
  signature="Realisation",
  definition=function(.Object,Id){
    .Object@id<-Id
    return(.Object)
  }
  )

setGeneric(
  name="setWeight<-",
  def=function(.Object,Weight){standardGeneric("setWeight<-")}
  )

setReplaceMethod(
  f="setWeight",
  signature="Realisation",
  definition=function(.Object,Weight){
    .Object@weight<-Weight
    return(.Object)
  }
  )

setGeneric(
  name="convertToJSON",
  def=function(.Object) {standardGeneric("convertToJSON")}
  )

setMethod(
  f="convertToJSON",
  signature="Realisation",
  definition=function(.Object){
    Class=class(.Object)[1]
    Slots=slotNames(Class)
    obj<-list()
    for(i in 1:length(Slots)){
    obj[[Slots[[i]]]]<-slot(.Object, Slots[[i]])
    }
    rdata<-list()
    rdata[[Class]]=obj
    return(rdata)    
  }
  )

setGeneric(
  name="writeToJSON",
  def=function(.Object, file){standardGeneric("writeToJSON")}
  )

setMethod(
  f="writeToJSON",
  signature="Realisation",
  definition=function(.Object, file){
    Class=class(.Object)[1]
    rdata<-convertToJSON(.Object)

    #return(rdata)
    library("rjson")
    write(toJSON(rdata), file)
  }
  )
