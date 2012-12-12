setClass(
  Class="Realisation",
  representation=representation(value="numeric", categories="character", 
                                id="numeric", weight="numeric")
  )

setMethod(
  f="initialize",
  signature="Realisation",
  definition=function(.Object, Value, category='',Id, Weight){
    .Object@value<-Value
    .Object@categories<-category
    .Object@id<-Id
    .Object@weight<-Weight
    return(.Object)
  }
  )

setGeneric(
  name="getRealisationValues",
  def=function(.Object) {standardGeneric("getRealisationValues")}
  )

setMethod(
  f="getRealisationValues",
  signature="Realisation",
  definition=function(.Object){
    return(.Object@value)
  }
  )

setGeneric(
  name="getRealisationCategories",
  def=function(.Object) {standardGeneric("getRealisationCategories")}
  )

setMethod(
  f="getRealisationCategories",
  signature="Realisation",
  definition=function(.Object){
    return(.Object@categories)
  }
  )

setGeneric(
  name="getRealisationId",
  def=function(.Object) {standardGeneric("getRealisationId")}
  )

setMethod(
  f="getRealisationId",
  signature="Realisation",
  definition=function(.Object){
    return(.Object@id)
  }
  )

setGeneric(
  name="getRealisationWeight",
  def=function(.Object) {standardGeneric("getRealisationWeight")}
  )

setMethod(
  f="getRealisationWeight",
  signature="Realisation",
  definition=function(.Object){
    return(.Object@weight)
  }
  )

setGeneric(
  name="setRealisationValues<-",
  def=function(.Object,value){standardGeneric("setRealisationValues<-")}
  )

setReplaceMethod(
  f="setRealisationValues",
  signature="Realisation",
  definition=function(.Object,value){
    .Object@value<-value
    return(.Object)
  }
  )

setGeneric(
  name="setRealisationCategories<-",
  def=function(.Object,value){standardGeneric("setRealisationCategories<-")}
  )

setReplaceMethod(
  f="setRealisationCategories",
  signature="Realisation",
  definition=function(.Object,value){
    .Object@categories<-value
    return(.Object)
  }
  )

setGeneric(
  name="setRealisationId<-",
  def=function(.Object,value){standardGeneric("setRealisationId<-")}
  )

setReplaceMethod(
  f="setRealisationId",
  signature="Realisation",
  definition=function(.Object,value){
    .Object@id<-value
    return(.Object)
  }
  )

setGeneric(
  name="setRealisationWeight<-",
  def=function(.Object,value){standardGeneric("setRealisationWeight<-")}
  )

setReplaceMethod(
  f="setRealisationWeight",
  signature="Realisation",
  definition=function(.Object,value){
    .Object@weight<-value
    return(.Object)
  }
  )