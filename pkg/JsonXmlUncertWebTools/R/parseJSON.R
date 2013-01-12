setGeneric(
  name="convertToJSON",
  def=function(.Object) {standardGeneric("convertToJSON")}
  )

setMethod(
  f="convertToJSON",
  signature="Uncertainty",
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
  signature="Uncertainty",
  definition=function(.Object, file){
    Class=class(.Object)[1]
    rdata<-convertToJSON(.Object)

    library("rjson")
    write(toJSON(rdata), file)
  }
  )

