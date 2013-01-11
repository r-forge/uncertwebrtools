setClass(
  Class="Distributions",
  representation=representation(jdata="character", rdata="list")
  )

setGeneric(
  name="readFromJSON",
  def=function(.Object,file){standardGeneric("readFromJSON")}
  )

setMethod(
  f="readFromJSON",
  signature="Distributions",
  definition=function(.Object,file){
    library("rjson")
    .Object@rdata<-fromJSON(paste(readLines(file)))
    return(.Object)
  }
  )

setGeneric(
  name="writeToJSON",
  def=function(.Object,file){standardGeneric("writeToJSON")}
  )

setMethod(
  f="writeToJSON",
  signature="Distributions",
  definition=function(.Object,file){
    library("rjson")
    .Object@jdata<-toJSON(.Object@rdata)
    write(.Object@jdata,file)
    return(.Object)
  }
  )