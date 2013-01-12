source('~/R/ClassNormalDistribution.R')

setClass(
  Class="UncertML"
  )

# setGeneric(
#   name="writeToJSON",
#   def=function(.Object, file){standardGeneric("writeToJSON")}
#   )
# 
# setMethod(
#   f="writeToJSON",
#   signature="UncertML",
#   definition=function(.Object, file){
#     Class=class(.Object)[1]
#     Slots=slotNames(Class)
#     obj<-list()
#     for(i in 1:length(Slots)){
#       obj[[Slots[[i]]]]<-slot(.Object, Slots[[i]])
#     }
#     rdata<-list()
#     rdata[[Class]]=obj
#     #return(rdata)
#     library("rjson")
#     write(toJSON(rdata), file)
#   }
#   )

# setGeneric(
#   name="readFromJSON",
#   def=function(file){standardGeneric("readFromJSON")}
#   )
# 
# setMethod(
#   f="readFromJSON",
#   signature="UncertML",
#   definition=function(file){
#     library("rjson")
#     rdata<-fromJSON(paste(readLines(file)))
#     className<-names(rdata)
#     slotNames<-names(rdata[[1]])
#     obj<-new(Class=className)
#     
#     
#   }
#   )

setGeneric(
  name="convertToJSON",
  def=function(.Object) {standardGeneric("convertToJSON")}
  )

setMethod(
  f="convertToJSON",
  signature="UncertML",
  definition=function(.Object){
    Class=class(.Object)[1]
    Slots=slotNames(Class)
    obj<-list()
    for(i in 1:length(Slots)){
      element<-slot(.Object, Slots[[i]])
      if (existsMethod("convertToJSON", typeof(element))){
        temp<-convertToJSON(element)
        obj[[Slots[[i]]]]<-temp[[1]]                            
      }
      else if(typeof(element)=="list"){
        if(typeof(element[[1]])=="S4" & existsMethod("convertToJSON", class(element[[1]])[1])){
          temp<-list()
          for (j in 1:length(element)){
            d<-convertToJSON(element[[j]])
            temp<-c(temp,d[[1]])
          }        
          obj[[Slots[[i]]]]<-temp
        }
      }
      else
        obj[[Slots[[i]]]]<-element
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
  signature="UncertML",
  definition=function(.Object, file){
    Class=class(.Object)[1]
    rdata<-convertToJSON(.Object)
    
    #return(rdata)
    library("rjson")
    write(toJSON(rdata), file)
  }
  )