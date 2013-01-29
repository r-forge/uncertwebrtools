library("rjson")

setClass(
  Class="parseJSON"
)

setGeneric(
  name="convertToJSON",
  def=function(.Object) {standardGeneric("convertToJSON")}
  )

setMethod(
  f="convertToJSON",
  signature="parseJSON",
  definition=function(.Object){
    Class=class(.Object)[1]
    Slots=slotNames(Class)
    obj<-list()
    for(i in 1:length(Slots)){
      
      # TODO check if this is necessary
      if (existsMethod("convertToJSON", typeof(slot(.Object,Slots[[i]])))) {
        
        obj[[Slots[[i]]]]<-convertToJSON(slot(.Object, Slots[[i]]))
      } else {
        
        obj[[Slots[[i]]]]<-slot(.Object, Slots[[i]])
      }
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
  signature="parseJSON",
  definition=function(.Object, file){
    Class=class(.Object)[1]
    rdata<-convertToJSON(.Object)
    
    write(toJSON(rdata), file)
  }
)
  
  setGeneric(
    name="readFromJSON",
    def=function(.Object, file){standardGeneric("readFromJSON")}
  )
  
  setMethod(
    f="readFromJSON",
    signature="parseJSON",
    definition=function(.Object, file){

        tempJSON<-fromJSON(paste(readLines(file)))
        
        className<-names(tempJSON)
        
        # use this classname to instantiate the object
        .Object <- new(className)
        
        for (i in 1:length(names(tempJSON[[1]]))){
          # TODO check the value type and validate it against the schema
          # for now, just check the slot type
          if (is.numeric(slot(.Object,names(tempJSON[[1]])[i]))) {
            
            slot(.Object,names(tempJSON[[1]])[i])<-as.numeric(tempJSON[[1]][[i]])
          } else {
            # this is the point at which to check whether the slot has 
            # its own readFromJSON method - for nested types
            if (existsMethod("readFromJSON", typeof(slot(.Object,names(tempJSON[[1]])[i])))) {
              
              #read the object with its own method
              tempU <- new("Uncertainty")
              readFromXML(tempU,tempJSON[[1]][[i]])
              
              slot(.Object,names(tempJSON[[1]])[i])<-tempU
            } else {
              slot(.Object,names(tempJSON[[1]])[i])<-tempJSON[[1]][[i]]
            }
            
          }
          
        }
        return (.Object)
      
    }
)

