setClass(
  Class="parseXML",
  representation=representation(xdata="list", rdata="list")
  )

setGeneric(
  name="readFromXML",
  def=function(.Object, file){standardGeneric("readFromXML")}
  )

setMethod(
  f="readFromXML",
  signature="parseXML",
  definition=function(.Object, file){
   library("XML")
   xdata<-xmlTreeParse(file, getDTD=FALSE)
   root<-xmlRoot(xdata)
   className<-xmlName(root)
   slots<-list()
   for (i in 1:xmlSize(root)){
     slots[xmlName(root[[i]])]<-root[[i]][1]
   }
   rdata<-list()
   rdata[[className]]=slots
   return(rdata)
  }  
  )

setGeneric(
  name="writeToXML",
  def=function(.Object, file){standardGeneric("writeToXML")}
  )

setMethod(
  f="writeToXML",
  signature="parseXML",
  definition=function(.Object, file){
       
  }
  )