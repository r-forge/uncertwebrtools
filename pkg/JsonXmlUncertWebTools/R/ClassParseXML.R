library("XML")
library("xmlschemaHelper")

setClass(
  Class="parseXML"
  )

setGeneric(
  name="readFromXML",
  def=function(.Object, file){standardGeneric("readFromXML")}
  )

setMethod(
  f="readFromXML",
  signature="parseXML",
  definition=function(.Object, file){
   # TODO - do the file reading outside and change this incoming to 'currentRoot'
   # also maybe change the name of the function to 'convertFromXML'
    xdata<-xmlTreeParse(file, getDTD=FALSE)
   root<-xmlRoot(xdata)
   # TODO above will be removed - also pass in useful namespaces, schema record etc
   className<-xmlName(root)
   
   .Object <- new(className)

    for (i in 1:xmlSize(root)){
      # TODO check the value type and validate it against the schema
      # for now, just check the slot type
      if (is.numeric(slot(.Object,xmlName(root[[i]])))) {
        
        slot(.Object,xmlName(root[[i]]))<-as.numeric(xmlValue(root[[i]]))
      } else {
        # this is the point at which to check whether the slot has 
        # its own readFromXML method - for nested types
        if (existsMethod("readFromXML", typeof(slot(.Object,xmlName(root[[i]]))))) {
          
          #read the object with its own method
          tempU <- new("Uncertainty")
          readFromXML(tempU,root[[i]])
          #TODO - make sure that this can handle a node, not just a file
          slot(.Object,xmlName(root[[i]]))<-tempU
        } else {
          slot(.Object,xmlName(root[[i]]))<-xmlValue(root[[i]])
        }
        
      }
      
    }
   return (.Object)
  }  
  )

setGeneric(
  name="writeToXML",
  def=function(.Object, file, theXSDdoc){standardGeneric("writeToXML")}
)

setMethod(
  f="writeToXML",
  signature="parseXML",
  definition=function(.Object, file, theXSDdoc){
    data <- convertToXML(.Object, theXSDdoc)
    write(toString(data), file)
    
  }
)
setGeneric(
  name="convertToXML",
  def=function(.Object, theXSDdoc){standardGeneric("convertToXML")}
)

setMethod(
  f="convertToXML",
  signature="parseXML",
  definition=function(.Object, theXSDdoc){
    
    #TODO pass in the current node or buffer, not a whole file
    
    # temporary: parsing hardwired xsd here
    # TODO move this code outside when ready

    # Get the class name of the uncertainty type
    cN <- class(.Object)[[1]]
    
    # Get the schema def from the xsd - will use the one that's been passed in
    o <- getAttributesAndElementsForElementName(cN, "../../../../Rforge_XML_Schema_reader/extras/schema_document/uncertml.xsd")
    #print(length(o$attributes))
    # Write out element with type and any attributes
    # Create element
    
    data<-xmlNode(className, 
                  attrs=c(xmlns="http://www.uncertml.org/2.0"))
    # For all attributes...
    if (length(o$attributes)>0)
    {
      
      for (i in 1:length(o$attributes)){
        #print("a")
        # add attribute to element
        aname <- o$attributes[[i]]$name
        avalue <- to.character(slot(.Object,o$attributes[[i]]$name))
        #print("a2")
        # At this point, can check all sort of things about min/max value, optional etc. TODO
        addAttributes(data,aname=avalue)
      }
    }
 
    #TODO do namespace properly and add 'un:' if necessary - can specify namespace on xmlNode
    print(length(o$subelements))
    # For all sub-elements...
    if (length(o$subelements)>0)
    {
      for (i in 1:length(o$subelements)){
        # add sub-element to element
        
        sname <- o$subelements[[i]]$name
        
        # Keep checking for 'existsMethod' in case of nested Uncertainty types.
        if (sname == "")
        {
          print("blank name")
          if (existsMethod("convertToXML", typeof(slot(.Object,o$subelements[[i]]$type)))) {
            
            #temp
            print("convertToXML exists")
            print(o$subelements[[i]]$type)
            #endtemp
            
            # write this one out and add it
            se <- convertToXML(slot(.Object,o$subelements[[i]]$name), file, theXSDdoc)
            data$addNode(se)
            
          }
        } else {
          
          svalue <- slot(.Object,sname)
          
          # can be multiple...
          # TODO this 'values' may change as UncertML becomes more atomic
          # MAY need to format multiple values
          
          append.xmlNode(data, xmlNode(sname, as.character(svalue)))
          #data$addNode(sname, as.character(svalue))
          #temp
          print (toString(data))
          #end temp
        }
      }
    }
       
  }
  )