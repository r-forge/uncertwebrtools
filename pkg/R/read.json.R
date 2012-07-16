source('C:/Users/sony/Desktop/XML_Schema_Reader/pkg/R/read.schema.R')
library("rjson")
library("XML")
f <- "C:/Users/sony/Desktop/XML_Schema_Reader/pkg/data/uncertml.xsd"
doc=xmlParse(f)
ns <- c(un="http://www.uncertml.org/2.0",xs="http://www.w3.org/2001/XMLSchema")
logLevel=1
file<-"C:/Users/sony/Desktop/test.txt"
xfile<-"C:/Users/sony/Desktop/testx.txt"

validateElements<-function(elDetails, sValue)
{
  sName<-elDetails$name
  print("Validating")
  if(!is.null(elDetails$minOccurs) & is.null(sValue))
  {
    print(paste("The value of the slot 1",sName," is required"))
    return(FALSE)
  }
  if(elDetails$type=='real')
  {
    if(!is.null(elDetails$minEx))
    {
      if(sValue<=elDetails$minEx)
      {
        print(paste("Invalid value for the slot 2",sName))
        return(FALSE)
      }     
    }
    if(!is.null(elDetails$maxEx))
    {
      if(sValue>=elDetails$maxEx)
      {
        print(paste("Invalid value for the slot 3",sName))
        return(FALSE)
      }     
    }
    if(!is.null(elDetails$minIn))
    {
      if(sValue<elDetails$minIn)
      {
        print(paste("Invalid value for the slot 4",sName))
        return(FALSE)
      }     
    }
    if(!is.null(elDetails$maxIn))
    {
      if(sValue>elDetails$maxIn)
      {
        print(paste("Invalid value for the slot 5",sName))
        return(FALSE)
      }     
    }
  }
  return(TRUE)
}

validateAttributes<-function(atDetails, sValue)
{
  sName<-atDetails$name
  if(!is.null(atDetails$required) & is.null(sValue))
  {
    print(paste("The value of the slot 1",sName," is required"))
    return(FALSE)
  }
  if(atDetails$type$type=='real')
  {
    if(!is.null(atDetails$type$minEx))
    {
      if(sValue<=atDetails$type$minEx)
      {
        print(paste("Invalid value for the slot 2",sName))
        return(FALSE)
      }     
    }
    if(!is.null(atDetails$maxEx))
    {
      if(sValue>=atDetails$maxEx)
      {
        print(paste("Invalid value for the slot 3",sName))
        return(FALSE)
      }     
    }
    if(!is.null(atDetails$type$minIn))
    {
      if(sValue<atDetails$type$minIn)
      {
        print(paste("Invalid value for the slot 4",sName))
        return(FALSE)
      }     
    }
    if(!is.null(atDetails$type$maxIn))
    {
      if(sValue>atDetails$type$maxIn)
      {
        print(paste("Invalid value for the slot 5",sName))
        return(FALSE)
      }     
    }
  }
  return(TRUE)
}


readFromJSON<-function(file)
{
  rData<-fromJSON(paste(readLines(file)))
  eName<-names(rData)[[1]]
  eObject<-new(Class=eName)
  result <- getAttributesAndElementsForElementName(eName, doc, ns)
  for (elDetails in result$subelements)
  {
	  sName<-elDetails$name
	  sValue<-rData[[eName]][[sName]]
    if(validateElements(elDetails, sValue))
    {
      slot(eObject, sName)<-sValue
    }
 	}
  for (elDetails in result$attributes)
  {
    sName<-elDetails$name
    sValue<-rData[[eName]][[sName]]
    if(validateAttributes(elDetails, sValue))
    {
      slot(eObject, sName)<-sValue
    }
  }
  
  return(eObject)
}


convertToJSON<-function(.Object)
{
  eName<-class(.Object)[1]
  result <- getAttributesAndElementsForElementName(eName, doc, ns)
  element<-list()
  for (elDetails in result$subelements)
  {
    sName<-elDetails$name
    sValue<-slot(.Object, sName)
    if(elementExistsInSchema(sName, doc, ns))
    {
      subElement<-convertToJSON(sValue)
      if(!is.null(subElement))
        element[[sName]]<-subElement[[1]]
      else
        return(NULL)
    }
    else if(typeof(sValue)=="list")
    {
      if(typeof(sValue[[1]])=="S4" & elementExistsInSchema(class(sValue[[1]])[[1]], doc, ns))
      {
        subElement<-list()
        for (j in 1:length(sValue))
        {
          en<-convertToJSON(sValue[[j]])
          if(!is.null(en))
            subElement<-c(subElement,en[[1]])
          else
            return(NULL)
        }
        element[[sName]]<-subElement
      }
    }
    else
    {
      if(validateElements(elDetails, sValue))
      {
        element[[sName]]<-sValue
      }
      else
      {
        return(NULL)
      }      
    }    
  }
  for (elDetails in result$attributes)
  {
    sName<-elDetails$name
    sValue<-slot(.Object, sName)
    if(elementExistsInSchema(sName, doc, ns))
    {
      subElement<-convertToJSON(sValue)
      if(!is.null(subElement))
        element[[sName]]<-subElement[[1]]
      else
        return(NULL)
    }
    else if(typeof(sValue)=="list")
    {
      if(typeof(sValue[[1]])=="S4" & elementExistsInSchema(class(sValue[[1]])[[1]], doc, ns))
      {
        subElement<-list()
        for (j in 1:length(sValue))
        {
          en<-convertToJSON(sValue[[j]])
          if(!is.null(en))
            subElement<-c(subElement,en[[1]])
          else
            return(NULL)
        }
        element[[sName]]<-subElement
      }
    }
    else
    {
      if(validateAttributes(elDetails, sValue))
      {
        element[[sName]]<-sValue
      }
      else
      {
        return(NULL)
      }      
    }    
  }
  rdata<-list()
  rdata[[eName]]<-element
  rdata
}

writeToJSON<-function(.Object, file)
{
  eName<-class(.Object)[1]
  rData<-convertToJSON(.Object)
  library("rjson")
  write(toJSON(rData), file)
}

readFromXML<-function(xfile)
{
  xdata<-xmlTreeParse(xfile, getDTD=FALSE)  
  en<-xmlRoot(xdata)
  eName<-xmlName(en)
  eList<-xmlToList(en)
  #create an object eObject
  eObject<-new(Class=eName)
  result <- getAttributesAndElementsForElementName(eName, doc, ns)
  for (attr in result$attributes)
  {
    sName<-attr$name
    sValue<-xmlGetAttr(en, sName)
    if(validateAttributes(attr, sValue))
    {
      slot(eObject,sName)<-sValue
    }     
  }
  for(elDetails in result$subelements)
  {
    sName<-elDetails$name
    sValue<-as(eList[[sName]], elDetails$type)
    if(validateElements(elDetails, sValue))
    {
      slot(eObject,sName) <- sValue
    }
  }
  return(eObject)
}

writeToXML<-function(.Object, file)
{
  eName<-class(.Object)[1]
  ns<-getNamespaceDefinitions(f)
  result <- getAttributesAndElementsForElementName(eName, doc, ns)
  attrsList<-c()
  for (elDetails in result$attributes)
  {
    sName<-elDetails$name
    sValue<-slot(.Object, sName)
    if(validateAttributes(elDetails, sValue))
    {
      attrsList<-c(attrsList, sName=sValue)
    }
    else
    {
        stop(paste("Invalid value for the attribute", sName, "-", sValue))
    }          
  }
  eNode<-xmlNode(eName, attrs=attrsList, namespace="un", ns)
  for (elDetails in result$subelements)
  {
    sName<-elDetails$name
    sValue<-slot(.Object, sName)
    if(validateElements(elDetails, sValue))
    {
      cNode<-xmlNode(sName, namespace="un", sValue)
      addChildren(eNode, cNode)
    }
    else
    {
      stop(paste("Invalid value for slot", sName, "-", sValue))
    }          
  }  
}