setClass(
  Class="Probability",
  representation=representation(probabilities="numeric", gt="numeric", lt="numeric", 
                                ge="numeric", le="numeric")
  )

setMethod(
  f="initialize",
  signature="Probability",
  definition=function(.Object, description, value, probability)
    {
      for(i in 1:length(description))
      {
        desc<-description[[i]]
        val<-value[[i]]
        if(length(which(slotNames(.Object)==desc))>0)
        {
          slot(.Object,desc)<-val
        }
        else
        {
          stop("The description can be any one of : 'lt', 'gt', 'le', 'ge'")
        }          
      }
      if(probability<=1 & probability>=0)
        .Object@probabilities<-probability
      else
        stop("The probability value should be between 0 and 1(both inclusive)")
      return(.Object)
    }
  )

setGeneric(
  name="getProbabilityValue",
  def=function(.Object){standardGeneric("getProbabilityValue")}
  )

setMethod(
  f="getProbabilityValue",
  signature="Probability",
  definition=function(.Object)
  {
    desc<-c()
    if(!is.null(.Object@lt))
    {
      desc$"lt"<-.Object@lt
    }
    if(!is.null(.Object@le))
    {
      desc$"le"<-.Object@le
    }
    if(!is.null(.Object@gt))
    {
      desc$"gt"<-.Object@gt
      if(!is.null(.Object@lt))
      {
        if(.Object@lt<.Object@gt)
          stop("Invalid domain set")
      }
      if(!is.null(.Object@le))
      {
        if(.Object@le<.Object@gt)
          stop("Invalid domain set")
      }
      
    }
    if(!is.null(.Object@ge))
    {
      desc$"ge"<-.Object@ge
      if(!is.null(.Object@lt) & .Object@lt<.Object@ge)
      {
        stop("Invalid domain set")
      }
      if(!is.null(.Object@le) & .Object@le<.Object@ge)
      {
        stop("Invalid domain set")
      }
    }
    if(.Object@probabilities<=1 & .Object@probabilities>=0)
      p<-.Object@probabilities
    else
      stop("The probability value should be between 0 and 1(both inclusive)")
    return(list, description=desc, proabability=p)
  }
  )

setGeneric(
  name="setProbabilityValue<-",
  def=function(.Object, description, points, value)
    {standardGeneric("setProbabilityValue<-")}
  )

setReplaceMethod(
  f="setProbabilityValue",
  signature="Probability",
  definition=function(.Object, description, points, value)
    {
      for(i in 1:length(description))
      {
        desc<-description[[i]]   
        val<-points[[i]]
        if(length(which(slotNames(.Object)==desc))>0)
        {
          slot(.Object,desc)<-val
        }          
        else
        {
          stop("The description can be any one of : 'lt', 'gt', 'le', 'ge'")
        }
      }
      if(value<=1 & value>=0)
        .Object@probabilities<-value
      else
        stop("The probability value should be between 0 and 1(both inclusive)")
      return(.Object)   
    }
  )
