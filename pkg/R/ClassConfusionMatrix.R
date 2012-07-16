setClass(
  Class="ConfusionMatrix",
  representation=representation(sourceCategories="character", 
                                targetCategories="character",
                                counts="numeric")
  )

setMethod(
  f="initialize",
  signature="ConfusionMatrix",
  definition=function(.Object, Source, Target, Count)
  {
    .Object@sourceCategories<-Source
    .Object@targetCategories<-Target
    .Object@counts<-Count
    return(.Object)    
  }
  )

setGeneric(
  name="getConfusionMatrixSourceCategories",
  def=function(.Object){standardGeneric("getConfusionMatrixSourceCategories")}
  )

setMethod(
  f="getConfusionMatrixSourceCategories",
  signature="ConfusionMatrix",
  definition=function(.Object)
  {
    return(.Object@sourceCategories)    
  }
  )

setGeneric(
  name="getConfusionMatrixTargetCategories",
  def=function(.Object){standardGeneric("getConfusionMatrixTargetCategories")}
  )

setMethod(
  f="getConfusionMatrixTargetCategories",
  signature="ConfusionMatrix",
  definition=function(.Object)
  {
    return(.Object@targetCategories)    
  }
  )

setGeneric(
  name="getConfusionMatrixCount",
  def=function(.Object, sourceCategory, targetCategory){standardGeneric("getConfusionMatrixCount")}
  )

setMethod(
  f="getConfusionMatrixCount",
  signature="ConfusionMatrix",
  definition=function(.Object, sourceCategory, targetCategory)
  {
    s<-which(.Object@sourceCategories==sourceCategory)
    t<-which(.Object@sourceCategories==targetCategory)
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    return(countArray[t,s])    
  }
  )

setGeneric(
  name="addConfusionMatrixSourceCategories<-",
  def=function(.Object, Category){standardGeneric("addConfusionMatrixSourceCategories<-")}
  )

setReplaceMethod(
  f="addConfusionMatrixSourceCategories",
  signature="ConfusionMatrix",
  definition=function(.Object, Category)
  {
    .Object@sourceCategories<-c(.Object@sourceCategories, Category)
    return(.Object)   
  }
  )

setGeneric(
  name="addConfusionMatrixTargetCategories<-",
  def=function(.Object, Category){standardGeneric("addConfusionMatrixTargetCategories<-")}
  )

setReplaceMethod(
  f="addConfusionMatrixTargetCategories",
  signature="ConfusionMatrix",
  definition=function(.Object, Category)
  {
    .Object@targetCategories<-c(.Object@targetCategories, Category)
    return(.Object)   
  }
  )

setGeneric(
  name="setConfusionMatrixCount<-",
  def=function(.Object, sourceCategory, targetCategory, value)
    {standardGeneric("setConfusionMatrixCount<-")}
  )

setReplaceMethod(
  f="setConfusionMatrixCount",
  signature="ConfusionMatrix",
  definition=function(.Object, sourceCategory, targetCategory, value)
    {
      s<-which(.Object@sourceCategories==sourceCategory)
      t<-which(.Object@targetCategories==targetCategory)
      countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                         ncol=length(.Object@sourceCategories), byrow=TRUE)
      countArray[t,s]<-value
      .Object@counts<-c(t(countArray))
      return(.Object)   
    }
  )

setGeneric(
  name="getConfusionMatrixCommissionErrorPercentage",
  def=function(.Object, sourceCategory){standardGeneric("getConfusionMatrixCommissionErrorPercentage")}
)

setMethod(
  f="getConfusionMatrixCommissionErrorPercentage",
  signature="ConfusionMatrix",
  definition=function(.Object, sourceCategory)
  {
    s<-which(.Object@sourceCategories==sourceCategory)
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    trueTotal<-sum(countArray[s,])
    commissionError<-trueTotal-countArray[s,s]
    return((commissionError*100)/trueTotal)    
  }
)

setGeneric(
  name="getConfusionMatrixOmissionErrorPercentage",
  def=function(.Object, sourceCategory){standardGeneric("getConfusionMatrixOmissionErrorPercentage")}
)

setMethod(
  f="getConfusionMatrixOmissionErrorPercentage",
  signature="ConfusionMatrix",
  definition=function(.Object, sourceCategory)
  {
    s<-which(.Object@sourceCategories==sourceCategory)
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    predictedTotal<-sum(countArray[,s])
    OmissionError<-predictedTotal-countArray[s,s]
    return((OmissionError*100)/predictedTotal)    
  }
)

setGeneric(
  name="getConfusionMatrixKappa",
  def=function(.Object){standardGeneric("getConfusionMatrixKappa")}
)

setMethod(
  f="getConfusionMatrixKappa",
  signature="ConfusionMatrix",
  definition=function(.Object)
  {
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    totalItems<-sum(countArray)
    diagSum<-sum(diag(countArray))
    t<-0
    for(i in 1:length(.Object@sourceCategories))
    {
      t<-t+(sum(countArray[,i])*sum(countArray[i,]))
    }
    kappa<-(totalItems*diagSum-t)/(totalItems^2-t)
    return(kappa)    
  }
)