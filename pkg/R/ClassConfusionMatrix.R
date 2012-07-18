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
  name="getConfusionMatrixUserAccuracy",
  def=function(.Object, CategoryName){standardGeneric("getConfusionMatrixUserAccuracy")}
  )

setMethod(
  f="getConfusionMatrixUserAccuracy",
  signature="ConfusionMatrix",
  definition=function(.Object, CategoryName)
  {
    s<-which(.Object@sourceCategories==CategoryName)
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    accuracy<-countArray[s,s]*100/sum(countArray[,s])   
    return(accuracy)
  }
  )

setGeneric(
  name="getConfusionMatrixProducerAccuracy",
  def=function(.Object, CategoryName){standardGeneric("getConfusionMatrixProducerAccuracy")}
  )

setMethod(
  f="getConfusionMatrixProducerAccuracy",
  signature="ConfusionMatrix",
  definition=function(.Object, CategoryName)
  {
    s<-which(.Object@sourceCategories==CategoryName)
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    accuracy<-countArray[s,s]*100/sum(countArray[s,])   
    return(accuracy)
  }
  )

setGeneric(
  name="getConfusionMatrixCommissionErrorPercentage",
  def=function(.Object, CategoryName){standardGeneric("getConfusionMatrixCommissionErrorPercentage")}
)

setMethod(
  f="getConfusionMatrixCommissionErrorPercentage",
  signature="ConfusionMatrix",
  definition=function(.Object, CategoryName)
  {
    acc<-getConfusionMatrixUserAccuracy(.Object, CategoryName)
    return(100-acc)    
  }
)

setGeneric(
  name="getConfusionMatrixOmissionErrorPercentage",
  def=function(.Object, CategoryName){standardGeneric("getConfusionMatrixOmissionErrorPercentage")}
)

setMethod(
  f="getConfusionMatrixOmissionErrorPercentage",
  signature="ConfusionMatrix",
  definition=function(.Object, CategoryName)
  {
    acc<-getConfusionMatrixProducerAccuracy(.Object, CategoryName)
    return(100-acc)
  }
)

setGeneric(
  name="getConfusionMatrixMisallocationPercentage",
  def=function(.Object, actualCategoryName, predictedCategoryName){standardGeneric("getConfusionMatrixMisallocationPercentage")}
  )

setMethod(
  f="getConfusionMatrixMisallocationPercentage",
  signature="ConfusionMatrix",
  definition=function(.Object, actualCategoryName, predictedCategoryName)
  {
    s<-which(.Object@sourceCategories==actualCategoryName)
    t<-which(.Object@sourceCategories==predictedCategoryName)    
    countArray<-matrix(.Object@counts, nrow=length(.Object@targetCategories),
                       ncol=length(.Object@sourceCategories), byrow=TRUE)
    total<-sum(countArray[s,])   
    misallocated<-countArray[s,t]
    return(list(percentage=misallocated*100/total, samplSize=total))
  }
  )

setGeneric(
  name="getConfusionMatrixKhat",
  def=function(.Object){standardGeneric("getConfusionMatrixKhat")}
)

setMethod(
  f="getConfusionMatrixKhat",
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