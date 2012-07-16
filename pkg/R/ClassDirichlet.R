setClass(
  Class="DirichletDistribution",
  representation=representation(concentration="numeric")
  )

setMethod(
  f="initialize",
  signature="DirichletDistribution",
  definition=function(.Object, concentration){
    .Object@concentration<-concentration
    return(.Object)
  }
  )

setGeneric(
  name="getDirichletConcentration",
  def=function(.Object) {standardGeneric("getDirichletConcentration")}
  )

setMethod(
  f="getDirichletConcentration",
  signature="DirichletDistribution",
  definition=function(.Object){
    return(.Object@concentration)
  }
  )

setGeneric(
  name="setDirichletConcentration<-",
  def=function(.Object,value){standardGeneric("setDirichletConcentration<-")}
  )

setReplaceMethod(
  f="setDirichletConcentration",
  signature="DirichletDistribution",
  definition=function(.Object,value){
    .Object@concentration<-value
    return(.Object)
  }
  )

setGeneric(
  name="getDirichletSamples",
  def=function(.Object,number) {standardGeneric("getDirichletSamples")}
  )

setMethod(
  f="getDirichletSamples",
  signature="DirichletDistribution",
  definition=function(.Object, number){
    sample<-c()
    gSample<-c()
    dim<-length(.Object@concentration)
    for(x in 1:dim)
    {
      gSample[[x]]<-rgamma(number, shape=.Object@concentration[[x]], scale=1)
    }
    for(i in 1:number)
    {
      s=0
      for(k in 1:dim)
      {
        s<-s+gSample[[k]][i]
      }
      temp<-c()
      for(j in 1:dim)
      {
        temp<-c(temp, gSample[[j]][i]/s)
      }
      sample[[i]]<-temp
      sample[i]<-new(Class="Realisation", Value=temp, Id=i, Weight=1/number)
      
    }
    rsample<-new(Class="RandomSample", sample)
    return(rsample)
  }
  )

setGeneric(
  name="getDirichletMean",
  def=function(.Object){standardGeneric("getDirichletMean")}
  )

setMethod(
  f="getDirichletMean",
  signature="DirichletDistribution",
  definition=function(.Object)
  {
    mean<-c()
    s<-sum(.Object@concentration)
    for(i in 1:length(.Object@concentration))
    {
      mean[[i]]<-.Object@concentration[[i]]/s
    }
    return(mean)  
  }
  )

setGeneric(
  name="getDirichletVariance",
  def=function(.Object){standardGeneric("getDirichletVariance")}
  )

setMethod(
  f="getDirichletVariance",
  signature="DirichletDistribution",
  definition=function(.Object)
  {
    var<-c()
    s<-sum(.Object@concentration)
    for(i in 1:length(.Object@concentration))
    {
      c<-.Object@concentration[[i]]
      var[[i]]<-(c*(s-c))/((s^2)*(s+1))
    }
    return(var)
  }
  )

setGeneric(
  name="getDirichletStandardDeviation",
  def=function(.Object){standardGeneric("getDirichletStandardDeviation")}
  )

setMethod(
  f="getDirichletStandardDeviation",
  signature="DirichletDistribution",
  definition=function(.Object)
  {
    var<-getDirichletVariance(.Object)
    sd<-c()
    for(i in 1:length(var))
    {
      sd[[i]]<-sqrt(var[[i]])
    }
    return(sd)  
  }
  )

setGeneric(
  name="getDirichletMode",
  def=function(.Object){standardGeneric("getDirichletMode")}
  )

setMethod(
  f="getDirichletMode",
  signature="DirichletDistribution",
  definition=function(.Object)
  {
    m<-c()
    s<-sum(.Object@concentration)
    for(i in 1:length(.Object@concentration))
    {
      c<-.Object@concentration[[i]]
      m[[i]]<-(c-1)/(s-length(.Object@concentration))
    }
    return(m)
  }
  )