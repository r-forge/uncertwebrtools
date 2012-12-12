source('~/R/ClassDistributions.R')
source('~/R/ClassNormalDist.R')
source('~/R/ClassRealisation.R')
source('~/R/ClassSamples.R')

jfile<-"C:\\Users\\sony\\Desktop\\test.txt"
rfile<-"C:\\Users\\sony\\Desktop\\testr.txt"
dist<-new(Class="NormalDist", jfile)
number=3
rsample<-getSamples(dist,number)
sample1<-rsample@realisations[[1]]

getMean(rsample)
l<-writeToJSON(rsample, rfile)

#k<-writeToJSON(sample1, rfile)

# typeof(rsample)
# l<-as.list(getRealisations(rsample))
# l
# typeof(l)