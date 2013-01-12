source('C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/pkg/R/ClassDistributions.R')
#source('C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/pkg/R/ClassNormalDist.R')
#source('C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/pkg/R/ClassRealisation.R')
#source('C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/pkg/R/ClassSamples.R')

jsonfile<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/files_to_read/normaldist.json"
jsonNDdata<-fromJSON(jsonfile)

jfile<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/files_to_read/normaldist.xml"
rfile<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/output/normalsamples.xml"
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