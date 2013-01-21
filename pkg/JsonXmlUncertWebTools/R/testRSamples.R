source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassNormalDistribution.R')
source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassRealisation.R')
source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassRandomSample.R')

jsonfile<-"C:/subversion/Rforge_UncertWebTools/extras_JSON_XML/files_to_read/normaldist.json"
nd <- new("NormalDistribution")
nd<-readFromJSON(nd, jsonfile)

jfile<-"C:/subversion/Rforge_UncertWebTools/extras_JSON_XML/files_to_read/normaldist.xml"
x_out_file<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/output/normalsamples.xml"
j_out_file<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/output/normalsamples.json"

x_out_nd_file<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/output/normaldist.xml"
j_out_nd_file<-"C:/subversion/Rforge_UncertWebTools/JsonXmlUncertWebTools/output/normaldist.json"

xml_ND<-readFromXML(new("Uncertainty"),jfile)
number=3
rsample<-getSamples(xml_ND,number)

#writeToJSON(rsample, j_out_file)
#writeToXML(rsample, x_out_file)
writeToJSON(xml_ND, j_nd_out_file)
writeToXML(xml_ND, x_nd_out_file)


# typeof(rsample)
# l<-as.list(getRealisations(rsample))
# l
# typeof(l)