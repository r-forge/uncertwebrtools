source('ClassNormalDistribution.R')
source('ClassRealisation.R')
source('ClassRandomSample.R')

jfile<-"C:/subversion/R-forge_UncertWeb/extras_JSON_XML/files_to_read/normaldist.xml"
x_out_file<-"C:/subversion/R-forge_UncertWeb/extras_JSON_XML/output/normalsamples.xml"
j_out_file<-"C:/subversion/R-forge_UncertWeb/extras_JSON_XML/output/normalsamples.json"

x_out_nd_file<-"C:/subversion/R-forge_UncertWeb/extras_JSON_XML/output/normaldist.xml"
j_out_nd_file<-"C:/subversion/R-forge_UncertWeb/extras_JSON_XML/output/normaldist.json"

jsonfile<-"C:/subversion/R-forge_UncertWeb/extras_JSON_XML/files_to_read/normaldist.json"
nd <- new("NormalDistribution")
nd<-readFromJSON(nd, jsonfile)

xml_ND <- new("NormalDistribution")
xml_ND<-readFromXML(xml_ND, jfile)

rsample<-getSamples(nd,3)

#writeToJSON(rsample, j_out_file)

#writeToXML(rsample, x_out_file)

#writeToJSON(nd, j_out_nd_file)
writeToXML(nd, x_out_nd_file)

writeToJSON(xml_ND, j_out_nd_file)
writeToXML(xml_ND, x_out_nd_file)


# typeof(rsample)
# l<-as.list(getRealisations(rsample))
# l
# typeof(l)