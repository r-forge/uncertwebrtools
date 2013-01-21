source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassParseJSON.R')
source('C:/subversion/Rforge_UncertWebTools/pkg/JsonXmlUncertWebTools/R/ClassParseXML.R')
setClass(
  Class="Uncertainty",
  contains="parseXML","parseJSON"
)

