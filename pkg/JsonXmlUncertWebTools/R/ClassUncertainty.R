source('ClassParseJSON.R')
source('ClassParseXML.R')
setClass(
  Class="Uncertainty",
  contains="parseXML","parseJSON"
)

