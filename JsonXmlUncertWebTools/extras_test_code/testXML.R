source('~/R/ClassParseXML.R')
source('~/R/ClassNormalDistribution.R')

xfile<-"C:\\Users\\sony\\Desktop\\testxml.txt"
xfile1<-"C:\\Users\\sony\\Desktop\\testx.txt"
dist<-new("NormalDistribution", 0, 0)
readFromXML(dist, xfile)
writeToXML(dist, xfile1)
