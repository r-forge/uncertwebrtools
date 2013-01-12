readUNetCDF <- function(file,x="lon",y="lat",time=NULL, variables=NULL, projection=NULL, realisation=NULL){
  uncdf <- open.nc(file, write=T)
  xdim <- var.get.nc(uncdf, x)
  ydim <- var.get.nc(uncdf, y)
  tdim <- NULL
  rdim <- NULL

  df <- NULL
  fileInfo <- file.inq.nc(uncdf)
  if(fileInfo$nvars>1) { # I assume all data is provided in tables/arrays
    for(variable in 0:(fileInfo$nvars-1)){
      varInfo <- var.inq.nc(uncdf,variable)
      cat("Found variable",varInfo$name,"with",varInfo$ndims,"dimension(s):")
      
      # Coordinate variables
      if(varInfo$name %in% c(x,y)) {
        cat(" used as coordinates \n")
        next()
      }
      
      # Time variable
      if(!is.null(time) && varInfo$name == time) {
        # get the unit and make a POSIXct object from the variable
        tunit <- att.get.nc(uncdf, varInfo$name, "units")
        tunit.list <- strsplit(tunit, " ")[[1]]
        startDate = paste(tunit.list[3],tunit.list[4],tunit.list[5])
        if(tunit.list[1]=="days"){
          tdim <- as.POSIXct(startDate,tz="GMT")+24*3600*var.get.nc(uncdf,time)
        }else if(tunit.list[1]=="hours"){
          tdim <- as.POSIXct(startDate,tz="GMT")+3600*var.get.nc(uncdf,time)
        }else if(tunit.list[1]=="minutes"){
          tdim <- as.POSIXct(startDate,tz="GMT")+60*var.get.nc(uncdf,time)
        }else if(tunit.list[1]=="seconds"){
          tdim <- as.POSIXct(startDate,tz="GMT")+var.get.nc(uncdf,time)
        }        
        cat(" used as time \n")
        next()
      }
      
      # Realisation Variable
      if(!is.null(realisation) && varInfo$name == realisation){
        rdim = var.get.nc(uncdf,realisation)
        next()
      }
 
      if(!is.null(variables) && !(varInfo$name %in% variables)) {
        cat(" not part of argument \"variables\" -> dropped \n")
        next()
      }
      
      # Add data from variables
      if(varInfo$ndims > 1) {    
        df[[varInfo$name]] <- as.numeric(var.get.nc(uncdf,varInfo$name))
        cat(" added as data \n")
      } else {cat(" dropped \n")}
    }
  }

  
  # create dataframe for making sp or st objects
  # simplest case
  if(is.null(time)&&is.null(realisation)){
    df <- as.data.frame(df)
  }else if(is.null(time)){
      # TODO: implement
  }else if(is.null(realisation)){
      # TODO: implement 
  }else{
    # loop through dimensions to find the order
    # order has to be x,y,t for STFDF
      varInfo <- var.inq.nc(uncdf,variables)
      dimList <- NULL
      for(d in 1:length(varInfo$dimids)){
        dimList[[d]]<- dim.inq.nc(uncdf,varInfo$dimids[d])
      }
      n1 <- dim.inq.nc(uncdf,0)$length
      n2 <- dim.inq.nc(uncdf,1)$length
      n3 <- dim.inq.nc(uncdf,2)$length
      n4 <- dim.inq.nc(uncdf,3)$length   
      
      # sorting in this netcdf file
      x4 <- rep(1:dimList[[4]]$length, each=(dimList[[3]]$length*dimList[[2]]$length*dimList[[1]]$length))
      x3 <- rep(rep(1:dimList[[3]]$length, each=(dimList[[1]]$length*dimList[[2]]$length)), times=dimList[[4]]$length)
      x2 <- rep(rep(1:dimList[[2]]$length, each=dimList[[1]]$length), times=(dimList[[4]]$length*dimList[[3]]$length))
      x1 <- rep(1:dimList[[1]]$length, (dimList[[4]]$length*dimList[[3]]$length*dimList[[2]]$length))
 
      # change to required sorting: x, y, time, realisation
      dimSort = data.frame(x1=x1, x2=x2, x3=x3, x4=x4)
      dimSort[,dimList[[1]]$id+1] = x1
      dimSort[,dimList[[2]]$id+1] = x2
      dimSort[,dimList[[3]]$id+1] = x3
      dimSort[,dimList[[4]]$id+1] = x4
            
      index <- dimSort[,1]+(dimSort[,2]-1)*n1+(dimSort[,3]-1)*n1*n2+(dimSort[,4]-1)*n1*n2*n3
     df[[1]] <- df[[1]][order(index)]
    tmp <- NULL
    var.name <- names(df)
    vdim <- 1:(length(df[[1]])/length(rdim))
    for(r in rdim){
      tmp <- cbind(tmp, df[[1]][(r-1)*length(vdim)+vdim])
    }
   
    df <- as.data.frame(tmp)
    names(df) <- paste(var.name, "_r",rdim, sep="")
  }  
  
  # if no time is available make simple SpatialDataFrame
  if(is.null(tdim)) {
    df$x <- rep(xdim,length(ydim))
    df$y <- rep(ydim,each=length(xdim))
    # how are projections commonly defined? I am just guessing here
    # "gridded" can so far as well only be guessed
    coordinates(df) <- ~x+y
    if(!is.null(projection)){
      proj4string(df) <- CRS(projection)
    }else{
      proj4string(df) <- CRS(projargs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    }
    gridded(df) <- TRUE
  } else { # for time make SpatialGridDataFrame
      if(!is.null(projection)){
        sp <- SpatialPoints(coords=cbind(rep(xdim,length(ydim)),rep(ydim,each=length(xdim))), proj4string=CRS(projection))
      }else{
        sp <-SpatialPoints(coords=cbind(rep(xdim,length(ydim)),rep(ydim,each=length(xdim))), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
        gridded(sp) <- TRUE
        df <- STFDF(sp=sp, time=tdim, data=df)
    }
  # adapted from print.nc in RNetCDF
  if (fileInfo$ngatts != 0) {
  cat("\n// global attributes:\n")
    for (j in 0:(fileInfo$ngatts - 1)) {
      attinfo <- att.inq.nc(uncdf, "NC_GLOBAL", j)
      cat(rep(" ", 8), ":", attinfo$name, sep = "")
      if (attinfo$type == "NC_CHAR") cat(" = \"", att.get.nc(uncdf, "NC_GLOBAL", j), "\" ;\n", sep = "")
      else cat(" = ", att.get.nc(uncdf, "NC_GLOBAL", j), " ;\n", sep = "")
    }
  }
  return(df)
}
# 
# str(spdf@sp)
# 
# data <- as(EU_june,"STFDF")
# newfile <- "~/EU_June.nc"
# 
# file.inq.nc(newUncdf)
# 
# str(unprojGrid)


selectType <- function(data) {
  if(is.numeric(data)) return("NC_DOUBLE")
  else return("NC_CHAR")
}

setGeneric("writeUNetCDF", function (newfile, data, ...) standardGeneric("writeUNetCDF"))

# This function will override without any warning, be careful!
writeStUNetCDF <- function(newfile, data, varStruc=NULL, x="lon", y="lat", time=NULL, realisation=NULL) {
  newUncdf <- create.nc(filename=newfile,clobber=T)

  if(fullgrid(data@sp)) {
    fullGrid <- data@sp
  } else {
    fullGrid <- as(data@sp, "SpatialGrid")
  }
  
  cellIds <- over(data@sp,fullGrid)
  
  # writing Conventions
  att.put.nc(newUncdf,"NC_GLOBAL",name="Conventions","NC_CHAR",value="CF-1.5 UW-1.0")  
  
  spDim <- fullGrid@grid@cells.dim
  dim.def.nc(newUncdf, dimname=x, dimlength=spDim[1], unlim=F)
  dim.def.nc(newUncdf, dimname=y, dimlength=spDim[2], unlim=F)

  # defining x-axis
  var.def.nc(newUncdf, varname=x,"NC_DOUBLE", dimensions=x)
  var.put.nc(newUncdf, variable=x, data=fullGrid@grid@cellcentre.offset[1]+0:(spDim[1]-1)*fullGrid@grid@cellsize[1])
  
  # defining y-axis
  var.def.nc(newUncdf, varname=y,"NC_DOUBLE",dimensions=y)
  var.put.nc(newUncdf, variable=y, data=fullGrid@grid@cellcentre.offset[2]+0:(spDim[2]-1)*fullGrid@grid@cellsize[2])
 
  # add CRS variable
  proj4info <- proj4string(fullGrid)
  if(!is.na(proj4info)) {
    splitCRS <- strsplit(proj4info, " +",fixed=T)[[1]]
    epsg <- strsplit(splitCRS[pmatch("init=",splitCRS)],"=")[[1]][2]
    var.def.nc(newUncdf,varname="crs", vartype="NC_CHAR", dimensions=NA)
    if(!is.na(epsg)) att.put.nc(newUncdf, variable="crs", name="epsg", "NC_CHAR", value=epsg)
    att.put.nc(newUncdf, variable="crs", name="proj4string", "NC_CHAR", value=proj4info)
    for(proj4arg in splitCRS) {
      keyValuePair <- strsplit(proj4arg,"=")[[1]]
      if(length(keyValuePair)<2) next
      att.put.nc(newUncdf, variable="crs", name=keyValuePair[1], "NC_CHAR", value=keyValuePair[2])
    }
  }
  
  # further CRS treatment
  if (!is.projected(fullGrid)) { # unprojected data
    att.put.nc(newUncdf, variable=x, name="long_name",type="NC_CHAR",value="longitude")
    att.put.nc(newUncdf, variable=x, name="units",type="NC_CHAR",value="degrees_east")
    att.put.nc(newUncdf, variable=x, name="axis",type="NC_CHAR",value="Lon")
    
    att.put.nc(newUncdf, variable=y, name="long_name",type="NC_CHAR",value="latitude")
    att.put.nc(newUncdf, variable=y, name="units",type="NC_CHAR",value="degrees_north") 
    att.put.nc(newUncdf, variable=y, name="axis",type="NC_CHAR",value="Lat")
  } else { # projected data
    unit <- strsplit(splitCRS[pmatch("units=",splitCRS)],"=")[[1]][2]

    att.put.nc(newUncdf, variable=x, name="axis",type="NC_CHAR",value="x")
    att.put.nc(newUncdf, variable=x, name="units",type="NC_CHAR",value=unit)
    att.put.nc(newUncdf, variable=y, name="axis",type="NC_CHAR",value="y")
    att.put.nc(newUncdf, variable=y, name="units",type="NC_CHAR",value=unit) 
    
    unprojGrid <- spTransform(as(fullGrid,"SpatialPoints"),
                              CRSobj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    var.def.nc(newUncdf, varname="lon", "NC_DOUBLE", dimensions=c(x,y))
    var.put.nc(newUncdf, variable="lon", data=matrix(unprojGrid@coords[,1],ncol=spDim[2]))
    att.put.nc(newUncdf, variable="lon", name="long_name",type="NC_CHAR",value="longitude")
    att.put.nc(newUncdf, variable="lon", name="units",type="NC_CHAR",value="degrees_east")
    
    var.def.nc(newUncdf, varname="lat", "NC_DOUBLE", dimensions=c(x,y))
    var.put.nc(newUncdf, variable="lat", data=matrix(unprojGrid@coords[,2],ncol=spDim[2]))
    att.put.nc(newUncdf, variable="lat", name="long_name",type="NC_CHAR",value="latitude")
    att.put.nc(newUncdf, variable="lat", name="units",type="NC_CHAR",value="degrees_north")
  }
  
  # add time dimension and variable
  dim.def.nc(newUncdf, dimname=time, dimlength=nrow(data@time), unlim=F)
  var.def.nc(newUncdf, varname=time,"NC_DOUBLE", dimensions=time)
  var.put.nc(newUncdf, variable=time, data=as.numeric(data@time))    
    
  # create time unit
  lag <- as.POSIXct(row.names(as.data.frame(data@time[2]))) - as.POSIXct(row.names(as.data.frame(data@time[1])))
  start <- as.POSIXct(row.names(as.data.frame(data@time[1])))-lag
    
  if(is.na(strptime(start, format="%Y-%m-%d %H:%M:%S"))) {
    timeUnit <- paste(units(lag), "since", strptime(start, format="%Y-%m-%d"),"00:00:00 00:00")
  } else {
    timeUnit <- paste(units(lag), "since", strptime(start, format="%Y-%m-%d %H:%M:%S"),"00:00")
  }    
  att.put.nc(newUncdf, variable=time, name="units",type="NC_CHAR",value=timeUnit)
  att.put.nc(newUncdf, variable=time, name="long_name",type="NC_CHAR",value="time")
  
  # add realisation dimension and variable
  # assumes that only one variable is in the data and each realisation is one column
#   if(!is.null(realisation)) {   
#     cat("Found",ncol(data@data),"realisations. \n")
#     dim.def.nc(newUncdf, dimname=realisation, dimlength=ncol(data@data), unlim=F)
#     var.def.nc(newUncdf, varname=realisation,"NC_INT", dimensions=realisation)
#     var.put.nc(newUncdf, variable=realisation, data=1:ncol(data@data))
#     att.put.nc(newUncdf, variable=realisation, name="ref",type="NC_CHAR",value="http://www.uncertml.org/samples/realisation") 
#     
#     # writing primary variables; by now all included ones
#     att.put.nc(newUncdf, "NC_GLOBAL", name="primary_variables", "NC_CHAR", value=strsplit(names(data@data)[1],"_")[[1]][1])
# 
#     # add variable data
#     if(!is.null(time)) {
#       dataDim = spDim[1]*spDim[2]
#       var.def.nc(newUncdf, varname=strsplit(names(data@data)[1],"_")[[1]][1],"NC_DOUBLE",dimensions=c(x,y,time,realisation))
#       # fill data into variable array by looping through realisations and time steps
#       for(r in 1:ncol(data@data)) {
#         for(t in 1:nrow(data@time)) {
#           var.put.nc(newUncdf, variable=strsplit(names(data@data)[1],"_")[[1]][1], 
#                      start=c(1,1,t,r), count=c(spDim[1],spDim[2],1,1), 
#                      data=matrix(data[,t]@data[,r],ncol=spDim[1]))          
#         }
#       }
#     } else {
#       var.def.nc(newUncdf, varname=strsplit(names(data@data)[1],"_")[[1]][1],"NC_DOUBLE",dimensions=c(x,y,realisation))
#       # fill data into variable array by looping through realisations
#       for(r in 1:ncol(data@data)){
#         var.put.nc(newUncdf, variable=strsplit(names(data@data)[1],"_")[[1]][1], 
#                 start=c(1,1,r), count=c(spDim[1],spDim[2],1), 
#                 data=matrix(data@data[,r],ncol=spDim[1]))               
#       }
#     }
#     
#     att.put.nc(newUncdf, variable=strsplit(names(data@data)[1],"_")[[1]][1], name="missing_value",type="NC_DOUBLE",value=-999) 
#   } else { # if no realisations are in the data

  primVars <- c()
  for(tmpVar in varStruc) { # tmpVar <- varStruc[[1]]
    if("ancVar" %in% names(tmpVar)) {
      primVars <- c(primVars, tmpVar[["name"]])
      
      var.def.nc(newUncdf, varname=tmpVar[["name"]], vartype="NC_DOUBLE", dimensions=NA)
      att.put.nc(newUncdf, variable=tmpVar[["name"]], name="ancillary_variables", type="NC_CHAR", 
                 value=paste(lapply(tmpVar[["ancVar"]], function(x) x[["name"]]), collapse=" "))
      if (!is.null(tmpVar[["longname"]])) {
        att.put.nc(newUncdf, variable=tmpVar[["name"]], name="long_name", type="NC_CHAR", value=tmpVar[["longname"]])
      }
      att.put.nc(newUncdf, variable=tmpVar[["name"]], name="ref", type="NC_CHAR", value=tmpVar[["ref"]])
      if(!is.null(tmpVar[["units"]])) {
        att.put.nc(newUncdf, variable=tmpVar[["name"]], name="units", type="NC_CHAR", value=tmpVar[["units"]])
      }
      if (is.projected(fullGrid)) {
        att.put.nc(newUncdf, variable=tmpVar[["name"]], name="grid_mapping", type="NC_CHAR", value=tmpVar[["grid_mapping"]])
      }
      
      if (tmpVar[["ancVar"]][[1]][["name"]] %in% colnames(data@data)) {
        curDim <- c(x,y,time)
      } else curDim <- c(x,y)
      
      for(ancVar in tmpVar[["ancVar"]])  { # ancVar <- tmpVar[["ancVar"]][[1]]
        var.def.nc(newUncdf, varname=ancVar[["name"]],"NC_DOUBLE", dimensions=curDim)
        att.put.nc(newUncdf, variable=ancVar[["name"]], name="missing_value", type="NC_DOUBLE", value=-999)
        if (!is.null(ancVar[["longname"]])) {
          att.put.nc(newUncdf, variable=ancVar[["name"]], name="long_name", type="NC_CHAR", value=ancVar[["longname"]])
        }
        att.put.nc(newUncdf, variable=ancVar[["name"]], name="ref", type="NC_CHAR", value=ancVar[["ref"]])
        
        tDim <- length(data@time)
        newData <- rep(NA,prod(spDim)*tDim)
        newData[as.vector(sapply((0:(tDim-1))*prod(spDim),function(x)x+cellIds))] <- data@data[[ancVar[["name"]]]]
        newData <- array(newData,dim=c(spDim,tDim))
        var.put.nc(newUncdf, variable=ancVar[["name"]], data=newData)
      
#         for (timeStamp in 1:length(data@time)) {
#           tmpSGDF <- SpatialGridDataFrame(fullGrid,data.frame(rep(NA,prod(spDim))))
#           tmpSGDF@data[cellIds,] <- data[,timeStamp,ancVar[["name"]]]@data
#           colnames(tmpSGDF@data) <- ancVar[["name"]]
#           var.put.nc(newUncdf, variable=ancVar[["name"]],
#                      data=(matrix(tmpSGDF@data[[ancVar[["name"]]]], ncol=spDim[2])),
#                      start=c(1,1,timeStamp), count=c(spDim,1))
#         }
      }
    } else {
      primVars <- c(primVars, tmpVar[["name"]])
      
      if (tmpVar[["name"]] %in% colnames(data@data)) {
        curDim <- c(x,y,time)
      } else curDim <- c(x,y)
      
      var.def.nc(newUncdf, varname=tmpVar[["name"]], vartype="NC_DOUBLE", dimensions=curDim)
      att.put.nc(newUncdf, variable=tmpVar[["name"]], name="missing_value", type="NC_DOUBLE", value=-999)
      if(!is.null(tmpVar[["longname"]])) {
        att.put.nc(newUncdf, variable=tmpVar[["name"]], name="long_name", type="NC_CHAR", value=tmpVar[["longname"]])
      }
      if(!is.null(tmpVar[["ref"]])) {
        att.put.nc(newUncdf, variable=tmpVar[["name"]], name="ref", type="NC_CHAR", value=tmpVar[["ref"]])
      }
      if (is.projected(fullGrid)) {
        att.put.nc(newUncdf, variable=tmpVar[["name"]], name="grid_mapping", type="NC_CHAR", value=tmpVar[["grid_mapping"]])
      }
      if(length(curDim==2)) {
        newData <- rep(NA,prod(spDim))
        newData[cellIds] <- data@sp@data[[tmpVar[["name"]]]]
        newData <- array(newData,dim=spDim)
        var.put.nc(newUncdf, variable=tmpVar[["name"]], data=newData)
      } else {
        tDim <- length(data@time)
        newData <- rep(NA,prod(spDim)*tDim)
        newData[as.vector(sapply((0:(tDim-1))*prod(spDim),function(x)x+cellIds))] <- data@data[[tmpVar[["name"]]]]
        newData <- array(newData,dim=c(spDim,tDim))
        var.put.nc(newUncdf, variable=tmpVar[["name"]], data=newData)
#         for (timeStamp in 1:length(data@time)) {
#           tmpSGDF <- SpatialGridDataFrame(fullGrid,data.frame(rep(NA,prod(spDim))))
#           tmpSGDF@data[cellIds,] <- data[,timeStamp, tmpVar[["name"]]]@data
#           colnames(tmpSGDF@data) <- tmpVar[["name"]]
#           var.put.nc(newUncdf, variable=tmpVar[["name"]],
#                      data=(matrix(tmpSGDF@data[[tmpVar[["name"]]]],ncol=spDim[2])),
#                      start=c(1,1,timeStamp), count=c(spDim,1))
#         }
      }
    }
  }
  
  att.put.nc(newUncdf, "NC_GLOBAL", name="primary_variables", "NC_CHAR", value=paste(primVars,collapse=" "))
  
#   cat("No realistaions found. \n")
#     if (is.null(time)) {
#       tmpdata <- as(data,"SpatialGridDataFrame")
#       for (variable in colnames(data@data)) {
#         # writing primary variables; by now all included ones
#         att.put.nc(newUncdf, "NC_GLOBAL", name="primary_variables", "NC_CHAR", value=paste(colnames(data@data),collapse=" "))
#         var.def.nc(newUncdf, varname=variable,"NC_DOUBLE",dimensions=c(x,y))
#         att.put.nc(newUncdf, variable=variable, name="missing_value",type="NC_DOUBLE",value=-999)
#         var.put.nc(newUncdf, variable=variable, data=(matrix(tmpdata@data[[variable]],ncol=spDim[2])))
#         att.put.nc(newUncdf, variable=variable, name="ref", type="NC_CHAR", value=paste("http://www.uncertml.org/distributions/normal#",strsplit(variable,"_")[[1]][2],sep="")) # this value needs to become generic
#         att.put.nc(newUncdf, variable=variable, name="shape", type="NC_CHAR", value=paste(x,y))
#       }
#     } else { # t <- 1
#       for (variable in colnames(data@data)) {
#         
#       }
#     }
#   }
  close.nc(newUncdf)
}

setMethod(writeUNetCDF,c("character", "ST"),writeStUNetCDF)