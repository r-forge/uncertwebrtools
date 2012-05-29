## reading UNetCDF
# get spatial structure, assuming variables lon and lat and time as time
readUNetCDF <- function(file,x="lon",y="lat",time=NULL, variables=NULL){
  uncdf <- open.nc(file, write=F)
  xdim <- var.get.nc(uncdf, x)
  ydim <- var.get.nc(uncdf, y)
  tdim <- NULL

  df <- NULL
  natts <- NULL
  fileInfo <- file.inq.nc(uncdf)
  if(fileInfo$nvars>1) { # I assume all data is provided in tables/arrays
    for(variable in 0:(fileInfo$nvars-1)){
      varInfo <- var.inq.nc(uncdf,variable)
      cat("Found variable",varInfo$name,"with",varInfo$ndims,"dimension(s):")
      if(varInfo$name %in% c(x,y)) {
        cat(" used as coordinates \n")
        next()
      }
      if(!is.null(variables) && !(varInfo$name %in% variables)) {
        cat(" not part of argument \"variables\" -> dropped \n")
        next()
      }
      # How is time commonly defined?
      # seconds since 1970-1-1 00:00:00.0 +00:00
      if(!is.null(time) && varInfo$name == time) {
        tdim <- as.POSIXct("1970-1-1 00:00:00.0 +00:00",tz="GMT")+var.get.nc(uncdf,time)
        cat(" used as time \n")
        next()
      }
      if(varInfo$ndims > 1) {
        df[[varInfo$name]] <- as.numeric(var.get.nc(uncdf,varInfo$name))
	if(varInfo$natts > 1) {
	  natts[[varInfo$name]] <- list()
	  for (attID in 1:(varInfo$natts-1)) {
            natts[[varInfo$name]][[att.inq.nc(uncdf, varInfo$name, attID)$name]] <- att.get.nc(uncdf, varInfo$name, attID)
          }
        }
        cat(" added as data \n")
      } else {cat(" dropped \n")}
    }
  }
  df <- as.data.frame(df)
  if(is.null(tdim)) {
    df$x <- rep(xdim,length(ydim))
    df$y <- rep(ydim,each=length(xdim))
    # how are projections commonly defined? I am just guessing here
    # "gridded" can so far as well only be guessed
    coordinates(df) <- ~x+y
    proj4string(df) <- CRS(projargs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    gridded(df) <- TRUE
  } else {
    df <- STFDF(sp=SpatialPoints(coords=cbind(xdim,ydim), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")), time=tdim, data=df)
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
  attr(df@data,"UncertML") <- natts
  return(df)
}

# This function will override without any warning, be carefull!
writeUNetCDF <- function(newfile,spdf) {
  newUncdf <- create.nc(filename=newfile,clobber=T)
  
  # writing Conventions
  att.put.nc(newUncdf,"NC_GLOBAL",name="Conventions","NC_CHAR",value="CF-1.5 UW-1.0")  
  # writing primary variables; by now all included ones
  att.put.nc(newUncdf, "NC_GLOBAL", name="primary_variables", "NC_CHAR", value=paste(colnames(spdf@data),collapse=" "))
  spDim <- spdf@grid@cells.dim
  # has to handle different CRS
  dim.def.nc(newUncdf, dimname="lon", dimlength=spDim[1], unlim=F)
  dim.def.nc(newUncdf, dimname="lat", dimlength=spDim[2], unlim=F)

  # defining longitude
  var.def.nc(newUncdf, varname="lon","NC_DOUBLE",dimensions="lon")
  var.put.nc(newUncdf, variable="lon", data=spdf@coords[1:spDim[1],1])
  att.put.nc(newUncdf, variable="lon",name="long_name",type="NC_CHAR",value="longitude")
  att.put.nc(newUncdf, variable="lon",name="units",type="NC_CHAR",value="degrees_east")
  
  # defining latitude
  var.def.nc(newUncdf, varname="lat","NC_DOUBLE",dimensions="lat")
  var.put.nc(newUncdf, variable="lat", data=spdf@coords[(0:(spDim[2]-1))*spDim[1]+1,2])
  att.put.nc(newUncdf, variable="lat",name="long_name",type="NC_CHAR",value="latitude")
  att.put.nc(newUncdf, variable="lat",name="units",type="NC_CHAR",value="degrees_north")
  
  UncertML <- attr(spdf@data,"UncertML")

  for (variable in colnames(spdf@data)) {
    var.def.nc(newUncdf, varname=variable,"NC_DOUBLE",dimensions=c("lon","lat"))
    att.put.nc(newUncdf, variable=variable, name="missing_value",type="NC_DOUBLE",value=-999)
    var.put.nc(newUncdf, variable=variable, data=(matrix(spdf@data[[variable]],ncol=spDim[2])))
    if(!is.null(UncertML[[variable]])) {
      for(attr in names(UncertML[[variable]])) {
        attr.type <- switch(class(UncertML[[variable]][[attr]]), character="NC_CHAR", numeric="NC_DOUBLE", "NC_CHAR")
        att.put.nc(newUncdf, variable=variable, name=attr, type=attr.type, value=UncertML[[variable]][[attr]])
      }
    }
  }
  close.nc(newUncdf)
}