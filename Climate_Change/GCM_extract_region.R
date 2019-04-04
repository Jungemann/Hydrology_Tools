##############################################################################
# Title   : GCM_extract_region.R
# Purpose : Read netCDF files from NEX-GDDP for GCM CMIP5 (Local stored)
# Author  : Harold Llauca
##############################################################################

GCM_extract_region <- function(path, region, period, var){

# path	 : Path where netCDF (.nc) files are stored
# region : Limits of the study area [min.lon, max.lon, min.lat, max.lat]
# period : 'Historical' - Processing historical data
#		       'Future'     - Processing future data
# var	   : Select a variable to process
#          'Pp'   - Rainfall
#		       'Tmax' - Maximum temperature
#		       'Tmin' - Minimum temperature


  # Initial time
  start.time <- Sys.time()
  
  # Verify packages
  if("ncdf4" %in% rownames(installed.packages()) == FALSE){
    install.packages("ncdf4")
  }
  if("stringr" %in% rownames(installed.packages()) == FALSE){
    install.packages("stringr")
  }
  
  # Load packages
  library(ncdf4)
  library(stringr)
  
  # Read filenames
  files.all <- list.files(path, pattern='nc$')
  
  # Extract model's names
  names <- vector()
  for (k in 1:length(files.all)){
    y <- strsplit(files.all, '[_]')
    y <- strsplit(y[[k]], '[ ]')
    names[k] <- y[[6]]
  }
  model <- unique(names)

  # Extract initial year from all files
  y <- strsplit(files.all[1], '[_]')
  y <- strsplit(y[[1]], '[ ]')
  y.ini <- as.numeric(str_extract(y[[7]], "...."))
  
  # Assign variables to numbers
  if (var=='Pp')  {id.var <- 1}
  if (var=='Tmax'){id.var <- 2}
  if (var=='Tmin'){id.var <- 3}

  # Variable's names
  if (period=='Historical'){
    var.type <-c('pr1', 'tasmax1','tasmin1')
    location <-c('lon1', 'lat1')
  } 
  if (period=='Future'){
    var.type <-c('pr', 'tasmax','tasmin')
    location <-c('lon', 'lat')
  }

## Extract array dimension ===========================================================
  # Extract  lat & lon from netCDF
  ncin  <- nc_open(paste(path, files.all[1], sep='/'))
  lon   <- ncvar_get(ncin, location[1])-360
  lat   <- ncvar_get(ncin, location[2])
  
  # Set extension of the area of study
  min.lon <- region[1]
  max.lon <- region[2]
  min.lat <- region[3]
  max.lat <- region[4]
  
  # Subset region
  lon.region <- subset(lon, (lon> min.lon) & (lon< max.lon))
  lat.region <- subset(lat, (lat> min.lat) & (lat< max.lat))
  
  # Close netCDF
  nc_close(ncin)
  
  # Calculate no.years
  no.years  <- length(files.all[str_detect(files.all, paste(model[1],'_',sep=''))])
  
  # Create an array [lon x lat x day x year x model]
  dta.reg <- array(NA, dim=c(length(lon.region), length(lat.region), 366, no.years, length(model)))

## Read all netCDF files ===============================================================
  # Read each GCM model 
  for (m in 1:length(model)){
     files.model    <- files.all[str_detect(files.all, paste(model[m],'_',sep=''))]
     no.files.model <- length(files.model)
    
    # Read files of an GCM model
      for (w in 1:no.files.model){
        
        ## Show message
        count.all   <- round(((m)/length(model)*100),2)
        count.model <- round((w/no.files.model)*100,2)
        cat('\f')
        print(paste('Total de modelos:...', length(model)), sep='')
        print(paste('Modelos procesados:...', count.all,'%',sep=''))
        print(paste('Procesando modelo Nยบ-',m,' ',model[m],':...', count.model,'%',sep=''))
        
        # Open netCDF
        ncin <- nc_open(paste(path, files.model[w],sep='/'))
        
        # Read netCDF
        lon.gcm <- ncvar_get(ncin, location[1])-360
        lat.gcm <- ncvar_get(ncin, location[2])
        lon.reg <- subset(lon.gcm, (lon.gcm> min.lon) & (lon.gcm< max.lon))
        lat.reg <- subset(lat.gcm, (lat.gcm> min.lat) & (lat.gcm< max.lat))
        
        if (var=='Pp'){
          dta <- ncvar_get(ncin, var.type[id.var])*86400
        } else {
          dta <- ncvar_get(ncin, var.type[id.var])-273
        }
        
        # Subset data for the study area [lat x lon x day x year x model]
        if (period=='Historical'){
          dta     <- dta[, match(lat.reg, lat.gcm), match(lon.reg, lon.gcm)]
          dta     <- aperm(dta, c(3,2,1))
          dta.reg[c(1:dim(dta)[1]),c(1:dim(dta)[2]),c(1:dim(dta)[3]),w,m] <- dta
        }
        
        if (period=='Future'){
          dta     <- dta[match(lon.reg,lon.gcm), match(lat.reg,lat.gcm),]
          dta.reg[c(1:dim(dta)[1]),c(1:dim(dta)[2]),c(1:dim(dta)[3]),w,m] <- dta
        }
      }
    }

# Output variables
 Yreturn <- list(data=dta.reg, lat=lat.region, lon=lon.region, model=model, yini=y.ini, var=var, per=period)
 return(Yreturn)
 
 # Show computing time
 end.time   <- Sys.time()
 time.taken <- end.time - start.time
 print(round(time.taken))
 alarm()
}
