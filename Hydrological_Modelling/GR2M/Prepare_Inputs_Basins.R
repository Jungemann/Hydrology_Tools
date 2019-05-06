# Este script extrae series areales de PISCO P (Operativo) y PET a nivel mensual

# Remove previous variables
rm(list=ls())
# Clean console
cat('\f')


# Enter input variables
Path      <- 'D:/GR2M_PERU/GR2M_SemiDistr_Titicaca/Ilave'
Shapefile <- 'Ilave.shp'
PET       <- 'PISCOmpe_oudin_v1.1.nc'
PP        <- 'PISCOOpm_2019-marzo.Rda'
DateIni   <- '1981/01/01'
DateEnd   <- '2019/04/29'


###################################################################################################
########################################## NO MODIFICAR ###########################################
###################################################################################################

# Load packages
library(rgdal)
library(raster)
library(rgeos)


# Set work directory
setwd(Path)


# Create vector of dates
DatesMonths <- seq(as.Date(DateIni), as.Date(DateEnd), by='month')

# Load basins shapefile
Basins    <- readOGR(file.path('2_SHP', Shapefile))
nBasins   <- nrow(Basins@data)

## EXTRACT MONTHLY PET (CLIMATOLOGICAL) ============================
# Como no se cuenta con datos actuales de PET se extraerá la serie climatológica 
# para cada subcuenca de estudio

  # Read pet data
  pet  <-brick(file.path('D:/GR2M_PERU/METEORO', PET))

  # Create a climatological dataset of PET
  nDataPET <- nlayers(pet)
  index    <- rep(1:12, nDataPET/12)
  clim.pet <- stackApply(pet, index, fun=mean, na.rm=T)
  addYears <- as.numeric(format(as.Date(DateEnd), '%Y'))-1-2016
  addMonths<- as.numeric(format(as.Date(DateEnd), '%m'))
    
  # Extract data for each subbasin
  DataPET    <- matrix(NA, ncol=nBasins, nrow=length(DatesMonths))
  for (i in 1:nBasins){
    
    clim.crop <- crop(clim.pet, Basins[i,])
    clim.mask <- mask(clim.crop, Basins[i,])
    clim.mean <- cellStats(clim.mask, "mean")
    
    if (is.na(clim.mean[1])==TRUE){
      xycen         <- coordinates(gCentroid(Basins[i,]))  
      xy.point      <- SpatialPoints(matrix(as.numeric(xycen), ncol=2, nrow=1))
      crs(xy.point) <- crs(Basins)
      pet.value     <- round(as.vector(extract(clim.pet, xy.point, method='bilinear')),2)
      DataPET[,i]   <- round(c(rep(pet.value,(nDataPET/12+addYears)),pet.value[1:addMonths]),2)
    }else{
      DataPET[,i]   <- round(c(rep(clim.mean,(nDataPET/12+addYears)),clim.mean[1:addMonths]),2)
    }
  }

  
## EXTRACT MONTHLY PP ===============================
  # Read Pp data
  load(file.path('D:/GR2M_PERU/METEORO', PP))
  nDataPP <- nlayers(pp)

  # Extract data for each basin
  DataPP <- matrix(NA, ncol=nBasins, nrow=length(DatesMonths))
  for (i in 1:nBasins){
    
    pp.crop <- crop(pp, Basins[i,])
    pp.mask <- mask(pp.crop, Basins[i,])
    pp.mean <- cellStats(pp.mask, "mean")

    if (is.na(pp.mean[1])==TRUE){
      xycen         <- coordinates(gCentroid(Basins[i,]))  
      xy.point      <- SpatialPoints(matrix(as.numeric(xycen), ncol=2, nrow=1))
      crs(xy.point) <- crs(Basins)
      DataPP[,i]    <- round(as.vector(extract(pp, xy.point, method='bilinear')),2)
    }else{
      DataPP[,i]    <- round(pp.mean,2)
    }
  }
DataPP[DataPP<0] <- 0


## EXPORT EXTRACTED DATA =========================================
  df <- data.frame(DatesMonths, DataPP, DataPET)
  colnames(df) <- c('DatesR', paste0('P',1:nBasins), paste0('E',1:nBasins))
  write.table(df, file=file.path('4_INPUT','Inputs_Basins.csv'), sep=',', row.names=F)
