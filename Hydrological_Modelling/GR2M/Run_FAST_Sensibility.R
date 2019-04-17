# This script is to compute sensibility analysis for gr2m hydrological model using the 
# Faster Fourier Transformation (FAST) algorithm
# By Harold Llauca

# Remove previous variables and clean console
rm(list=ls())
cat('\f')

# Load libraries
library(airGR)
library(fast)
library(tictoc)
library(rgdal)


# Enter input variables
Path      <- 'D:/GR2M_PERU/GR2M_saFast_Test'
File      <- 'Inputs_Basins.csv'
Basin     <- 'Basin.shp'
ParMin    <- c(1,0.01)
ParMax    <- c(2000,2)
Warm.Ini  <- '01/1981'
Warm.End  <- '12/1987'
Model.Ini <- '01/1988'
Model.End <- '03/2019'


######################################################################################################
######################################################################################################
######################################################################################################
tic()

# Load require functions
source('run_gr2m_airGR.R')


# Set work directory
setwd(Path)

# Load shapefile
Shp <- readOGR(file.path('2_SHP', Basin))
nBasins <- nrow(Shp@data)

# Read input data
Data  <- read.table(file.path('1_DATA', File), sep=',', header=T)
Data$DatesR <- as.POSIXct(Data$DatesR, "GMT", tryFormats=c("%Y-%m-%d", "%d/%m/%Y"))


# Range of X1 and X2 parameters (minimum and maximum)
param <- fast_parameters(minimum=ParMin, maximum=ParMax, cukier=F, factor=70) # 1050 set of parameters


# Auxiliary variables
npar <- nrow(param)
out  <- matrix(NA, ncol=ncol(param), nrow=npar)
rr   <- matrix(NA, ncol=ncol(param), nrow=nBasins)
rv   <- matrix(NA, ncol=ncol(param), nrow=nBasins)

# Start iteration of each basin ...
for (j in 1:nBasins) {
  
  # Print message
  cat('\f')
  message(paste0('Processing...', round(100*j/nBasins,2),'%'))
  
  # SUbset data for each basin
  Database <- Data[,c(1,j+1,j+1+nBasins)]
  
  # Start iteration for each set of parameters
  for (i in 1:npar) {

    # Run GR2M model
    out[i,]  <- run_gr2m_airGR(Model.Input=Database,
                               Model.Parameter=as.numeric(as.vector(param[i,])),
                               Warm.Ini, Warm.End, Model.Ini, Model.End)
  }
  
  # Calculate SA with FAST algorithm
  rr[j,] <- round(sensitivity(x=out[,1], numberf=ncol(param), cukier=F),3)
  rv[j,] <- round(sensitivity(x=out[,2], numberf=ncol(param), cukier=F),3)

} # End loop

# Store results into shapefile attribute table
Shp@data$RR_X1  <- rr[,1]
Shp@data$RR_X2  <- rr[,2]
Shp@data$RV_X1  <- rv[,1]
Shp@data$RV_X2  <- rv[,2]

# Save shapefile
writeOGR(Shp, dsn=file.path('2_SHP', Basin), layer='SA', driver="ESRI Shapefile", verbose=F)

# End script
cat('\f')
toc()
