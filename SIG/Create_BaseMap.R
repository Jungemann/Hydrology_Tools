# This script is to create a map using R
# By Harold Llauca
rm(list=ls())
cat('\f')


# Load libraries
library(raster)
library(rgdal)
library(globe)
library(RColorBrewer)
library(fields)


# Work directory
Location <- 'D:/MODELO_DIARIO'
setwd(Location)


# Load require functions
source(file.path(Location,'SCRIPTS/all_coordinates.R'))
source(file.path(Location,'SCRIPTS/add_alpha.R'))


# Load data
MetaData <- read.table(file.path('DATA_SIG/TABLES','XY_PACIF_GEO.csv'), header=T, sep=',')


# Load shapefiles
Basin  <- readOGR(file.path('DATA_SIG/SHP/GEO/UH_MODEL_PACIF_GEO.shp'))
Hydro  <- readOGR(file.path('DATA_SIG/SHP/GEO/HIDRO_MODEL_PACIF_GEO.shp'))
Macro  <- readOGR(file.path('DATA_SIG/SHP/GEO/BASE/MACROCUENCAS_GEO.shp'))
South  <- readOGR(file.path('DATA_SIG/SHP/GEO/BASE/SURAMERICA_GEO.shp'))
Pacif  <- readOGR(file.path('DATA_SIG/SHP/GEO/MACRO_PACIFICO_GEO.shp'))
Idx    <- readOGR(file.path('DATA_SIG/SHP/GEO/ID_MODEL_PACIF_GEO.shp'))
Labels <- readOGR(file.path('DATA_SIG/SHP/GEO/LAB_MODEL_PACIF_GEO.shp'))
Box    <- readOGR(file.path('DATA_SIG/SHP/GEO/BOX_MAPA_GEO.shp'))


# Load rasters
Dem    <- raster(file.path('DATA_SIG/RASTER/GEO/Dem_250m_PERUMAP_GEO.tif'))
Mdt    <- raster(file.path('DATA_SIG/RASTER/GEO/Mdt_250m_PERUMAP_GEO.tif'))


# Auxiliary variables
Names   <- toupper(as.matrix(MetaData$Cuenca))
nBasins <- length(Names)
lat     <- MetaData$Lat_fix
lon     <- MetaData$Lon_fix
PacifXY <- all_coordinates(Pacif)
IdXY    <- coordinates(Idx)
LabXY   <- coordinates(Labels)



## Create figure
  tiff(file.path(Location,'FIGURAS','Map_models.tif'), units="px", res=200, height=1400, width=1300)
  
    # Graphical parameters
    par(mfrow=c(1,1))
    par(mar=c(3,3,0.5,6), oma=c(0,0,0,0))
    par(mgp=c(0,0.2,0))
    par(tck=0.02)
    par(fig=c(0,1,0,1))
    par(box=list(col="black", lwd=.5))
    
    
    # Other auxiliary variables
    LonRange <- as.vector(extent(Dem))[c(1,2)]
    LatRange <- as.vector(extent(Dem))[c(3,4)]
    DemRange <- c(minValue(setMinMax(Mdt)), maxValue(setMinMax(Dem)))
    MdtRange <- c(minValue(setMinMax(Mdt)), maxValue(setMinMax(Mdt)))
    DemBreaks<- seq(0, DemRange[2], 1/100)
    MdtBreaks<- seq(MdtRange[1],MdtRange[2],1)
    DemColor <- addalpha(colorRampPalette(rev(brewer.pal(11,'RdGy')))(length(DemBreaks)), 1)
    MdtColor <- addalpha(colorRampPalette(rev(brewer.pal(9,'Greys')))(length(MdtBreaks)), 0.5)
    LonAxis  <- seq(-81, -60, 4)
    LatAxis  <- seq(-21, 0, 4)
    DemLabels<- seq(0, DemRange[2], 1000)
    
    
    # Plot raster and shapes
    image(Dem, zlim=DemRange, xlim=LonRange, ylim=LatRange, col=DemColor, ann=F, xaxt='n', yaxt='n', asp=1)
    image(Mdt, zlim=MdtRange, xlim=LonRange, ylim=LatRange, col=MdtColor, ann=F, xaxt='n', yaxt='n', asp=1, add=T)
    plot(South, border='black', lwd=0.8, lty=1, add=T)
    plot(Macro, border='black', lwd=0.5, lty=2, add=T)
    plot(Pacif, border='darkred', lwd=1.5, add=T)
    plot(Basin, border='gray25',add=T, lwd=1)
    
    for (i in 1:nBasins){
      fname  <- paste0('HIGH_B',i,'_',Names[i],'_GEO.shp')
      basin  <- readOGR(file.path(Location, 'DATA_SIG/SHP/GEO/BASIN_PACIF', fname))
      plot(basin, col=rgb(0,0,1, alpha=0.2), lwd=1.5, add=T)
    }
    
    plot(Hydro, pch=19, col='darkblue', cex=0.8, add=T)
    abline(v=LonAxis, col="gray50", lty="dotted", lwd=.5)
    abline(h=LatAxis, col="gray50", lty="dotted", lwd=.5)
    axis(1, at=LonAxis, lab=paste(-1*LonAxis,'°O'), lwd=.5, cex.axis=0.7)
    axis(3, at=LonAxis, lab=F, lwd=.5, cex.axis=0.7)
    axis(2, at=LatAxis, lab=(paste(-1*LatAxis,'°S')), lwd=.5, cex.axis=0.7, las=2)
    axis(4, at=LatAxis, lab=F, lwd=.5, cex.axis=0.7, las=2)
    mtext('Longitud', side=1, line=1.8, cex=0.9)
    mtext('Latitud', side=2, line=1.8, cex=0.9)
    text(IdXY, paste0('B',1:17), cex=0.4, font=2)
    text(LabXY, as.vector(Labels@data$Etiqueta), cex=0.5, font=3)
  
    
    # Legend
    legend('topright', legend=c('Cuenca alta (modelo)','Cuenca piloto',
                                'Vertiente del Pacífico','Estación hidrométrica'),
           pch=c(22,22,22,19), pt.lwd=1.2, pt.bg=c(rgb(0,0,1,alpha=0.2), rep(NA,3)),
           col=c('black','gray25','darkred','darkblue'), cex=0.6, pt.cex=c(2,2,2,1),
           horiz=F, x.intersp=2, y.intersp=1.5, inset=c(0.02, 0.25))
  
    
    # Colorkey
    image.plot(legend.only=T, zlim=range(0,DemRange[2]), col=DemColor, horizontal=F, legend.shrink=1,
               smallplot=c(0.89, 0.92, 0.20, 0.90), legend.width=0.1,
               axis.args=list(cex.axis=0.7, at=DemLabels, tck=-0.3, lwd=.5, mgp=c(0,0.5,0)))
    mtext(side=1, 'Elevación', font=2, outer=T, line=-5.8, adj=0.96, cex=0.8)
    mtext(side=1, '[msnm]', font=2, outer=T, line=-4.8, adj=0.95, cex=0.8)
    
    
    # Globe map
    par(fig = c(0.1,0.4,0.1,0.4), new=T)
    par(mar=c(0.5,0.5,0.5,0.5), oma=c(0,0,0,0), new=T)
    
      globeearth(eye=list(-80,-10), col="black", lwd=1.2)
      globedrawlat(lat=seq(-90, 90, 30), col='gray70', lwd=1)
      globedrawlong(lon=seq(-180, 180, 30), col='gray70', lwd=1)
      globelines(PacifXY, col='darkred', lwd=2)
      mtext('Vertiente del Pacífico', side=1, line=-5, adj=0.05, col='darkred', cex=0.55)

      
  dev.off()
