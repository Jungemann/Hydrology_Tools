# Este script genera el raster de máscara (centroides de cada subcuenca) para realizar el Weighted 
# Flow Accumulation y Genera el raster de Flow Direction usando GRASS 7.4.4
# Elaborado por Harold Llauca

# Remover variables anteriores y limpiar consola
rm(list=ls())
cat('\f')



# Datos de entrada
wd  <- 'D:/GR2M_PERU/GR2M_SemiDistr_Titicaca/Azangaro'
Dem <- 'Dem_Azangaro.tif'
Shp <- 'Azangaro.shp'


###################################################################################################
########################################## NO MODIFICAR ###########################################
###################################################################################################

# Cargar librerías
library(rgrass7)
library(rgdal)
library(raster)


# Directorio DEM y Shp
Shp.Path <- file.path(wd, '2_SHP', Shp)
Dem.Path <- file.path(wd, '3_RASTER', Dem)


## Generar raster máscara (centroides de subcuencas)
#===================================================

# Cargar shapefile
area  <- readOGR(Shp.Path)
nsub  <- nrow(area@data)

# Cargar raster
num   <- raster(Dem.Path)
qmask <- num
values(num)   <- 1:ncell(num)
values(qmask) <- 0
Mask  <- stack(qmask)

# Generar rasters centroides
for (j in 1:nsub){
  xy  <- coordinates(gCentroid(area[j,]))
  val <- extract(num, xy, method='simple')
  qmask[num==val] <- 1
  Mask <- addLayer(Mask, qmask)
  values(qmask) <- 0
}
QMask <- stackApply(Mask, rep(1,nsub+1), fun=sum)
QMask <- mask(QMask, area)

# Guardar raster centroides (máscara)
writeRaster(QMask, filename=file.path(wd, '3_RASTER', 'Qmask.tif'), overwrite=T)



## Generar raster flow direction
#===============================

# Iniciar GRASS
loc <- initGRASS('C:/Program Files/GRASS GIS 7.4.4',
                 home=getwd(),
                 gisDbase="GRASS_TEMP",
                 override=TRUE)

# Importar dem
execGRASS("r.in.gdal", flags=c('o','overwrite'), parameters=list(input=Dem.Path, output="dem"))


# Configurar la extensión del área de estudio
execGRASS("g.region", parameters=list(raster="dem"))


# Generar dirección de flujo y stream
execGRASS("r.watershed", flags=c("overwrite", "s", "a"), parameters=list(elevation="dem", drainage='fdr'))


# Guardar raster de dirección de flujo
execGRASS("r.out.gdal", flags='overwrite',
          parameters=list(input='fdr', output=file.path(wd,'3_RASTER','FlowDirection.tif'), format='GTiff'))


# Limpiar espacio de trabajo de GRASS
unlink(file.path(wd,'3_RASTER', "GRASS_TEMP"), recursive=T)
