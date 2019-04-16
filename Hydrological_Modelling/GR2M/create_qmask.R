#######################################################################################################
# Title     : create_qmask.R
# Purpose   : Crea un raster de máscara para el weighted flow accumulation
# Author    : Harold Llauca
#######################################################################################################

# create_qmask(Raster.Path, Area.Path, FlowDir=T)
# Raster.Path : Directorio del raster (dem o fdr) usado del area de estudio
# Area.Path   : Directorio del shapefile del area de estudio
# FlowDir     : Condicional (T) si Raster.Path corresponde a flow direction (T por defecto)


create_qmask <- function(Raster.Path,
                         Area.Path,
                         FlowDir=T){

  # Cargar librerias
  if("rgrass7" %in% rownames(installed.packages()) == FALSE){
    install.packages("rgrass7")
  }
  library(rgrass7)
  
  if("rgdal" %in% rownames(installed.packages()) == FALSE){
    install.packages("rgdal")
  }
  library(rgdal)
  
  if("raster" %in% rownames(installed.packages()) == FALSE){
    install.packages("raster")
  }
  library(raster)

  
  # Iniciar GRASS
  loc <- initGRASS('C:/Program Files/GRASS GIS 7.4.4',
                   home=getwd(),
                   gisDbase="GRASS_TEMP",
                   override=TRUE)
  

  # Importar raster de entrada a GRASS =======================================================
  # Si el raster ingresado es de dirección de flujo
  if (FlowDir==T){

    # Importar raster
    execGRASS("r.in.gdal", flags=c('o','overwrite'), Sys_show.output.on.console=F,
              parameters=list(input=Raster.Path, output="fdr"))

    # Configurar la extensión del área de estudio
    execGRASS("g.region", Sys_show.output.on.console=F, parameters=list(raster="fdr"))

  # Si el raster ingresado es DEM
  } else{

    # Importar raster
    execGRASS("r.in.gdal", flags=c('o','overwrite'), Sys_show.output.on.console=F,
              parameters=list(input=Raster.Path, output="dem"))


    # Configurar la extensión del área de estudio
    execGRASS("g.region", Sys_show.output.on.console=F, parameters=list(raster="dem"))


    # Generar dirección de flujo
    execGRASS("r.watershed", flags=c("overwrite", "s", "a"), Sys_show.output.on.console=F,
               parameters=list(elevation="dem",
                               drainage='fdr'))
  }

  # Acumulación ponderada del flujo
  execGRASS("r.accumulate", flags=c("overwrite"), Sys_show.output.on.console=F,
            parameters=list(direction="fdr",
                            accumulation="facum"))
  

  # Cargar dem y shp
  basin <- readOGR(Area.Path, verbose=F)
  nsub  <- nrow(basin@data)

  
  # Leer salida de GRASS en R
  facum      <- raster(readRAST('facum'))
  cat('\f')
  crs(facum) <- crs(basin)
  facum      <- mask(facum, basin)

  
  # Generar raster mask
  qsub <- c()
  for(j in 1:nsub){
    qsub[j] <- maxValue(setMinMax(mask(facum, basin[j,])))
  }
  qmask <- facum
  values(qmask) <- 0

  for (j in 1:nsub){
    qmask[facum==qsub[j]] <- 1
  }
  qmask <- mask(qmask, basin)


  # Clean GRASS workspace
  unlink(file.path(getwd(), "GRASS_TEMP"), recursive=T)

  
  # Salida
  return(qmask)
}