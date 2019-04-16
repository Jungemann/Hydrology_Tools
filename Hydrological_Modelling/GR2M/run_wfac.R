#######################################################################################################
# Title     : run_wfac.R
# Purpose   : Ejecuta el algoritmo de acumulación ponderada del flujo
# Author    : Harold Llauca
#######################################################################################################

# run_wfac(Raster.Path, Area. Path, Mask.Path, Stream.Path, FlowDir)
# Raster.Path : Directorio del raster (dem o fdr) usado del area de estudio
# Area.Path   : Directorio del shapefile del area de estudio
# Mask.Path   : Directorio del raster de máscara usado para generar el raster weight
# FlowDir     : Condicional (T) si Raster.Path corresponde a flow direction (T por defecto)


run_wfac <- function(Raster.Path,
                     Area.Path,
                     Mask.Path,
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


  # Importar shapes de subcuencas
  execGRASS("v.in.ogr", flags=c('o','overwrite'), Sys_show.output.on.console=F,
            parameters=list(input=Area.Path, output="subbasin"))
  
  
  # Rasterizar Qsim para cada subcuenca
  execGRASS("v.to.rast", flags='overwrite', Sys_show.output.on.console=F,
            parameters=list(input="subbasin",
                            output="qsim",
                            use="attr",
                            attribute_column='Q'))
  
  # Importar máscara
  execGRASS("r.in.gdal", flags=c('o','overwrite'), Sys_show.output.on.console=F,
            parameters=list(input=Mask.Path, output="qmask"))
  

  # Calcular raster de pesos
  execGRASS("r.mapcalc", flags=c('overwrite'), Sys_show.output.on.console=F,
            parameters=list(expression="qweight = qmask*qsim"))
  

  # Acumulación ponderada del flujo
  execGRASS("r.accumulate", flags=c("overwrite"), Sys_show.output.on.console=F,
            parameters=list(direction="fdr",
                            weight='qweight',
                            accumulation="qacum"))
  

  # Cargar dem y shp
  basin <- readOGR(Area.Path, verbose=F)
  nsub  <- nrow(basin@data)
  rast  <- raster(Raster.Path)

  
  # Leer salida de GRASS en R
  qacum      <- raster(readRAST('qacum'))
  cat('\f')
  crs(qacum) <- crs(basin)
  qacum      <- mask(qacum, basin)

  
  # Clean GRASS workspace
  unlink(file.path(getwd(), "GRASS_TEMP"), recursive=T)
  

  # Q de salida
  qsub <- c()
  for(j in 1:nsub){
    qsub[j] <- maxValue(setMinMax(mask(qacum, basin[j,])))
  }
  
  # Salidas
  Results  <- list(Qsub=qsub)
  return(Results)
}