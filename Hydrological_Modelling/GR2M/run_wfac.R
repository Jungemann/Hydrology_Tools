#######################################################################################################
# Title     : run_wfac.R
# Purpose   : Ejecuta el algoritmo de acumulación ponderada del flujo
# Author    : Harold Llauca
#######################################################################################################


  run_wfac <- function(Raster.Path,
                       Area.Path,
                       Mask.Path){

    
  # Raster.Path : Directorio del raster (dem o fdr) usado del area de estudio
  # Area.Path   : Directorio del shapefile del area de estudio
  # Mask.Path   : Directorio del raster de máscara usado para generar el raster weight

  

  # Importar raster de entrada a GRASS
  execGRASS("r.in.gdal", flags=c('o','overwrite'), Sys_show.output.on.console=F,
            parameters=list(input=Raster.Path, output="fdr"))

  # Configurar la extensión del área de estudio
  execGRASS("g.region", Sys_show.output.on.console=F, parameters=list(raster="fdr"))


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
            parameters=list(expression="qweight=qmask*qsim"))
  

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


  # Q de salida
  qsub <- c()
  for(j in 1:nsub){
    qsub[j] <- maxValue(setMinMax(mask(qacum, basin[j,])))
  }
  
  
  # Salidas
  Results  <- list(Qsub=qsub)
  return(Results)
}