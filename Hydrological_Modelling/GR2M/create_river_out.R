#######################################################################################################
# Title     : create_river_out.R
# Purpose   : Crea un shapefile con los valores de Qsim en la tabla de atributos
# Author    : Harold Llauca
#######################################################################################################

# create_river_out(WorkDir, Basin, Stream, Qsim, Date)
# WorkDir : Directorio de trabajo
# Basin   : Shapefile de subcuencas
# Stream  : Raster de rios (generado a partir del flow direction)
# Qsim    : Caudales simulados en cada subcuenca
# Date    : Día al que corresponde el Qsim


create_river_out <- function(WorkDir,
                             Basin,
                             Stream,
                             Qsim,
                             Date){
  
  # Cargar librerias
  if("rgrass7" %in% rownames(installed.packages()) == FALSE){
    install.packages("rgrass7")
  }
  library(rgrass7)
  
  # Cargar librerias
  if("raster" %in% rownames(installed.packages()) == FALSE){
    install.packages("raster")
  }
  library(raster)
  
  # Cargar librerias
  if("rgdal" %in% rownames(installed.packages()) == FALSE){
    install.packages("rgdal")
  }
  library(rgdal)

    # Leer data de Qsim para la fecha deseada
	  Qsim$Fecha <- as.POSIXct(Qsim$Fecha, "GMT", tryFormats=c("%Y-%m-%d", "%d/%m/%Y"))
    IdRun      <- which(format(Qsim$Fecha, format="%m/%Y") == Date)
  	Qsim       <- as.numeric(Qsim[IdRun,-1])
	
	
    # Leer shapefiles y rasters
    Basin      <- readOGR(file.path(WorkDir,'2_SHP',Basin))
    Stream     <- raster(file.path(WorkDir,'3_RASTER',Stream))
    
    # Crear variables auxiliares
    Stream.Sub <- list()
    nsub       <- nrow(Basin@data)
    
    # Asignar Qsim a cada tramo de río 
    for (i in 1:nsub){
      r   <- mask(crop(Stream, Basin[i,]), Basin[i,])
      r[!is.na(r)]    <- Qsim[i]  
      Stream.Sub[[i]] <- r
    }
    
    # Merge rasters
    for (i in 1:(nsub-1)){
      if (i == 1){
        ras <- merge(Stream.Sub[[i]], Stream.Sub[[i+1]])
      }
      ras <- merge(ras, Stream.Sub[[i+1]])
    }
    

    # Iniciar GRASS
    loc <- initGRASS('C:/Program Files/GRASS GIS 7.4.4',
                     home=getwd(),
                     gisDbase="GRASS_TEMP",
                     override=TRUE)
    
    # Importar raster
    writeRAST(as(ras, "SpatialGridDataFrame"), 'Rios')
    
    
    # Configurar la extensión del área de estudio
    execGRASS("g.region", parameters=list(raster="Rios"))
    
    
    # Vectorizar raster de segmentos de río
    execGRASS('r.to.vect', flags=c('s','overwrite'), Sys_show.output.on.console=F,
              parameters=list(input='Rios',
                              output='rios',
                              type='line',
                              column='Qsim'))
    
    # Extraer ríos
    Rios      <- readVECT("Rios")
    cat('\f')
    crs(Rios) <- crs(Basin)
    
    # Clean GRASS workspace
    unlink(file.path(getwd(), "GRASS_TEMP"), recursive=T)
    
	# Salida
    return(Rios)
}