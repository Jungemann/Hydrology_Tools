####################################################################################################
# Title     : Run_GR2M_SemiDistr.R
# Purpose   : Ejecuta el modelo GR2M Semidistribuido
# Author    : Harold Llauca
####################################################################################################

# Run_GR2M_SemiDistr(Parameters, HRU, WorkDir, Raster, Shapefile, Input, RunIni, RunEnd,
#                    WarmIni, WarmEnd, FlowDir, IniState, WHC)
# Parameters  : Parámetros del modelo GR2M (X1, X2)
# HRU         : Zonas a las que pertenece cada subcuenca
# WorkDir     : Directorio de trabajo donde se almacenada la data, raster y shp
# Raster      : Nombre del archivo raster (FlowDirection o DEM)
# Shapefile   : Nombre del archivo shapefile de la cuenca (subcuencas)
# Input       : Datos de entrada del modelo en .csv (Tiempo, P, E, Qmm). Si existen varias
#               estaciones, se ingresan datos como tiempo,P1,P2,...,E1,E2,...,Qmm
# RunIni      : Fecha de inicio de la simulación (mm/aaaa)
# RunEnd      : Fecha de fin de la simulación (mm/aaaa)
# WarmIni     : Fecha de inicio del periodo de warm-up (mm/aaaa)
# WarmEnd     : Fecha de fin del periodo de warm-up (mm/aaaa)
# FlowDir     : [Lógico] Verdadero si el raster ingresado es Flow Direction (T por defecto).
# IniState    : Condiciones iniciales del modelo (NULL por defecto)


Run_GR2M_SemiDistr <- function(Parameters,
                               HRU,
                               WorkDir,
                               Raster,
                               Shapefile,
                               Input,
                               RunIni,
                               RunEnd,
                               WarmIni,
                               WarmEnd,
                               FlowDir=T,
                               IniState=NULL){


  # Cargar librerias
    if("rgdal" %in% rownames(installed.packages()) == FALSE){
      install.packages("rgdal")
    }
    library(rgdal)
    
    if("raster" %in% rownames(installed.packages()) == FALSE){
      install.packages("raster")
    }
    library(raster)
    
    if("rtop" %in% rownames(installed.packages()) == FALSE){
      install.packages("rtop")
    }
    library(rtop)
    
    if("hydroGOF" %in% rownames(installed.packages()) == FALSE){
      install.packages("hydroGOF")
    }
    library(hydroGOF)
    
    if("foreach" %in% rownames(installed.packages()) == FALSE){
      install.packages("foreach")
    }
    library(foreach)

    if("tictoc" %in% rownames(installed.packages()) == FALSE){
      install.packages("tictoc")
    }
    library(tictoc)
    tic()
  

  # Cargar funciones requeridas (contenidas en la carpeta de trabajo)
    source(file.path(WorkDir,'1_FUNCIONES','create_qmask.R'))
    source(file.path(WorkDir,'1_FUNCIONES','run_gr2m.R'))
    source(file.path(WorkDir,'1_FUNCIONES','run_wfac.R'))
  
  
  ## Leer topología del modelo
    # Rutas de acceso a archivos
    path.shp   <- file.path(WorkDir,'2_SHP',Shapefile)
    path.rast  <- file.path(WorkDir,'3_RASTER',Raster)
    path.mask  <- file.path(WorkDir,'3_RASTER','Qmask.tif')
    
    
    # Importar archivos raster y shapefile
    area       <- readOGR(path.shp, verbose=F)
    nsub       <- nrow(area@data)
    rast       <- raster(path.rast)

    
  ## Forzantes del modelo
    # Importar datos de entrada (forzantes) del modelo
    Data        <- read.table(file.path(WorkDir, '4_INPUT',Input), sep=',', header=T)
    Data$DatesR <- as.POSIXct(Data$DatesR, "GMT", tryFormats=c("%Y-%m-%d", "%d/%m/%Y"))
    
    
    # Extraer datos para el periodo seleccionado
    Subset      <- seq(which(format(Data$DatesR, format="%m/%Y") == RunIni),
                       which(format(Data$DatesR, format="%m/%Y") == RunEnd))
    Database    <- Data[Subset,]
    time        <- length(Subset)         

  
  ## Ejecutar el modelo GR2M de forma semidistribuida
    # Variables de almacenamiento
    q.model    <- matrix(NA, nrow=time , ncol=nsub)
    q.routing  <- matrix(NA, nrow=time , ncol=nsub)
    Param.Sub  <- list()
    out.model  <- list()
    States     <- list()
    EndState   <- list()
  
    
    # Parámetros del modelo
    Zone  <- sort(unique(HRU))
    Param <- data.frame(Zona=Zone,
                          X1=Parameters[1:length(Zone)],
                          X2=Parameters[(length(Zone)+1):length(Parameters)])
  

    # Crear máscara de Q
    qmask <-create_qmask(path.rast, path.shp, FlowDir=FlowDir)
    writeRaster(qmask, filename=path.mask, overwrite=T)
    
    
    # Estimar Qmensual para cada paso de tiempo 'i'
    for (i in 1:time){
      
      # En condiciones inciales
      if (i==1){
        
        Date   <- format(Database$DatesR[i], "%m/%Y")
        
        foreach (j=1:nsub) %do% {
  
          Param.Sub[[j]]  <- c(subset(Param$X1, Param$Zona==HRU[j]),
                               subset(Param$X2, Param$Zona==HRU[j]))
          
        # # Version de Niel et al. (2003)
        #   out.model[[j]]  <- run_gr2m_niel(Model.Input=Database[,c(1,j+1,j+1+nsub)],
        #                                    Model.Parameters=Param.Sub[[j]],
        #                                    Model.State=IniState,
        #                                    Model.Run=Date,
        #                                    Model.WHC=WHC[j])
        
        # Version de airGR
        out.model[[j]]  <- run_gr2m(Database[,c(1,j+1,j+1+nsub)],
                                    Param.Sub[[j]],
                                    IniState[[j]],
                                    Date)
          q.model[i,j]  <- round(out.model[[j]]$Qsim,3)
        }
        
      # Otros time step
      } else{
        
        Date   <- format(Database$DatesR[i], "%m/%Y")
  
        foreach (j=1:nsub) %do% {
  
          States[[j]]     <- out.model[[j]]$StateEnd
          Param.Sub[[j]]  <- c(subset(Param$X1, Param$Zona==HRU[j]),
                               subset(Param$X2, Param$Zona==HRU[j]))
          
          # # Version de Niel et al. (2003)
          # out.model[[j]]  <- run_gr2m_niel(Model.Input=Database[,c(1,j+1,j+1+nsub)],
          #                                  Model.Parameters=Param.Sub[[j]],
          #                                  Model.State=States[[j]],
          #                                  Model.Run=Date,
          #                                  Model.WHC=WHC[j])
          
          # Version de airGR
          out.model[[j]]  <- run_gr2m(Database[,c(1,j+1,j+1+nsub)],
                                      Param.Sub[[j]],
                                      States[[j]],
                                      Date)
          q.model[i,j]    <- round(out.model[[j]]$Qsim,3)
          
          # Estado final del sistema
          EndState[[j]]   <- out.model[[j]]$StateEnd
        }
      }
  
      # Sobrescribir shapefile de la cuenca (contenida en la carpeta de trabajo)
      area@data$Q  <- q.model[i,]
      writeOGR(area, dsn=path.shp, layer='Qsim', driver="ESRI Shapefile", verbose=F)
      
      
      # Rutear hidrogramas hasta la salida de la cuenca
      out.routing   <- run_wfac(path.rast, path.shp, path.mask, FlowDir=FlowDir)
      q.routing[i,] <- round(out.routing$Qsub,3)
  
  
      # Mostrar mensaje 
      cat('\f')
      print(paste('Procesando...: ', format(Database$DatesR[i], "%b-%Y"), sep=''))
    }
    
    # Extraer datos para el periodo seleccionado
    Subset2     <- seq(which(format(Database$DatesR, format="%m/%Y") == WarmIni),
                       which(format(Database$DatesR, format="%m/%Y") == WarmEnd))
    Database2   <- Database[-1*Subset2,]
    time2       <- length(Subset2)
    
  
  ## Salidas del modelo
  Results <- list(Qsim=q.routing[-1*Subset2,],
                  Qobs=Database2$Qmm,
                  Pr=Database2[,c(2:(nsub+1))],
                  Pet=Database2[,c((nsub+2):(2*nsub+1))],
                  Dates=Database2$DatesR,
                  State=EndState)

  message('FIN DEL PROCESO')
  toc()

  return(Results)
}
