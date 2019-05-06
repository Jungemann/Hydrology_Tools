####################################################################################################
# Title     : Run_GR2M_SemiDistr.R
# Purpose   : Ejecuta el modelo GR2M Semidistribuido
# Author    : Harold Llauca
####################################################################################################


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
                               IdBasin,
                               Remove=FALSE,
                               Plot=FALSE,
                               IniState=NULL){

  
  # Parameters=Model.Param
  # HRU=Model.HRU
  # WorkDir=Location
  # Raster=File.Raster
  # Shapefile=File.Shape
  # Input=File.Data
  # RunIni=RunModel.Ini
  # RunEnd=RunModel.End
  # WarmIni=WarmUp.Ini
  # WarmEnd=WarmUp.End
  # IdBasin=Optim.Basin
  # Remove=Optim.Remove
  # Plot=TRUE
  # IniState=NULL
  
  
  # Parameters  : Parámetros del modelo GR2M (X1, X2)
  # HRU         : Región a las que pertenece cada subcuenca
  # WorkDir     : Directorio de trabajo donde se almacenada la data, raster y shp
  # Raster      : Nombre del archivo raster de dirección de flujo
  # Shapefile   : Nombre del archivo shapefile de la cuenca (subcuencas)
  # Input       : Datos de entrada del modelo en .csv (Tiempo, P, E, Qmm). Si existen varias
  #               estaciones, se ingresan datos como tiempo,P1,P2,...,E1,E2,...,Qmm
  # RunIni      : Fecha de inicio de la simulación (mm/aaaa)
  # RunEnd      : Fecha de fin de la simulación (mm/aaaa)
  # WarmIni     : Fecha de inicio del periodo de warm-up (mm/aaaa)
  # WarmEnd     : Fecha de fin del periodo de warm-up (mm/aaaa)
  # IdBasin     : Valor numérico (ID) de la subcuenca utilizada como punto de control de calibración.
  # Remove      : Valor lógico para remover Qgenerado en la subcuenca de control (FALSE por defecto).
  # Plot        : Valor lógico para graficas serie observada vs. simulada (FALSE por defecto).
  # IniState    : Condiciones iniciales del modelo (NULL por defecto)

  
  # Cargar librerias utilizadas
    if("rgdal" %in% rownames(installed.packages()) == FALSE){
      install.packages("rgdal")
    }
    library(rgdal)
    
    if("raster" %in% rownames(installed.packages()) == FALSE){
      install.packages("raster")
    }
    library(raster)
  
    if("rgeos" %in% rownames(installed.packages()) == FALSE){
      install.packages("rgeos")
    }
    library(rgeos)

    if("rgrass7" %in% rownames(installed.packages()) == FALSE){
      install.packages("rgrass7")
    }
    library(rgrass7)
    
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
    source(file.path(WorkDir,'1_FUNCIONES','run_gr2m.R'))
    source(file.path(WorkDir,'1_FUNCIONES','run_wfac.R'))
  
  
  ## Leer topología del modelo
    # Rutas de acceso a archivos
    path.shp   <- file.path(WorkDir,'2_SHP', Shapefile)
    path.rast  <- file.path(WorkDir,'3_RASTER', Raster)
    path.mask  <- file.path(WorkDir,'3_RASTER', 'Qmask.tif')
    
    
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


  ## Iniciar GRASS 7.4.4
     loc <- initGRASS('C:/Program Files/GRASS GIS 7.4.4',
                     home=getwd(),
                     gisDbase="GRASS_TEMP",
                     override=TRUE)
    
    
  ## Ejecutar el modelo GR2M de forma semidistribuida
    # Variables de almacenamiento
    tic()
    q.model    <- matrix(NA, nrow=time , ncol=nsub)
    q.routing  <- matrix(NA, nrow=time , ncol=nsub)
    Param.Sub  <- list()
    out.model  <- list()
    States     <- list()
    EndState   <- list()
    Qmodel     <- raster(path.mask)
    QMask      <- as.matrix(raster(path.mask))
    QRas       <- QMask
    
    
    # Parámetros del modelo
    Zone  <- sort(unique(HRU))
    Param <- data.frame(Zona=Zone,
                          X1=Parameters[1:length(Zone)],
                          X2=Parameters[(length(Zone)+1):length(Parameters)])

    
    # Estimar Q para mes 'i' y subcuenca 'j'
    for (i in 1:time){
      Date <- format(Database$DatesR[i], "%m/%Y")
    
        # Calcular el caudal en cada subcuenca 'j'
        foreach (j=1:nsub) %do% {
            Param.Sub[[j]] <- c(subset(Param$X1, Param$Zona==HRU[j]),
                                subset(Param$X2, Param$Zona==HRU[j]))
              if (i==1){
                out.model[[j]] <- run_gr2m(Database[,c(1,j+1,j+1+nsub)],
                                           Param.Sub[[j]],
                                           IniState[[j]],
                                           Date)
              }else{
                States[[j]]    <- out.model[[j]]$StateEnd
                out.model[[j]] <- run_gr2m(Database[,c(1,j+1,j+1+nsub)],
                                           Param.Sub[[j]],
                                           States[[j]],
                                           Date)
              }
            q.model[i,j]   <- round(out.model[[j]]$Qsim,3)
            QRas[QMask==j] <- q.model[i,j]
         }

      # Crear raster de caudales generados en cada subcuenca 'j'
      Qmodel[] <- as.vector(t(QRas))

      # Rutear hidrogramas hasta la salida de la cuenca
      q.routing[i,] <- round(run_wfac(rast, Qmodel, area, nsub),3)

      # Mostrar mensaje
      cat('\f')
      message(paste0('Ejecutando GR2M SemiDistr...: ', format(Database$DatesR[i], "%b-%Y")))
    }
    
    # Limpiar espacio de trabajo de GRASS
    unlink(file.path(getwd(), "GRASS_TEMP"), recursive=T)
    
    # Descontar valores correspondientes al periodo de calentamiento (warm-up)
    Subset2     <- seq(which(format(Database$DatesR, format="%m/%Y") == WarmIni),
                       which(format(Database$DatesR, format="%m/%Y") == WarmEnd))
    Database2   <- Database[-1*Subset2,]
    time2       <- length(Subset2)
    
    
  # Mostrar figura comparativa
    if (Plot==TRUE){
      x11()
      if (Remove==FALSE){
        Qsim <- q.routing[-1*Subset2,IdBasin]
        Qobs <- Database2$Qmm
        ggof(Qsim, Qobs)
      } else{
        Qsim <- q.routing[-1*Subset2,IdBasin]-q.model[-1*Subset2,IdBasin]
        Qobs <- Database2$Qmm
        ggof(Qsim, Qobs)
      }
    }

  
  ## Salidas del modelo
  Results <- list(Qsim=q.routing[-1*Subset2,],
                  Qsub=q.model[-1*Subset2,],
                  Qobs=Database2$Qmm,
                  Precip=Database2[,c(2:(nsub+1))],
                  Pet=Database2[,c((nsub+2):(2*nsub+1))],
                  Dates=Database2$DatesR,
                  State=EndState)
  
  toc()
  
  # Resultados
  return(Results)
}
