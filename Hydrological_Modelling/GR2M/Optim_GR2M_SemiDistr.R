#######################################################################################################
# Title     : Optim_GR2M_SemiDistr.R
# Purpose   : Optimiza los parámetros X1 y X2 del modelo GR2M Semidistribuido con SCE-UA
# Author    : Harold Llauca
#######################################################################################################


Optim_GR2M_SemiDistr <- function(Parameters,
                                 Parameters.Min,
                                 Parameters.Max,
                                 Max.Optimization,
                                 Optimization='NSE',
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
                                 No.Optim=NULL){


  # Parameters    : Parámetros del modelo GR2M (X1, X2)
  # Parameters.Min: Umbral inferior de los parámetros X1 y X2 para cada Zona.
  # Parameters.Max: Umbral superior de los parámetros X1 y X2 para cada Zona.
  # Max.Iteration : Máximo número de iteraciones para evaluar la F.O.
  # Optimization  : Estadístico para la evalaución del desempeño (NSE por defecto).
  # HRU           : Región a las que pertenece cada subcuenca.
  # WorkDir       : Directorio de trabajo donde se almacenada la data, raster y shp
  # Raster        : Nombre del archivo raster de dirección de flujo.
  # Shapefile     : Nombre del archivo shapefile de la cuenca (subcuencas)
  # Input         : Datos de entrada del modelo en .csv (Tiempo, P, E, Qmm). Si existen varias
  #                 estaciones, se ingresan los datos como  [tiempo,P1,P2,...,E1,E2,...,Qmm]
  # RunIni        : Fecha de inicio de la simulación (mm/aaaa)
  # RunEnd        : Fecha de fin de la simulación (mm/aaaa)
  # WarmIni       : Fecha de inicio del periodo de warm-up (mm/aaaa)
  # WarmEnd       : Fecha de fin del periodo de warm-up (mm/aaaa)
  # IdBasin       : Valor numérico (ID) de la subcuenca utilizada como punto de control de calibración.
  # Remove        : Valor lógico para remover Qgenerado en la subcuenca de control (FALSE por defecto).
  # No.Optim      : Regiones que no se optimizarán (NULL por defecto).
  
  # Cargar librerias utilizadas
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
  
  
  # Cargar funciones requeridas (contenidas en la carpeta de trabajo)
  source(file.path(WorkDir,'1_FUNCIONES','Run_GR2M_SemiDistr.R'))
  

  # Variables auxiliares
  idx <- 1:length(Parameters)
  idy <- which(No.Optim==rep(HRU,2))
  Stb <- Parameters[idy]
  
  # Función objetivo =======
    OFUN <- function(Variable, HRU2, WorkDir2, Raster2, Shapefile2, Input2,
                     RunIni2, RunEnd2, WarmIni2, WarmEnd2, IdBasin2,
                     Remove2, Eval, No.Optim2, idx2, idy2, Stb2){
  
    
      # Seleccionar parámetros de subcuencas a optimizar
      if (is.null(No.Optim2)==TRUE){
        Optimization <- Variable
      } else{
        dta <- rbind(cbind(idx2[-idy2], Variable), cbind(idy2, Stb2))
        Optimization <- dta[match(sort(dta[,1]), dta[,1]), 2]
      }

      
          # Modelo GR2M semidistribuido
          x  <- Run_GR2M_SemiDistr(Optimization,
                                   HRU2,
                                   WorkDir2,
                                   Raster2,
                                   Shapefile2,
                                   Input2,
                                   RunIni2,
                                   RunEnd2,
                                   WarmIni2,
                                   WarmEnd2,
                                   IdBasin2,
                                   Remove2)
								                      
          
          # Datos de salida del modelo
          if (Remove2==FALSE){
            Qsim <- x$Qsim[,IdBasin2]
          } else{
            Qsim <- x$Qsim[,IdBasin2]-x$Qsub[,IdBasin2]
          }
          Qobs <- x$Qobs
          
          
          # Criterios de desempeño del modelo
          if (Eval == 'KGE'){
            return(1 - round(KGE(Qsim, Qobs),3))
          }
        
          if (Eval == 'NSE'){
            return(1 - round(NSE(Qsim, Qobs),3))
          }
          
          if (Eval == 'lnNSE'){
            return(1 - round(NSE(ln(Qsim), ln(Qobs)),3))
          }
          
          if (Eval == 'RMSE'){
            return(round(rmse(Qsim, Qobs),3))
          }
          
          if (Eval == 'R'){
            return(1 - round(rPearson(Qsim, Qobs),3))
          }    
  }
  

  # Definir HRUs y parámetros a optimizar
  if (is.null(No.Optim)==TRUE){
    Zone       <- sort(unique(HRU))
  } else{
    Parameters <- Parameters[!(rep(HRU,2) %in% No.Optim)]
    Zone       <- sort(unique(HRU[!(HRU %in% No.Optim)]))
  }
  Parameters.Min <- rep(Parameters.Min, each=length(Zone))
  Parameters.Max <- rep(Parameters.Max, each=length(Zone))
  
  # Optimización con SCE-UA ========
  y <- sceua(OFUN,
             pars=Parameters,
             lower=Parameters.Min,
             upper=Parameters.Max,
             maxn=Max.Optimization,
             Eval=Optimization,
             HRU2=HRU,
             WorkDir2=WorkDir,
             Raster2=Raster,
             Shapefile2=Shapefile,
             Input2=Input,
             RunIni2=RunIni,
             RunEnd2=RunEnd,
             WarmIni2=WarmIni,
             WarmEnd2=WarmEnd,
             IdBasin2=IdBasin,
             Remove2=Remove,
             No.Optim2=No.Optim,
             idx2=idx,
             idy2=idy,
             Stb2=Stb)
  
  # Mostrar mensaje
  message('Optimizando...')
  
  
  # Resultados
  return(y)
}