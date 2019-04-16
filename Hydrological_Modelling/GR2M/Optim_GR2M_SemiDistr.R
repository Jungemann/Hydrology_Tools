#######################################################################################################
# Title     : Optim_GR2M_SemiDistr.R
# Purpose   : Optimiza los parámetros del modelo GR2M Semidistribuido con SCE-UA
# Author    : Harold Llauca
#######################################################################################################

# Optim_GR2M_SemiDistr(Parameters, HRU, Parameters.Min, Parameters.Max, Max.Iteration,
#                      WorkDir, Raster, Shapefile, Input, RunIni, RunEnd, WarmIni,
#                      WarmEnd, Optimization='NSE', IniState, WHC, FlowDir=T, IdBasin)
# Parameters    : Parámetros del modelo GR2M. Dataframe [Zona, X1, X2].
# HRU           : Zonas a las que pertenece cada subcuenca.
# Parameters.Min: Umbral inferior de los parámetros X1 y X2 para cada Zona.
# Parameters.Max: Umbral superior de los parámetros X1 y X2 para cada Zona.
# Max.Iteration : Máximo número de iteraciones para evaluar la F.O.
# WorkDir       : Directorio de trabajo donde se almacenada la data, raster y shp
# Raster        : Nombre del archivo raster (FlowDirection o DEM)
# Shapefile     : Nombre del archivo shapefile de la cuenca (subcuencas)
# Input         : Datos de entrada del modelo en .csv (Tiempo, P, E, Qmm). Si existen varias
#                 estaciones, se ingresan los datos como  [tiempo,P1,P2,...,E1,E2,...,Qmm]
# RunIni        : Fecha de inicio de la simulación (mm/aaaa)
# RunEnd        : Fecha de fin de la simulación (mm/aaaa)
# WarmIni       : Fecha de inicio del periodo de warm-up (mm/aaaa)
# WarmEnd       : Fecha de fin del periodo de warm-up (mm/aaaa)
# Optimization  : Estadístico para la evalaución del desempeño (NSE por defecto).
# FlowDir       : [Lógico] Verdadero si el raster ingresado es Flow Direction (T por defecto).
#                 cuando es un DEM. T por defecto.
# IdBasin       : ID (numérico) de la salida de la subucenca utilizada como punto de control


Optim_GR2M_SemiDistr <- function(Parameters,
                                 HRU,
                                 Parameters.Min,
                                 Parameters.Max,
                                 Max.Iteration,
                                 Optimization='NSE',
                                 WorkDir,
                                 Raster,
                                 Shapefile,
                                 Input,
                                 RunIni,
                                 RunEnd,
                                 WarmIni,
                                 WarmEnd,
                                 FlowDir=T,
                                 IdBasin){


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
  
  
  # Cargar funciones requeridas (contenidas en la carpeta de trabajo)
  source(file.path(WorkDir,'1_FUNCIONES','Run_GR2M_SemiDistr.R'))
  
  
  # Función objetivo =======
  OFUN <- function(Parameters2, HRU2, WorkDir2, Raster2, Shapefile2, Input2,
                   RunIni2, RunEnd2, WarmIni2, WarmEnd2, FlowDir2, Eval, ID){
  
    
          # Modelo GR2M semidistribuido (topologia basada en el DEM de entrada)
          x  <- Run_GR2M_SemiDistr(Parameters2,
                                   HRU2,
                                   WorkDir2,
                                   Raster2,
                                   Shapefile2,
                                   Input2,
                                   RunIni2,
                                   RunEnd2,
                                   WarmIni2,
                                   WarmEnd2,
                                   FlowDir2)
								                      
          
          # Datos de salida del modelo
          Qsim <- x$Qsim[,ID]
          Qobs <- x$Qobs
          
          
          # Criterios de desempeño
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
  

  # Optimización con SCE-UA ========
  Zone           <- sort(unique(HRU))
  Parameters.Min <- rep(Parameters.Min, each=length(Zone))
  Parameters.Max <- rep(Parameters.Max, each=length(Zone))
  
  y <- sceua(OFUN,
             pars=Parameters,
             lower=Parameters.Min,
             upper=Parameters.Max,
             maxn=Max.Iteration,
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
             FlowDir2=FlowDir,
             ID=IdBasin)
  
  message('Optimizando...')
  
  return(y)
}