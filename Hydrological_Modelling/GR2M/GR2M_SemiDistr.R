# Este script ejecuta el modelo GR2M de forma semidistribuida (subcuencas), utilizando los paquetes
# 'airGR' y 'rgrass7'. La topología de la cuenca se identifica automáticamente a partir del raster
# DEM o Flow Direction, utilizando el algoritmo de acumulación ponderada del flujo (Weighted Flow
# Accumulation). La calibración de los parámetros del modelo (X1 y X2) se realiza de forma automática
# mediante el algoritmo de optimización global Shuffle Complex Evolution (SCE-UA) usando el paquete
# 'rtop'. Se requiere instalar previamente GRASS v.7 (https://grass.osgeo.org/download/software/) y
# habilitar la extensión 'r.accumulate' en GRASS, utilizando el comando 'g.extension'.

# ©Harold LLauca
# Email: hllauca@gmail.com

# Remover variables almacenadas
rm(list=ls())
# Suprimir warnings
options(warn=-1)
# Limpiar consola
cat('\f')



## Ingresar datos de entrada para ejecutar el modelo GR2M SemiDistribuido
#====================================================================================================

  # Directorio de trabajo
  Location     <- 'D:/GR2M_PERU/GR2M_SemiDistr_Test'
  
  # Nombre del raster (.tif) de Flow Direction (TRUE) o DEM (FALSE),
  # y nombre del shapefile (.shp) de las subcuencas
  Is.Fdr       <- TRUE  
  File.Raster  <- 'FlowDir_450m.tif'
  File.Shape   <- 'Basin.shp'
  
  # Datos [Fecha, Precip, Evapotrans, Qobs_mm] en formato .csv
  File.Data    <- 'Inputs_Basins.csv'
  
  # Periodo de ejecución del modelo en formato 'mm/yyyy'
  RunModel.Ini <- '01/1981'
  RunModel.End <- '03/2019'
  
  # Periodo de warm-up del modelo en formato 'mm/yyyy'
  WarmUp.Ini   <- '01/1981'
  WarmUp.End   <- '12/1985'
  
  # Parámetros X1 y X2 del modelo, por HRUs
  Model.HRU    <- c('A','B','A')              # [SUB_1,SUB_2,SUB_3]
  Model.Param  <- c(200, 100, 0.2, 0.4)       # [X1_A,X1_B,X2_A,X2_B]

  # Raster de ríos para generar shapefile (ríos) con valores de
  # Qsimulados para la fecha de interés
  Date         <- '03/2019'
  File.River   <- 'Stream_450m.tif'
  
  # Definir si se realizará la optimización automática de parámetros (TRUE)
  # o no (FALSE)
  Optim        <- FALSE
  
  # Máx número de iteraciones para la optimización
  Optim.Max    <- 500
  
  # Criterio de desempeño a utilizar para la optimización (NSE, lnNSE, R, RMSE, KGE)
  Optim.Eval   <- 'NSE'
  Optim.Basin  <- 2                           # Ver tabla de atributos (e.g. SUB_2)
 
  # Valores min y max de los parámetros X1 y X2
  Model.ParMin <- c(1, 0.01)                 # [Min_X1, Min_X2]
  Model.ParMax <- c(2000, 2)                 # [Max_X1, Max_X2]

  
  
###################################################################################################
########################################## NO MODIFICAR ###########################################
###################################################################################################

# Condicional para la optimización
if (Optim == F){
  
# Optimizar parámetros del modelo GR2M semidistribuido con el algoritmo SCE-UA
#=================================================================================================
  
  # Cargar función
  source(file.path(Location,'1_FUNCIONES','Optim_GR2M_SemiDistr.R'))
  
  # Ejecutar modelo
  x <- Optim_GR2M_SemiDistr(Parameters=Model.Param,
                            HRU=Model.HRU,
                            Parameters.Min=Model.ParMin,
                            Parameters.Max=Model.ParMax,
                            Max.Iteration=Optim.Max,
                            Optimization=Optim.Eval,
                            WorkDir=Location,
                            Raster=File.Raster,
                            Shapefile=File.Shape,
                            Input=File.Data,
                            RunIni=RunModel.Ini,
                            RunEnd=RunModel.End,
                            WarmIni=WarmUp.Ini,
                            WarmEnd=WarmUp.End,
                            FlowDir=Is.Fdr,
                            IdBasin=Optim.Basin)

	# Extraer resultados
	Model.Param <- x$par
	print(x$value)
}
  
 
# Ejecutar modelo GR2M semidistribuido
#=================================================================================================
  
  # Verificar y cargar paquetes
  if("hydroGOF" %in% rownames(installed.packages()) == FALSE){
    install.packages("hydroGOF")
  }
  library(hydroGOF)
  
  
  # Cargar función
  source(file.path(Location,'1_FUNCIONES','Run_GR2M_SemiDistr.R'))

  # Correr modelo GR2M semidistribuido
  y  <- Run_GR2M_SemiDistr(Parameters=Model.Param,
                           HRU=Model.HRU,
                           WorkDir=Location,
                           Raster=File.Raster,
                           Shapefile=File.Shape,
                           Input=File.Data,
                           RunIni=RunModel.Ini,
                           RunEnd=RunModel.End,
                           WarmIni=WarmUp.Ini,
                           WarmEnd=WarmUp.End,
                           FlowDir=Is.Fdr)
   
  
  # Mostrar figura comparativa
  x11()
  ggof(y$Qsim[,Optim.Basin], y$Qobs)
  	
  
  # Crear carpeta de salida '5_OUTPUT'
  dir.create(file.path(Location, '5_OUTPUT'), recursive=T, showWarnings=F)
  
  
  # Guardar resultados en la carpeta de salida
  Qout <- data.frame(y$Dates, y$Qsim)
  colnames(Qout) <- c('Fecha', paste0('Qsim-',1:length(Model.HRU)))
  write.table(Qout, file=file.path(Location, '5_OUTPUT', 'Results_GR2M_Semidistr_Qsim.csv'),
              sep=',', row.names=F)
  
  
  # Generar ríos con atributos de Qsim para una fecha determinada
  # Cargar función
  source(file.path(Location,'1_FUNCIONES','create_river_out.R'))
  
  # Crear shapefile de ríos
  z <- create_river_out(WorkDir=Location,
                        Basin=File.Shape,
                        Stream=File.River,
                        Qsim=Qout,
                        Date=Date)
  
  # Crear carpeta 'SHP'
  dir.create(file.path(Location, '5_OUTPUT', 'SHP'), recursive=T, showWarnings=F)
  
  # Exportar archivos generados
  writeOGR(z, dsn=file.path(Location,'5_OUTPUT', 'SHP',
                            paste0('Rivers_GR2M_SemiDistr_',gsub("/",".",Date),'.shp')),
           layer='River', driver="ESRI Shapefile", overwrite_layer=T)
