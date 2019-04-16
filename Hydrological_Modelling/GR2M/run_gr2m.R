#############################################################################################################
# Title     : run_gr2m.R
# Purpose   : Ejecuta el modelo GR2M para cada paso de tiempo 'i'
# Author    : Harold Llauca
#############################################################################################################

# run_gr2m(Model.Input, Model.Parameter, Model.State, Model.Run)
# Model.Input     : Datos de entrada (Tiempo, P, E)
# Model.Parameter : Vector de parámetros (X1 y X2) del modelo GR2M
# Model.State     : Lista con variables de estado del modelo
# Model.Run       : Fecha de la simulación (mm/aaaa)


run_gr2m <- function (Model.Input,
                      Model.Parameter,
                      Model.State,
                      Model.Run){

  # Cargar librerias
  if("airGR" %in% rownames(installed.packages()) == FALSE){
    install.packages("airGR")
  }
  library(airGR)
  
  
  # Preparar inputs del modelo
  InputsModel   <- CreateInputsModel(FUN_MOD=RunModel_GR2M,
                                     DatesR=Model.Input$DatesR,
                                     Precip=Model.Input$P,
                                     PotEvap=Model.Input$E)

  
  # Exttraer datos para fecha seleccionada
  Ind_Run       <- which(format(Model.Input$DatesR, format="%m/%Y") == Model.Run)
  
  
  # Ejecutar modelos según condiciones iniciales
  if(is.null(Model.State)==T){
    
    # Parámetros de configuración para la corrida del modelo
    RunOptions    <- CreateRunOptions(FUN_MOD=RunModel_GR2M,
                                      InputsModel=InputsModel,
                                      IndPeriod_Run=Ind_Run)
    
    # Ejecutar modelo
    OutputsModel  <- RunModel(InputsModel=InputsModel,
                              RunOptions=RunOptions,
                              Param=Model.Parameter,
                              FUN=RunModel_GR2M)
  } else{
    
    # Parámetros de configuración para la corrida del modelo
    RunOptions    <- CreateRunOptions(FUN_MOD=RunModel_GR2M,
                                      InputsModel=InputsModel,
                                      IniStates=Model.State,
                                      IndPeriod_Run=Ind_Run)
    
    # Ejecutar modelo
    OutputsModel  <- RunModel(InputsModel=InputsModel,
                              RunOptions=RunOptions,
                              Param=Model.Parameter,
                              FUN=RunModel_GR2M)
    }

  
  # Extraer salidas
  Outputs       <- OutputsModel
  return(Outputs)
}