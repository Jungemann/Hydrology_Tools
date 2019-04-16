run_gr2m_airGR <- function(Model.Input,
                           Model.Parameter,
                           Warm.Ini,
                           Warm.End,
                           Model.Ini,
                           Model.End){  
  
  
    library(airGR)
    colnames(Model.Input) <- c('DatesR', 'P', 'E')
  
  
    # Extraer datos para fecha seleccionada
      Ind_Warm     <- seq(which(format(Model.Input$DatesR, "%m/%Y") == Warm.Ini),
                          which(format(Model.Input$DatesR, "%m/%Y") == Warm.End))
      Ind_Run      <- seq(which(format(Model.Input$DatesR, "%m/%Y") == Model.Ini),
                          which(format(Model.Input$DatesR, "%m/%Y") == Model.End))
      Ind_Mod      <- Ind_Warm[1]:Ind_Run[length(Ind_Run)]
  
    # Preparar inputs del modelo
      InputsModel   <- CreateInputsModel(FUN_MOD=RunModel_GR2M,
                                         DatesR=Model.Input$DatesR[Ind_Mod],
                                         Precip=Model.Input$P[Ind_Mod],
                                         PotEvap=Model.Input$E[Ind_Mod])
      
    # Parámetros de configuración para la corrida del modelo
      RunOptions    <- CreateRunOptions(FUN_MOD=RunModel_GR2M,
                                        InputsModel=InputsModel,
                                        IndPeriod_WarmUp=Ind_Warm,
                                        IndPeriod_Run=Ind_Run)
      
    # Ejecutar modelo
      OutputsModel  <- RunModel(InputsModel=InputsModel,
                                RunOptions=RunOptions,
                                Param=Model.Parameter,
                                FUN=RunModel_GR2M)
      
    # Runoff Ratio
      RR <- sum(OutputsModel$Qsim)/sum(OutputsModel$Precip)
    
    # Runoff Variability  
      RV <- sd(OutputsModel$Qsim)/sd(OutputsModel$Precip)
    
    # Results
      ans <- c(RR,RV)
      return(ans)
}