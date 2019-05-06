#######################################################################################################
# Title     : run_wfac.R
# Purpose   : Ejecuta el algoritmo de acumulación ponderada del flujo
# Author    : Harold Llauca
#######################################################################################################


  run_wfac <- function(FlowDir, Qmodel, Shapefile, nSub){

    
      # Importar raster de entrada a GRASS
      writeRAST(as(FlowDir, 'SpatialGridDataFrame'), "fdr", overwrite=T)
    
      
      # Configurar la extensión del área de estudio
      execGRASS("g.region", Sys_show.output.on.console=F, parameters=list(raster="fdr"))
    
  
      # Importar raster de caudales simulados
      writeRAST(as(Qmodel, 'SpatialGridDataFrame'), "qweight", overwrite=T)
  
        
      # Acumulación ponderada del flujo
      execGRASS("r.accumulate", flags=c("overwrite"), Sys_show.output.on.console=F,
                parameters=list(direction="fdr",
                                weight='qweight',
                                accumulation="qacum"))

      
      # Leer salida de GRASS en R
      Qacum      <- raster(readRAST('qacum'))
      cat('\f')
      # crs(Qacum) <- crs(Shapefile)
  
  
      # Q de salida
      Qsub <- c()
      for(w in 1:nSub){
        Qsub[w] <- maxValue(setMinMax(mask(Qacum, Shapefile[w,])))
      }
    
    
      # Salidas
      return(Qsub)
  }