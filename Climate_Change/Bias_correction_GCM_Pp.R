# Este script correge el sesgo de los GCM seleccionados usando técnicas de Mapeo de Cuantiles
# en caso de procesar variables negativas, adicional la varible wetdays=FALSE en la función
# fitQmap

# Remover variables anteriores y limpiar consola
rm(list=ls())
cat('\f')


#  Cargar libreria
library(qmap)


# Datos de entrada
path.hist  <- 'D:/Arcata/GCM/Historico/Pp'
path.fut   <- 'D:/Arcata/GCM/Futuro/Pp'
model      <- c('NorESM1.M','CSIRO.Mk3.6.0','MIROC5')
estaciones <- 'Estaciones.csv'
IniHist    <- '1981/01/01'
EndHist    <- '2005/12/01'
IniFut     <- '2026/01/01'
EndFut     <- '2050/12/01'


# Procesar datos para cada punto de estación
#===========================================

# Directorio de trabajo
setwd(path.hist)


# Cargar puntos de estaciones (Nombre, lon, lat)
ptos  <- read.table(estaciones, sep=',', header=T)
nom   <- ptos$ESTACION
lon   <- ptos$LONGITUD
lat   <- ptos$LATITUD
n.est <- length(nom)
nData <- length(seq(as.Date(IniHist), as.Date(EndHist), by='month'))


# Procesar datos para cada punto de estación
#===========================================
for (i in 1:n.est){


  # Figura: Bias correction quantile-quantile
  #==========================================
  tiff(paste0('Pp_bias_',nom[i],'.tif'), units='px', res=200, height=550, width=1600)
  
  
    # Parametros gráficos
    par(mfrow=c(1,3))
    par(mar=c(3,3,2,2), oma=c(1,1,0,0))
    par(tcl=0.2)
    par(mgp=c(0,0.1,0))
    
    
    # Crear matriz vacía para almacenar datos
    GCM.fut.corr <- matrix(NA, ncol=length(model), nrow=nData)
    
    
    # Leer cada modelo GCM
    for (j in 1:length(model)){
      
      XHist   <- read.table(paste('Pp_mensual', nom[i],'GCM.csv', sep='_'), sep=',', header=T)
      IdIni   <- which(XHist[,1]==format(as.Date(IniHist), "%b-%y"))
      IdEnd   <- which(XHist[,1]==format(as.Date(EndHist), "%b-%y"))
      GCM     <- XHist[(IdIni:IdEnd),-1]
      
      XFut    <- read.table(paste0(path.fut, '/Pp_mensual_', nom[i],'_GCM_FUT.csv'), sep=',', header=T)
      IdIni   <- which(XFut[,1]==format(as.Date(IniFut), "%B-%Y"))
      IdEnd   <- which(XFut[,1]==format(as.Date(EndFut), "%B-%Y"))
      GCM.fut <- XFut[(IdIni:IdEnd),-1]
      
      
      YHist   <- read.table(paste('Pp_mensual', nom[i],'EXT.csv', sep='_'), sep=',', header=T)
      IdIni   <- which(YHist[,1]==format(as.Date(IniHist), "%b-%y"))
      IdEnd   <- which(YHist[,1]==format(as.Date(EndHist), "%b-%y"))
      OBS     <- YHist[(IdIni:IdEnd),-1]

      
      # Mapeo de cuantiles usando SPLINES
      BIAS.fit  <- fitQmap(OBS, GCM[,model[j]], qstep=0.01, method="SSPLIN")
      GCM.corr  <- doQmap(GCM[,model[j]], BIAS.fit)
   
      # Generar figura
      Min   <- 0
      Max   <- 400
      Delta <- 100
      
      plot(sort(GCM.corr, decreasing=T), sort(OBS, decreasing=T), pch=20, cex=1.5, ylim=c(Min, Max),
           xlim=c(Min, Max), ann=F, xaxt='n', yaxt='n')
      points(sort(GCM$NorESM1.M, decreasing=T), sort(OBS, decreasing=T), col='red', pch=3, cex=0.8)
      axis(1, at=seq(Min, Max, Delta), cex.axis=.9, lwd=.5)
      axis(2, at=seq(Min, Max, Delta), cex.axis=.9, lwd=.5, las=2)
      axis(3, at=seq(Min, Max, Delta), lab=F, lwd=.5)
      axis(4, at=seq(Min, Max, Delta), lab=F, lwd=.5)
      abline(v=seq(Min, Max, Delta), col="gray50", lty="dotted", lwd=.5)
      abline(h=seq(Min, Max, Delta), col="gray50", lty="dotted", lwd=.5)
      abline(0,1)
      mtext(model[j], side=3, cex=0.6, font=2, line=0.05)
      legend("topleft", inset=0.05, c('Con correción', 'Sin correción'), pch=c(20,3), cex=0.8, col=c('black','red'),
             horiz=F, x.intersp=1, y.intersp=1.5)
    
      if (j==length(model)){
      mtext(expression('P'[GCM]~'[mm]'), side=1, cex=0.8, line=-0.5, outer=T)
      mtext(expression('P'[OBS]~'[mm]'), side=2, cex=0.8, line=-0.5, outer=T)  
      }
    
      # Corregir datos futuros del GCM
      GCM.fut.corr[,j] <- doQmap(GCM.fut[,model[j]], BIAS.fit)
    }

  dev.off()

  
  
  # Corregir datos futuros
  #=======================
  
  # Construir dataframe para exportar datos
  dates <- seq(as.Date(IniFut), as.Date(EndFut), by='month')
  dates <- format(dates, '%Y-%b')
  df    <- data.frame(dates, GCM.fut.corr)

  
  # Exportar datos en .csv (delimitado por comas)
  colnames(df) <- c('Fecha', model)
  write.table(df, file=paste0(path.fut,'/Pp_mensual_',nom[i],'_GCM_FUT_corr.csv'), row.names=F, sep=",")
}
