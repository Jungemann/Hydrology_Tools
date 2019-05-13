# Este script procesa archivos netCDF de Modelos de Circulaci?n General (GCM)
# Creado por Harold Llauca

  # Remover variables anteriores y limpiar consola
  rm(list=ls())
  cat('\f')
  
  # Cargar libreria
  # install.packages("plotrix")
  
  # Leer libreria
  library(plotrix)
  
  # Ingresar directorio de trabajo
  path      <- 'D:/Arcata/GCM/Historico/Tmax'
  setwd(path)
  
  
  # Cargar puntos de estaciones (Nombre, lon, lat)
  ptos <- read.table('Estaciones.csv', sep=',', header=T)
  nom  <- ptos$ESTACION
  lon  <- ptos$LONGITUD
  lat  <- ptos$LATITUD
  n.est<- length(nom)
  labs <- cbind(seq(10,18,2),seq(13,21,2),seq(13,22,2),seq(10,18,2),seq(16,25,2),seq(13,22,2))
  ymin <- c(11,14,12,11,16,12)
  ymax <- c(17,20,20,17,25,22)
  pos  <- ymax
  
  
  for (i in 1:n.est){
  
    # Generar filenames de archivos .CSV
    file.est  <- paste('Tmax_mensual',nom[i],'EXT.csv', sep='_')
    file.GCM  <- paste('Tmax_mensual',nom[i],'GCM.csv', sep='_')
    
    # Cargar datos GCM y Observados
    df.est    <- read.table(file.est, sep=',', header=T)
    df.GCM    <- read.table(file.GCM, sep=',', header=T)
    OBS       <- as.matrix(df.est[,-1])
    GCM       <- as.matrix(df.GCM[(-2:-133),-1])
    modelos   <- colnames(df.GCM)[-1]
  
    # Datos medios mensuales (estacionalidad)
    OBS.var   <- apply(matrix(OBS, nrow=12, ncol=length(OBS)/12), 1, mean)
    GCM.var   <- matrix(NA, ncol=21, nrow=12)
    for (j in 1:21){
      GCM.var[,j] <- apply(matrix(GCM[,j], nrow=12, ncol=length(OBS)/12), 1, mean)
    }
    
    # Ordenar por a?o hidrologico
    GCM.var   <- rbind(GCM.var[9:12,], GCM.var[1:8,])
    OBS.var   <- c(OBS.var[9:12], OBS.var[1:8])
    
  
    # Figura: Variacion estacional
    tiff(paste('Tmax_estacionalidad_',nom[i],'.tif', sep=''), units='px', res=200, height=1300, width=1600)
    
      # Parametros graficos
      par(mfrow=c(6,4))
      par(mar=c(0,0,3,2), oma=c(2,4,0.5,0.5))
      par(tcl=0.2)
      par(mgp=c(0,0.1,0))
      
      # Crear figura
      for (w in 1:21){
        plot(OBS.var, type='l', lwd=2, col='black', ylim=c(ymin[i],ymax[i]), ann=F, xaxt='n', yaxt='n')
        lines(1:12, GCM.var[,w], lwd=1, col='red')
        mtext(modelos[w], side=3, cex=0.6, font=2, line=0.05)
        axis(1, at=1:12, labels=c('S','O','N','D','E','F','M','A','M','J','J','A'), cex.axis=0.7)
        axis(3, at=1:12, labels =F)
        axis(2, at=labs[,i], cex.axis=0.8, las=2)
        axis(4, at=labs[,i], labels=F)
        abline(v=1:12, col="gray50", lty="dotted", lwd=.5)
        abline(h=labs[,i], col="gray50", lty="dotted", lwd=.5)
      }
      mtext('Temperatura máxima mensual [ºC]', side=2, cex=0.8, line=2, outer=T)
      mtext('Año hidrológico', side=1, cex=0.8, line=0.2, outer=T)
  
      # Crear leyenda
      par(xpd=NA)
      legend(45, pos[i], c('OBS', 'GCM'), lty=1, lwd=c(2,1), cex=1, col=c('black','red'),
             horiz=F, x.intersp=1, y.intersp=1.5)
    
    dev.off()
    
    
    
    # Figura: Correlacion lineal
    tiff(paste('Tmax_regresion_',nom[i],'.tif', sep=''), units='px', res=200, height=1300, width=1250)
    
      # Parametros graficos
      par(mfrow=c(5,5))
      par(mar=c(0,0,3,2), oma=c(2,4,0.5,0.5))
      par(tcl=0.2)
      par(mgp=c(0,0.1,0))
      
      for (w in 1:21){
        R   <- round(cor(OBS.var, GCM.var[,w]), 3)
        fit <- lm(GCM.var[,w] ~ OBS.var)

        # Crear figura
        plot(OBS.var, GCM.var[,w], type='p', pch=22, bg='green', cex=1.8, col='black', ann=F,
             xaxt='n', yaxt='n', ylim=c(ymin[i],ymax[i]), xlim=c(ymin[i],ymax[i]), asp=1)
        axis(1, at=labs[,i], cex.axis=.9, lwd=.5)
        axis(2, at=labs[,i], cex.axis=.9, lwd=.5, las=2)
        axis(3, at=labs[,i], lab=F, lwd=.5)
        axis(4, at=labs[,i], lab=F, lwd=.5)
        abline(v=labs[,i], col="gray50", lty="dotted", lwd=.5)
        abline(h=labs[,i], col="gray50", lty="dotted", lwd=.5)
        abline(0,1)
        mtext(modelos[w], side=3, cex=0.6, font=2, line=0.05)
        abline(fit, lwd=1, lty=2, col='red')
        mtext(bquote('R'*'='*.(R)), side=3, adj=0.05, cex=0.6, line=-1.3)
      }
      mtext(expression('Tmax'[GCM]~'[ºC]'), side=2, cex=0.8, line=2, outer=T)
      mtext(expression('Tmax'[OBS]~'[ºC]'), side=1, cex=0.8, line=0.2, outer=T)
  
      # Crear leyenda
      par(xpd=NA)
      legend(1.8*ymax[i], ymax[i], c('Regresión Lineal', 'Línea de identidad'), lty=c(2,1),lwd=c(1,1),
             cex=1, col=c('red','black'), horiz=F, x.intersp=1, y.intersp=1.5)
      
    dev.off()

    
    
    # # Figura: Variacion estacional
    # tiff(paste('Tmax_dist_acumulada_',nom[i],'.tif', sep=''), units='px', res=200, height=1300, width=1600)
    # 
    # # Parametros graficos
    # par(mfrow=c(6,4))
    # par(mar=c(0,0,3,2), oma=c(2,4,0.5,0.5))
    # par(tcl=0.2)
    # par(mgp=c(0,0.1,0))
    # 
    # # Crear figura
    # for (w in 1:21){
    #   OBS.CDF <- sort(OBS, decreasing=F)
    #   GCM.CDF <- sort(GCM[,w], decreasing=F)
    #   Prob    <- 1:length(OBS)/(length(OBS)+1)
    #   
    #   plot(OBS.CDF, Prob, type='l', lwd=1, col='black', ann=F, xaxt='n', yaxt='n')
    #   lines(GCM.CDF, Prob, lwd=1, col='red')
    #   mtext(modelos[w], side=3, cex=0.6, font=2, line=0.05)
    #   axis(1, at=seq(10,20,2), cex.axis=0.8)
    #   axis(3, at=seq(10,20,2), labels=F)
    #   axis(2, at=seq(0,1,0.25), cex.axis=0.7, las=2)
    #   axis(4, at=seq(0,1,0.25), labels =F)
    #   abline(h=seq(0,1,0.25), col="gray50", lty="dotted", lwd=.5)
    #   abline(v=seq(10,20,2), col="gray50", lty="dotted", lwd=.5)
    # }
    # mtext('Probabilidad de no excedencia', side=2, cex=0.8, line=2, outer=T)
    # mtext('Temperatura máxima mensual [ºC]', side=1, cex=0.8, line=0.2, outer=T)
    # 
    # # Crear leyenda
    # par(xpd=NA)
    # legend(40, 1, c('OBS', 'GCM'), lty=1, lwd=c(2,1), cex=1, col=c('black','red'),
    #        horiz=F, x.intersp=1, y.intersp=1.5)
    # 
    # dev.off()
    
}