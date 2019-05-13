# Este script procesa archivos netCDF de Modelos de Circulaci?n General (GCM)
# Creado por Harold Llauca

  # Remover variables anteriores y limpiar consola
    rm(list=ls())
    cat('\f')
  
  # Cargar libreria
    library(plotrix)
  
  
  # Ingresar directorio de trabajo
    path      <- 'D:/Arcata/GCM/Historico/Pp'
    setwd(path)
    
  
  # Cargar puntos de estaciones (Nombre, lon, lat)
    ptos <- read.table('Estaciones.csv', sep=',', header=T)
    nom  <- ptos$ESTACION
    lon  <- ptos$LONGITUD
    lat  <- ptos$LATITUD
    n.est<- length(nom)
    
  for (i in 1:n.est){
  
    # Generar filenames de archivos .CSV
      file.est  <- paste('Pp_mensual', nom[i],'EXT.csv', sep='_')
      file.GCM  <- paste('Pp_mensual', nom[i],'GCM.csv', sep='_')
    
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
    tiff(paste('Pp_estacionalidad_',nom[i],'.tif', sep=''), units='px', res=200, height=1300, width=1600)
    
      # Parametros graficos
        par(mfrow=c(6,4))
        par(mar=c(0,0,3,2), oma=c(2,4,0.5,0.5))
        par(tcl=0.2)
        par(mgp=c(0,0.1,0))
      
      # Crear figura
        for (w in 1:21){
          barp(OBS.var, col='black', ylim=c(0,200), names.arg=F, height.at=5000, shadow=T)
          lines(1:12, GCM.var[,w], lwd=1.5, col='red')
          mtext(modelos[w], side=3, cex=0.6, font=2, line=0.05)
          axis(1, at=1:12, labels=c('S','O','N','D','E','F','M','A','M','J','J','A'), cex.axis=0.7)
          axis(3, at=1:12, labels =F)
          axis(2, at=seq(0,150,50), cex.axis=0.8, las=2)
          axis(4, at=seq(0,150,50), labels=F)
          abline(v=1:12, col="gray50", lty="dotted", lwd=.5)
          abline(h=seq(0,150,50), col="gray50", lty="dotted", lwd=.5)
        }
        mtext('Precipitación [mm]', side=2, cex=0.8, line=2, outer=T)
        mtext('Año hidrológico', side=1, cex=0.8, line=0.2, outer=T)
      
      # Crear leyenda
        par(xpd=NA)
        legend(50, 160, c('OBS', 'GCM'), pch=c(22,1), lty=c(NA,1),lwd=c(NA,2),
               pt.cex=c(3,NA), pt.bg=c('black', NA), cex=1, col=c('black','red'), horiz=F,
               x.intersp=1, y.intersp=1.5)
    
    dev.off()
  
    
  
    # Figura: Correlacion lineal
    tiff(paste('Pp_regresion_',nom[i],'.tif', sep=''), units='px', res=200, height=1300, width=1250)
    
      # Parametros graficos
        par(mfrow=c(5,5))
        par(mar=c(0,0,3,2), oma=c(2,4,0.5,0.5))
        par(tcl=0.2)
        par(mgp=c(0,0.1,0))
        

          # Crear figura
            for (w in 1:21){
              R   <- round(cor(OBS.var, GCM.var[,w]), 3)
              fit <- lm(GCM.var[,w] ~ OBS.var)
              labs<- seq(0,200,100)
              plot(OBS.var, GCM.var[,w], type='p', pch=22, bg='green', cex=1.8, col='black', ann=F,
                   xaxt='n', yaxt='n', ylim=c(0,200), xlim=c(0,200), asp=1)
              axis(1, at=labs, cex.axis=.9, lwd=.5)
              axis(2, at=labs, cex.axis=.9, lwd=.5, las=2)
              axis(3, at=labs, lab=F, lwd=.5)
              axis(4, at=labs, lab=F, lwd=.5)
              abline(v=labs, col="gray50", lty="dotted", lwd=.5)
              abline(h=labs, col="gray50", lty="dotted", lwd=.5)
              abline(0,1)
              mtext(modelos[w], side=3, cex=0.6, font=2, line=0.05)
              abline(fit, lwd=1, lty=2, col='red')
              mtext(bquote('R'*'='*.(R)), side=3, adj=0.05, cex=0.6, line=-1.3)
            }
          mtext(expression('P'[GCM]~'[mm]'), side=2, cex=0.8, line=2, outer=T)
          mtext(expression('P'[OBS]~'[mm]'), side=1, cex=0.8, line=0.2, outer=T)
      
        # Crear leyenda
          par(xpd=NA)
          legend(900, 160, c('Regresión Lineal', 'Línea de identidad'), lty=c(2,1),lwd=c(1,1),
                 cex=1, col=c('red','black'), horiz=F, x.intersp=1, y.intersp=1.5)
      
    dev.off()

    
    
    # # Figura: Distribucion acumulada
    # tiff(paste('Pp_dist_acumulada_',nom[i],'.tif', sep=''), units='px', res=200, height=1300, width=1600)
    # 
    # # Parametros graficos
    #   par(mfrow=c(6,4))
    #   par(mar=c(0,0,3,2), oma=c(2,4,0.5,0.5))
    #   par(tcl=0.2)
    #   par(mgp=c(0,0.1,0))
    # 
    # # Crear figura
    #   for (w in 1:21){
    #     OBS.CDF <- sort(OBS, decreasing=F)
    #     GCM.CDF <- sort(GCM[,w], decreasing=F)
    #     Prob    <- 1:length(OBS)/(length(OBS)+1)
    #     
    #     plot(OBS.CDF, Prob, type='l', lwd=1, col='black', ann=F, xaxt='n', yaxt='n')
    #     lines(GCM.CDF, Prob, lwd=1, col='red')
    #     mtext(modelos[w], side=3, cex=0.6, font=2, line=0.05)
    #     axis(1, at=seq(0,550,25), cex.axis=0.8)
    #     axis(3, at=seq(0,550,25), labels=F)
    #     axis(2, at=seq(0,1,0.25), cex.axis=0.7, las=2)
    #     axis(4, at=seq(0,1,0.25), labels =F)
    #     abline(h=seq(0,1,0.25), col="gray50", lty="dotted", lwd=.5)
    #     abline(v=seq(0,550,25), col="gray50", lty="dotted", lwd=.5)
    #   }
    #   mtext('Probabilidad de no excedencia', side=2, cex=0.8, line=2, outer=T)
    #   mtext('Precipitación media mensual [mm]', side=1, cex=0.8, line=0.2, outer=T)
    # 
    # # Crear leyenda
    #   par(xpd=NA)
    #   legend(900, 1, c('OBS', 'GCM'), lty=1, lwd=c(2,1), cex=1, col=c('black','red'),
    #          horiz=F, x.intersp=1, y.intersp=1.5)
    # 
    # dev.off()
  
}