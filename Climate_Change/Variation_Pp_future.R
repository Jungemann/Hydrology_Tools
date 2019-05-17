# Este script genera un gráfico de comparación de la variación estacional histórica y futura para la
# precipitación media mensual
# Puede emplearse también para graficar la variación estacional de la temperatura y caudales

# Remover variables anteriores y limpiar consola
rm(list=ls())
cat('\f')


# Cargar libreria
library(plotrix)


# Datos de entrada
model <- c('NorESM1.M','CSIRO.Mk3.6.0','MIROC5')
path  <- 'D:/Arcata/Analisis/Pp'
file  <- 'Pp_mensual_Arcata_CC.csv'


# Directorio de trabajo
setwd(path)


# Leer datos
dta <- read.table(file, sep=',', header=T)
dta <- dta[c(9:12,1:8),]


# Figura: Variacion estacional futura
tiff('Pp_estacionalidad_futura_Arcata.tif', units='px', res=200, height=800, width=1200)

  
    # Parametros graficos
    par(mfrow=c(1,1))
    par(mar=c(3,3,1,1), oma=c(0,0,0,0))
    par(tcl=0.2)
    par(mgp=c(0,0.1,0))
    
    # Generar figura
    xhist <- dta[,2]
    ymin  <- dta[,3]
    yprom <- dta[,4]
    ymax  <- dta[,5]
    
    barp(xhist, col='gray40', ylim=c(0,250), names.arg=F, height.at=5000)
    lines(yprom, ylim=c(0,200), type='o',col='red', lwd=1.5)
    polygon(c(1:12,rev(1:12)), c(ymin, rev(ymax)), border=F, col=rgb(1,0,0,alpha=0.3))
    axis(1, at=1:12, lab=c('S','O','N','D','E','F','M','A','M','J','J','A'), cex.axis=.8, lwd=.5)
    axis(2, at=seq(0,200,50), cex.axis=.8, lwd=.5, las=2)
    axis(3, at=1:12, lab=F, lwd=.5)
    axis(4, at=seq(0,200,50), lab=F, lwd=.5)
    text(1:12, yprom+10, lab=yprom, cex=.6, col='red')
    # abline(v=1:12, col="gray50", lty="dotted", lwd=.5)
    # abline(h=seq(0,200,50), col="gray50", lty="dotted", lwd=.5)
    mtext('Año hidrológico', side=1, cex=.9, line=2)
    mtext('Pp media mensual [mm]', side=2, cex=.9, line=2)
    legend("topright", inset=0.03, c('Histórico (1981-2005)','Futuro (2026-2050)', 'Incertidumbre'), pch=c(15,NA,15),
           pt.cex=c(2,NA,2), pt.bg=c('black',NA, rgb(1,0,0,alpha=0.3)), lty=c(NA,1,NA), cex=0.6,
           col=c('gray40','red', rgb(1,0,0,alpha=0.3)), horiz=F, x.intersp=1, y.intersp=1.5)
    

dev.off()

  
