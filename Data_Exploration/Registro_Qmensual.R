# This script is used to select data by a specific time window and NAs threshold. In the end, a plot
# that show the availability of data for selected stations is created
# By Harold Llauca


# Remove stored variables and clean console
rm(list=ls())
cat('\f')


# Load requiere packages
library(plotrix)


# Enter input data
filename  <- 'Q_mensual_titicaca.csv'
wd        <- 'D:/GR2M_PERU/Caudales'
ini       <- '01/1956'                  
end       <- '03/2018'                  
pr.data   <- 0  # Percent of complited data


# Extract data from .csv file
setwd(wd)
df.raw    <- read.table(filename, sep=',', header=F)


# Extract metadata values
basin <- as.character(as.matrix(df.raw[1,-1]))
id    <- as.character(as.matrix(df.raw[2,-1]))
lon   <- as.numeric(as.matrix(df.raw[3,-1]))
lat   <- as.numeric(as.matrix(df.raw[4,-1]))
nom   <- as.character(as.matrix(df.raw[5,-1]))
df    <- df.raw[-5:-1,-1]
dates <- as.Date(paste0('01-',df.raw[-5:-1,1]), format="%d-%b-%Y")


# Sort data by lower latitudes
id        <- 1:length(nom)
coord     <- cbind(id, lat, lon)
coord.ord <- coord[order(coord[,2], decreasing=T),]
id.ord    <- coord.ord[,1]
nom.ord   <- nom[id.ord]
df.ord    <- df[,id.ord]
metadata  <- data.frame('Estacion'=nom.ord, 'Latitud'=lat[id.ord],
                        'Longitud'=lon[id.ord], 'Cuenca'=basin[id.ord])


# Subset data by an specific time window
id.cut   <- seq(which(format(dates, format="%m/%Y") == ini),
                which(format(dates, format="%m/%Y") == end))
df.cut   <- df.ord[id.cut,]
dates.cut<- dates[id.cut] 


# Subset data by number of NA's in each station
m <- c()
  for (w in 1:ncol(df.cut)){
    m[w] <- round((length(na.omit(df.cut[,w]))/nrow(df.cut))*100,1)
  }
df.end  <- df.cut[,m>pr.data]
nom.end <- nom.ord[m>pr.data]
colnames(df.end) <- nom.end


# Save subset data of the selected stations
ans1   <- data.frame(Fecha=format(dates[id.cut], '%b-%Y'), df.end)
write.table(ans1, file='Qm_titicaca_models.csv', sep=',', row.names=F)


# Save metadata of the selected stations
ans2 <- subset(metadata, metadata$Estacion %in% nom.end)
write.table(ans2, file='Qm_titicaca_metadata.csv', sep=',', row.names=F)


# Figure of available data and stations
tiff('Qm_titicaca_models.tif', units="px", res=200, height=800, width=1200)

    # Graphical parameters
    par(mfrow=c(1,1))
    par(mar=c(2,9,2,4), oma=c(0,0,0,0))
    par(mgp=c(0,0.2,0))
    par(tck=0.005)
  
    # Auxiliary variables
    x <- data.matrix(df.end)
    n <- ncol(df.end)
    for (i in 1:ncol(df.end)){x[!is.na(x[,i]),i] <- rev(1:n)[i]}
    lat.lab <- rev(paste0(format(round(-1*ans2$Latitud,2), nsmall=2),'°S'))
    per.lab <- rev(paste0(format(m[m>pr.data], nsmall=1),'%'))
    nom.lab <- rev(ans2$Estacion)
    xat     <- seq(1, nrow(x), 2*12)
    
    # Make a plot
    plot(1:nrow(x), x[,1], type='l', col='black', lwd=3, ylim=c(0.5, n+0.5), ann=F,
         xaxt='n', yaxt='n', yaxs='i', xaxs='i')
    for (j in 1:(n-1)){lines(1:nrow(x), x[,j+1], col='black', lwd=3)}
    axis(2, at=1:n, lab=nom.lab, cex.axis=0.4, las=2, lwd=0.5)
    axis(2, at=1:n, lab=lat.lab, cex.axis=0.4, las=2, line=5, col='red', col.axis='red', lwd=0.5)
    axis(4, at=1:n, lab=per.lab, cex.axis=0.4, las=2, col.axis='blue', col='blue', lwd=0.5)
    axis(1, at=xat, lab=format(dates.cut[xat],"%Y"), cex.axis=0.4)
    abline(v=xat, col="gray50", lty="dotted", lwd=.5)
    abline(h=1.5:(n-0.5), col="black", lty=1, lwd=.5)
    mtext('Registro de caudales medios mensuales en la vertiente del Titicaca (ene. 1956 - mar. 2018)',
           side=3, cex=0.6, font=2, line=0.5, adj=0.8)
    mtext ('Cobertura de datos', side=4, cex=0.5, line=1.5, col='blue')
    mtext ('Latitud', side=2, cex=0.5, line=7, col='red')

dev.off()