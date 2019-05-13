# Este script procesa archivos netCDF de Modelos de Circulaci?n General (GCM)
# Creado por Harold Llauca

	# Remover variables anteriores y limpiar consola
	rm(list=ls())
	cat('\f')

	# Ingresar directorio de trabajo
	path      <- 'D:/Arcata/GCM/Historico/Tmax'
	setwd(path)

	# # Ingresar directorio GCM
	# path.gcm  <- 'F:/DATA_GCM/Pp'
	# 
	# # Cargar funcion
	# source('GCM_extract_region.R')
	# 
	# # Extraer datos para la region de estudio
	# x <- GCM_extract_region(path.gcm, c(-73.25,-70.91, -15.67, -14.16), 'Historical', 'Tmax')
	# x$data            # Datos GCM en 5D [lon x lat d?a x a?o x modelo]
	# dim(x$data)
	# 
	# # Exportar datos en formato .Rdata
	# save(x, file='Tmax_GCM_Arcata_Historical.Rda')


	# Escalamiento a punto de estaci?n ======================================================
	
	# Cargar funcion
	source('GCM_spatial_down.R')
	
	# Cargar archivo en Rdata
	load('Tmax_GCM_Arcata_Historical.Rda')

	# Cargar puntos de estaciones (Nombre, lon, lat)
	ptos <- read.table('Estaciones.csv', sep=',', header=T)
	nom  <- ptos$ESTACION
	lon  <- ptos$LONGITUD
	lat  <- ptos$LATITUD
	n.est<- length(nom)
	
	for (i in 1:n.est){
	  
  	# Extraer serie de tiempo al punto de estaci?n
  	y <- GCM_spatial_down(x$data, x$lon, x$lat, c(lon[i], lat[i]), x$yini)
  
  	# Mensualizar datos diarios
  	df            <- data.frame(y$dates, y$est)
  	colnames(df)  <- c('Fecha', x$model)
  	meses         <- unique(format(as.Date(df$Fecha), "%B-%Y"))
  	df$Fecha      <- format(as.Date(df$Fecha), "%B-%Y")
  	df            <- aggregate(df[-1], by=list(df$Fecha), FUN=mean)
  	df            <- df[match(meses, df[,1]),]
  
  	# Exportar datos en .csv (delimitado por comas)
  	colnames(df)  <- c('Fecha', x$model)
  	write.table(df, file=paste('Tmax_mensual',nom[i],'GCM.csv', sep='_'), row.names=F, sep=",")
}