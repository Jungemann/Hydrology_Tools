# Este scprit post-procesa Modelos Globales de Clima (GCM) y extrae datos para puntos de interés
# Creado por Harold Llauca

	# Remover variables anteriores y limpiar consola
	rm(list=ls())
	cat('\f')

	
	# Ingresar directorio de trabajo
	path      <- 'D:/Arcata/GCM/Futuro/Pp'
	setwd(path)

	
	# # Ingresar directorio GCM
	# path.gcm  <- '/Users/harold/Documents/CONSULTORIAS/Stantec/Consultoria_CC/GCM/Futuro/Pp'
	# 
	# # Cargar funcion
	# source('GCM_extract_region.R')
	# 
	# # Extraer datos para la region de estudio
	# x <- GCM_extract_region(path.gcm, c(-73.25,-70.91, -15.67, -14.16), 'Future', 'Pp')
	# 
	# # Exportar datos en formato .Rdata
	# save(x, file='Pp_GCM_Arcata_Future.Rda')


	
	# Escalamiento a punto de estación
	#=================================
	
	# Cargar funcion
	source('GCM_spatial_down.R')
	
	# Cargar archivo en Rdata
	load('Pp_GCM_Arcata_Future.Rda')

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
  	df            <- aggregate(df[-1], by=list(df$Fecha), FUN=sum)
  	df            <- df[match(meses, df[,1]),]
  
  	
  	# Exportar datos en .csv (delimitado por comas)
  	colnames(df)  <- c('Fecha', x$model)
  	write.table(df, file=paste('Pp_mensual',nom[i],'GCM_FUT.csv', sep='_'), row.names=F, sep=",")
}