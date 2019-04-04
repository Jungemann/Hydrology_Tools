##############################################################################
# Title   : convert_d2m.R
# Purpose : Mensualiza valores diarios de una variable (Q, T, P)
# Author  : Harold Llauca
##############################################################################

convert_d2m <- function(data, ini, end, tolerance, FUN){


  # data=df
  # ini='01/01/1930'
  # end='23/01/2018'
  # tolerance=5
  # FUN='mean'

  # Datos de entrada
  date.ini <- as.Date(ini, format='%d/%m/%Y')
  date.end <- as.Date(end, format='%d/%m/%Y')
  date.num <- as.numeric(date.ini)
  dates    <- format(seq(date.ini, date.end, by='month'), '%b-%Y')
  years    <- as.numeric(format(seq(date.ini, date.end, by='month'), '%Y'))
  months   <- as.numeric(format(seq(date.ini, date.end, by='month'), '%m'))

  if (is.null(ncol(data)) == TRUE){
        data <- matrix(data, ncol=1, nrow=length(data))
  }
  y <- matrix(NA, ncol=ncol(data), nrow=length(dates))
  
  
  for (u in 1:ncol(data)){
    
    x <- as.numeric(as.vector(data[,u]))

    # Acumular datos diarios
    if (FUN=='sum'){
      i=1
      for (m in months){
          if (m==12){
            ini <- as.numeric(as.Date(paste(years[i], m, 01), format='%Y %m %d')) - date.num + 1
            end <- as.numeric(as.Date(paste(years[i], m, 31), format='%Y %m %d')) - date.num + 1
          } else {
            ini <- as.numeric(as.Date(paste(years[i], m, 01), format='%Y %m %d')) - date.num + 1
            end <- as.numeric(as.Date(paste(years[i], m+1, 01), format='%Y %m %d')) - date.num
          }
        
          if (length(x[ini:end][is.na(x[ini:end])]) >= tolerance){
            y[i,u] <- NA
          } else{
            y[i,u] <- round(sum(x[ini:end], na.rm=T),2)
          }
      i=i+1
      }
      ans <- data.frame(Fecha=dates, y)
    }
  
  
    # Promediar valores diarios
    if (FUN=='mean'){
      i=1
      for (m in months){
        if (m==12){
          ini <- as.numeric(as.Date(paste(years[i], m, 01), format='%Y %m %d')) - date.num + 1
          end <- as.numeric(as.Date(paste(years[i], m, 31), format='%Y %m %d')) - date.num + 1
        } else {
          ini <-as.numeric(as.Date(paste(years[i], m, 01), format='%Y %m %d')) - date.num + 1
          end <-as.numeric(as.Date(paste(years[i], m+1, 01), format='%Y %m %d')) - date.num
        }
        
        if (length(x[ini:end][is.na(x[ini:end])]) >= tolerance){
          y[i,u] <- NA
        } else{
          y[i,u] <- round(mean(x[ini:end], na.rm=T),2)
        }
        i=i+1
      }
      ans <- data.frame(Fecha=dates, y)
    }
  }
  
  # Salida de la función
  return(ans)
}
