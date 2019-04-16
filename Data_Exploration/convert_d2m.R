##############################################################################
# Title   : convert_d2m.R
# Purpose : Convert daily to monthly data (Q, T, P)
# Author  : Harold Llauca
##############################################################################

convert_d2m <- function(df, tolerance, FUN='sum'){
  
  # data: Dataframe with data (time, station...)
  # tolerance : Maximum numbers of NAs accepted to calculate monthly data
  # FUN : Write 'sum' in case of rainfall data and 'mean' dor streamflow and temperature data. ('sum' as default)
  

  # Auxiliary variables
  time     <- as.Date(df[,1], format='%d/%m/%Y')
  m.ini    <- time[1]
  m.end    <- time[length(time)]
  dates    <- seq(m.ini, m.end, by='month')
  date.num <- as.numeric(m.ini)
  years    <- as.numeric(format(seq(m.ini, m.end, by='month'), '%Y'))
  months   <- as.numeric(format(seq(m.ini, m.end, by='month'), '%m'))
  data     <- df[,-1]
  
  if (is.null(ncol(data)) == TRUE){
    data <- matrix(data, ncol=1, nrow=length(data))
  }
  
  y <- matrix(NA, ncol=ncol(data), nrow=length(dates))
  
  
  for (u in 1:ncol(data)){
    
	# Extract data for each station point
    x <- as.numeric(as.vector(data[,u]))
    
	
    # Sum daily data
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
    
    
    # Mean daily data
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
      ans <- data.frame(dates, y)
    }
 }
   
  # Output
  return(ans)
}
