all_coordinates <- function(x) {
  
  # Verify packages
  if("maptools" %in% rownames(installed.packages()) == FALSE){
    install.packages("maptools")
  }
  # Load packages
  library(maptools)
  
  ret = NULL 
  polys = x@polygons 
  for(i in 1:length(polys)) { 
    pp = polys[[i]]@Polygons 
    for (j in 1:length(pp)) 
      ret = rbind(ret, coordinates(pp[[j]])) 
  } 
  ret
  
}