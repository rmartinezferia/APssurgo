# SSURGO2APSIM Function

source("R/xmlCompile.R")

SSURGO2APSIM <- function(data,
                         area_threshold,
                         site_name,
                         coords,
                         crops,
                         path = "",
                         label = "") {
  
  #message(paste0("Creating APSIM toolbox with soils that occupy an area greather than ",area_threshold*100,"% of the ",site_name)," site.")
  
  #if(!(area_threshold >= 0 & area_threshold <= 1)) stop("'area_threshold' must be between 0 and 1")
  
  #data$soils <- lapply(data$soils, function(x) {if(x[[1]] > area_threshold) return(x)})
  #data$soils <- data$soils[!sapply(data$soils, is.null)] 

  out <- xmlCompile(data, site_name, crops, coords)
  
  writeLines(saveXML(out), paste0(path,site_name,label,".xml"))
}