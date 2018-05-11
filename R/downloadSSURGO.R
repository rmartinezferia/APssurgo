
downloadSSURGO <- function(SiteName = "field",
                           north=NULL,south=NULL,east=NULL,west=NULL,
                           by_soil = FALSE,
                           soilLayer_breaks){

    
    a <- polygon_from_extent(extent(west,east,south,north), # xmin, xmax, ymin, ymax
                             proj4string="+proj=longlat")
    
    x <- get_ssurgo(template=a, label=SiteName)
    
    return(x)

}
