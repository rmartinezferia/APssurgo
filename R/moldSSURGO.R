#x <- readRDS("x.rds")

Int <- function (value){
  
  depth <- 1:length(value)
  
  if(all(is.na(value)) | all(is.infinite(value))) { rep(NA, length(depth)) }
  
  if(length(unique(depth[!is.na(depth)])) > 1){
    
    approx(depth, value, 
           rule=2,
           xout=1:length(depth)-1,
           method="linear")$y
  }
  
  else {
    
    rep(unique(depth[!is.na(depth)]),length(depth)) 
  }
}


moldSSURGO <- function(x, soilLayer_breaks = c(0,10,20,30,100)){
  
  #stop(class(a) != "SpatialPolygons", "Needs a 'SpatialPolygons' object")
  
  # Extract useful data from
  component <- x$tabular$component
  chorizon <- x$tabular$chorizon
  mapunit <-x$tabular$mapunit
  watertable <- x$tabular$muaggatt[ , c("musym","wtdepaprjunmin")]
  
  majcomp <- component[component$majcompflag == "Yes", c("compname",
                                                         "taxclname",
                                                         "drainagecl",
                                                         "mukey","cokey","slope.r","hydgrp")]
  
  x$spatial@data %>%
    mutate(MUKEY = as.numeric(as.character(MUKEY))) %>%
    left_join(majcomp %>%
                mutate(mukey = as.numeric(as.character(mukey))), by = c("MUKEY" = "mukey")) -> majcomp2 
  
  # Merge data
  majcomp<- unique(merge(data.frame(mukey=as.character(x$spatial$MUKEY),
                                    musym=as.character(x$spatial$MUSYM)),majcomp))
  
  
  # Calculate % of area
  area <- x$spatial@data
  area$id <- rownames(x$spatial@data)
  area$area <- NA
  
  for (i in 1:length(area$id)) area$area[i] <- x$spatial@polygons[[i]]@area
  
  area %>% 
    left_join(majcomp, by = c("MUSYM" = "musym")) %>%
    group_by(MUSYM,compname) %>% 
    summarise(area=sum(area)) %>%
    group_by() %>%
    mutate(area = area/sum(area)) %>%
    as.data.frame() %>%
    `names<-`( c("musym","compname","area"))-> area
  
  # Merge into dataset
  majcomp %>%
    left_join(area) %>%
    left_join(chorizon) %>%
    left_join(watertable) %>%
    arrange(compname,hzdept.r) %>%
    mutate(compname = paste0(compname,"_",musym)) %>%
    select(c("compname","area","slope.r", "hydgrp","hzname",
             "hzdept.r","hzdepb.r","hzthk.r", # .r means representative value
             "sandtotal.r","claytotal.r",
             "dbthirdbar.r","dbovendry.r", 
             "om.r","ksat.r",
             "wfifteenbar.r","wthirdbar.r","ph1to1h2o.r","wtdepaprjunmin")) %>%
    `names<-`(c("name","area","slope","hydrogroup","hzname","top","bottom","thick","sand","clay","wetbd","drybd", "om","ksat","ll","dul","ph","watertable")) %>%
    mutate(watertable_g = watertable + 50) -> h 

  # Bin slope groups
  hydrogroup <- readRDS("R/hydrogroup.rds")
  h$slope_code <- .bincode(h$slope, breaks=c(0,2,5,10,100))
  
h %>%
  left_join(hydrogroup) %>%
  mutate(thick = ifelse(is.na(thick),bottom - top,thick),
         center = trunc(top + thick/2)) %>%
  group_by(name) %>%
  mutate(maxdepth  = max(bottom))-> h 
  
  # Add extra layer to the bottom if not deep enough 
  h$maxdepth <- ave(h$bottom,h$name,FUN = max)
  h2 <- h[h$bottom == h$maxdepth,]
  
  if(any(h2$maxdepth < max(soilLayer_breaks))) {
    h2 <- h2[h2$maxdepth < max(soilLayer_breaks),]
    h2$top <- h2$bottom + 1
    h2$bottom <- max(soilLayer_breaks)
    h2$thick <- h2$bottom - h2$top
    h <- rbind(h,h2)
  }
  
  h %>%
    select(-maxdepth) %>%
    #h <- h[order(h$name, h$center),-length(h2)]
    full_join(
      left_join(data.frame(expand.grid(name = unique(h$name),
                                       center = 1:max(soilLayer_breaks))),
                unique(h[,c("name","area","slope","hydrogroup","top","bottom","thick","slope_code","CN2")]))) %>%
    filter(center > top, center <= bottom, center < max(soilLayer_breaks)) %>%
    arrange(name, center) %>% 
    select(-hzname, -watertable,-watertable_g) %>%
    group_by(name) %>%
    mutate_at(c("sand","drybd","wetbd","ksat","clay",
                "om","ll","dul","ph"),
              Int) %>%
    mutate(layer = .bincode(center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))) -> h
}
  
#moldSSURGO(x) -> x2

#group_by(name,top,bottom,thick,hydrogroup) %>%
#summarise_all(mean)

  
  