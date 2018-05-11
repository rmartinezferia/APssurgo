source("R/SaxtonRawls.R")

calcSSURGO <- function(h, by_soil = TRUE, soilLayer_breaks = c(0,10,20,30,100)){
  
  soils <- vector("list", length(unique(h$name)))
  
  for(i in 1:length(unique(h$name))){
    horizon <- h[h$name == unique(h$name)[i], -1]
    
    # Calculate new variables ------------------------------------------------------------------- 
    
    # Soil physical properties  
    
    horizon$bd <- ifelse( horizon$wetbd < 0.9, 0.9, ifelse(horizon$wetbd > 1.8, 1.8,horizon$wetbd))
    #horizon$bd <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$BD
    #horizon$ll.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$LL15
    #horizon$dul.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$DUL
    
    horizon$ksat <- pmin(horizon$ksat*100/1.157,SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$KSAT*24) # mm/day
    
    #horizon$ksat <- ifelse(horizon$ksat > 500, 500, horizon$ksat) # mm/day
    
    horizon$sat <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$SAT/100
    
    horizon$PO <- 1-horizon$bd/2.65
    
    horizon$Salb <- 0.15 # Bare soil albedo
    
    horizon$MWCON <- 1 #(0-1)
    
    horizon$dul <- horizon$dul/100
    
    horizon$ll <- horizon$ll/100
    
    horizon$SWCON <- (horizon$PO-horizon$dul)/horizon$PO
    
    horizon$AirDry <- ifelse(horizon$center<=15,0.9,ifelse(horizon$center<=30,0.95,1))*horizon$ll
    
    horizon$U <- ifelse(horizon$clay<=20,5+0.175*horizon$clay,
                        ifelse(horizon$clay<=40,7.5+0.05*horizon$clay,
                               ifelse(horizon$clay<=50,11.5-0.05*horizon$clay,
                                      ifelse(horizon$clay<=70,12.75-0.075*horizon$clay,
                                             ifelse(horizon$clay<=80,11-0.05*horizon$clay,0))))) # mm
    
    horizon$cona <- ifelse(horizon$clay<=30,0.025*horizon$clay+3.25,
                           ifelse(horizon$clay<=50,4,
                                  ifelse(horizon$clay<=70,-0.025*horizon$clay+5.25,
                                         ifelse(horizon$clay<=80,3.5,0)))) # mm/d^5
    
    
    horizon$DiffusConst <- 40
    
    horizon$DiffusSlope <- 16
    
    horizon$CN2 <- ifelse(is.na(horizon$CN2),80,horizon$CN2)
    
    horizon$CNRed <- 20 #residue runoff
    
    horizon$CNCov	 <- 0.8
    
    horizon$EnrAcoeff	<- 7.4
    
    horizon$EnrBcoeff	<- 0.2
    
    horizon$XF_maize <- ifelse(horizon$center<=150,1,0)
    
    horizon$KL_maize <-	ifelse(horizon$center<=20,0.08,0.09*exp(-0.007*horizon$center))
    
    horizon$e	<- 0.5  #ifelse(F4=$BC$3,0.07,IF(F4=$BC$4,0.03,0.05))
    
    
    # Soil chemical properties
    
    horizon$ph <- 0.52+1.06*horizon$ph #pH 1:5
    
    horizon$OC <- horizon$om/1.72 # %
    
    horizon$OC <- c(horizon$OC[1],
                    ifelse(horizon$center[-1] >= 100 & diff(horizon$OC) == 0,
                           horizon$OC[1]*exp(horizon$center[-1]*-0.035),
                           horizon$OC)) # exponential decay below 100 cm if data is missing
    
    
    horizon$FInert <- ifelse(horizon$center<=1,0.4,
                             ifelse(horizon$center<=10,0.4,
                                    ifelse(horizon$center<60,0.008*horizon$center+0.32,
                                           ifelse(horizon$center<=120,0.8,
                                                  ifelse(horizon$center<180,0.0032*horizon$center+0.42,
                                                         ifelse(horizon$center<=300,0.99,0)))))) #(0-1)
    
    horizon$FBiom <- ifelse(horizon$center<=10,0.04,
                            ifelse(horizon$center<=20,0.055-0.0015*horizon$center,
                                   ifelse(horizon$center<=30,0.03-0.0005*horizon$center,
                                          ifelse(horizon$center<60,0.0216-0.0002*horizon$center,
                                                 ifelse(horizon$center<=300,0.01,0))))) #(0-1)
    
    horizon$RootCN <- 45
    
    horizon$SoilCN <- 13
    
    horizon$RootWt <- 1000
    
    horizon$sw <- horizon$dul
    
    horizon$no3ppm <- horizon$OC
    
    horizon$nh4ppm <- horizon$OC*0.5
    
    # Bin by layer
    
    horizon$layer <- .bincode(horizon$center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))
    
    horizon %>%
      dplyr::select(-hydrogroup) %>%
      group_by(layer) %>%
      mutate(thick = 10 + (max(center)-min(center))*10) %>%
      summarise_all("mean") -> horizon
    
    
    # Save df into list ###########
    
    soils[[i]] <- list(area = unique(horizon$area),
                       horizon = data.frame(horizon %>% 
                                              dplyr::select(-area, -top, -bottom,-center,-slope_code,-slope)))
    
  }
  
  names(soils) <- (unique(h$name))
  
  ### Return average soil
  
  if(by_soil) {
    
    soils <- list(Average_Soil =
                    list(area = 1,
                         horizon = do.call(rbind, sapply(soils, rbind)[2,])%>%
                           group_by(layer) %>%
                           summarise_all(weighted.mean,unlist(sapply(soils, rbind)[1,]))  %>%
                           mutate(thick = round(thick)) %>%
                           as.data.frame()))
  }
  
  return(soils)
  
}

#calcSSURGO(x2,by_soil = TRUE)
