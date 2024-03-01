sus_mapper <- function(x){
  
  nearby_catchments <- vector("list",length = nrow(x))
  nearby_flowlines <-  vector("list",length = nrow(x))
  
  for(i in 1:nrow(x)){
    
    nearby_flowlines[[i]] <- get_nhdplus(AOI = x[i,],
                                         realization = "flowline",
                                         t_srs = 4269) %>%
      dplyr::select(comid)
    
    nearby_catchments[[i]] <- get_nhdplus(AOI = x[i,], 
                                          realization = "catchment",
                                          t_srs = 4269) %>%
      dplyr::select(comid = featureid)
    
  }
  
  nearby_catchments <- bind_rows(nearby_catchments)
  nearby_flowlines <- bind_rows(nearby_flowlines)
  
  return(list(nearby_flowlines, nearby_catchments))
  
}