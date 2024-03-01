getWatersheds <- function(df = sites, massive = TRUE, make_pretty = TRUE){
  
  # Read in the NHD. This is a table representing all flow direction data across CONUS.
  nhd <- read_csv('data/nhd_flow_network.csv')
  
  subset_sites <- df %>% #remove comid duplicates (i.e., samples located in the same catchment)
    distinct(comid,.keep_all=T)
  
  watersheds <- function(spid_union){
    
    tracer <- function(samples){
      
      small_db <- as_tibble(subset_sites)
      
      outlet <- small_db %>%
        dplyr::filter(site == samples)
      
      upstream_nhd <- get_UT(nhd, outlet$comid) %>% #upstream trace function in nhdplusTools
        as_tibble() %>%
        dplyr::rename(comid_list = value) %>%
        mutate(origin = outlet$comid)
      
    }
    
    ws <- map_dfr(spid_union, tracer)
  }
  
  upstream_list <- subset_sites %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(comid_list = map(site, watersheds)) %>%
    tidyr::unnest(., cols = comid_list) %>%
    dplyr::select(origin,
           comid = comid_list) %>%
    distinct(.keep_all = TRUE)
  
  if(massive == FALSE){ 
    
    catchments <- vector("list", length = nrow(upstream_list))
    
    for(i in 1:nrow(upstream_list)){
      
      catchments[[i]] <- try(get_nhdplus(comid = unique(upstream_list$comid[i]),
                                         realization = 'catchment', 
                                         t_srs = 4269))
    }
    
    catchments <- bind_rows(catchments) %>%
      dplyr::select(comid = featureid)
    
  }
  
  if(massive == TRUE){
    # If you are running this code across MANY watersheds or watersheds are LARGE (think Mississippi River),
    # you can make the code faster by using the stored CONUS catchment polygons instead of the code below. 
    # Trade-off is polygons are not the most up-to-date since it uses a static, downloaded version.
    catchments <- readRDS('data/us_catchments.RDS') %>%
      dplyr::rename(comid = FEATUREID) %>%
      dplyr::filter(comid %in% upstream_list$comid)
  }
  
  site_watersheds <- merge(catchments, upstream_list, by = 'comid', all.x = FALSE) %>%
    group_by(origin) %>%
    dplyr::summarize() %>%
    dplyr::rename(comid = origin) #%>%
    #mutate(comid = as.character(comid))
  
  # Here, an option to remove odd holes that form within the watershed that are due to 
  # the catchment boundaries not lining up perfectly. However, this MAY introduce wrong watershed
  # boundaries for watersheds with "closed" systems within them.
  if(make_pretty == TRUE){
    site_watersheds <- site_watersheds %>%
    nngeo::st_remove_holes()
  }
  
  return(site_watersheds)
  
}
