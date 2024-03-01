getNHDxy <- function(df = sites){

  #Get the comid (i.e., hydrography identifier) for each site
for(i in 1:nrow(df)){
  df$comid[i] <- discover_nhdplus_id(df[i,])
}

# Link NHD hydrologic unit code (HUC, essentially sub-watershed polygons) data to each sample.
site_hucs <- list()

for(i in 1:nrow(df)){
  site_hucs[[i]] <- get_huc12(df[i,], t_srs=4269)
}

site_hucs <- do.call('rbind',site_hucs) %>%
  mutate(huc2 = paste0('HUC-',str_sub(huc12,end = 2)),
         huc4 = paste0('HUC-',str_sub(huc12,end = 4)),
         huc6 = paste0('HUC-',str_sub(huc12,end = 6)),
         huc8 = paste0('HUC-',str_sub(huc12,end = 8)),
         huc10 = paste0('HUC-',str_sub(huc12,end = 10)),
         huc12 = paste0('HUC-',huc12)) %>%
  dplyr::select(huc2,huc4,huc6,huc8,huc10,huc12)

#Select NHD flowlines that df are located on, subsequently getting NHD metadata for each sample location.
subset_nhdplus(comids = df$comid,
               output_file = 'data/site_flowlines.gpkg',
               nhdplus_data = 'download',
               overwrite = TRUE,
               return_data = FALSE,
               flowline_only = TRUE,
               out_prj = 4269)

site_lines <- st_read('data/site_flowlines.gpkg', quiet = T) %>%
  #if necessary, remove duplicates that happen due to multiple samples on the same NHD feature:
  distinct(comid,.keep_all=TRUE) %>% 
  st_join(.,site_hucs) %>%
  distinct(comid,.keep_all=TRUE) %>%
  as_tibble()

#join site points to NHD data
df <- df %>%
  left_join(site_lines,by='comid') %>%
  dplyr::select(-c(geom)) %>%
  mutate(across(3:146, as.character)) %>%
  mutate(comid = as.integer(comid))

print(paste0(nrow(df), " locations linked to the NHD."))

return(df)

}

