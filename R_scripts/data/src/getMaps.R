getMaps <- function(x = sites$site, y = sites$comid){
  
  flowlines <- st_read('~/GitHub/rc_sfa-rc-3-wenas-meta/FireMeta_Rproj/R_scripts/data/site_flowlines.gpkg', quiet = T) %>% filter(comid == y)
  watersheds <- dplyr::filter(site_watersheds, site == x)
  points <- dplyr::filter(sites, site == x)
  
  
  plot <- mapview(watersheds, col.regions = "#56B4E9", alpha.regions = 0.2, lwd = 3, layer.name = "Watershed") + 
          mapview(flowlines, lwd = 8, color = "red", layer.name = "Flowline") + 
          mapview(points, cex = 8, col.region = "black", layer.name = paste0(x))
  
  mapshot(plot, file = paste0('~/GitHub/rc_sfa-rc-3-wenas-meta/FireMeta_Rproj/R_scripts/data/maps/', x, '.jpg'))
}
