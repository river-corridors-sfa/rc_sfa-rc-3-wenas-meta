getChemistry <- function(df = sites, sf = site_watersheds){
  
  sf::sf_use_s2(FALSE)
  
  aluminum <- terra::rast(paste0("data/ChemistryData/aluminum_ngs.tif")) %>% terra::project(., sites)
  calcium <-terra::rast(paste0("data/ChemistryData/calcium_ngs.tif")) %>% terra::project(., sites)
  copper <-terra::rast(paste0("data/ChemistryData/copper_ngs.tif")) %>% terra::project(., sites)
  iron <-terra::rast(paste0("data/ChemistryData/iron_ngs.tif")) %>% terra::project(., sites)
  manganese <-terra::rast(paste0("data/ChemistryData/manganese_ngs.tif")) %>% terra::project(., sites)
  phosph <- terra::rast(paste0("data/ChemistryData/phosph_ngs.tif")) %>% terra::project(., sites)

  
  point_aluminum <- terra::extract(aluminum, df, na.rm = TRUE, df = TRUE) %>%
    select(AluminumSite = 2)
      
  ws_aluminum <- terra::extract(aluminum, sf, mean, na.rm=T, df=TRUE) %>%
    select(AluminumWs = 2)
  
  point_calcium <- terra::extract(calcium, df, na.rm = TRUE, df = TRUE) %>%
    select(CalciumSite = 2)
  
  ws_calcium <- terra::extract(calcium, sf, mean, na.rm=T, df=TRUE) %>%
    select(CalciumWs = 2)
  
  point_copper <- terra::extract(copper, df, na.rm = TRUE, df = TRUE) %>%
    select(CopperSite = 2)
  
  ws_copper <- terra::extract(copper, sf, mean, na.rm=T, df=TRUE) %>%
    select(CopperWs = 2)
  
  point_iron <- terra::extract(iron, df, na.rm = TRUE, df = TRUE) %>%
    select(IronSite = 2)
  
  ws_iron <- terra::extract(iron, sf, mean, na.rm=T, df=TRUE) %>%
    select(IronWs = 2)
  
  point_manganese <- terra::extract(manganese, df, na.rm = TRUE, df = TRUE) %>%
    select(ManganeseSite = 2)
  
  ws_manganese <- terra::extract(manganese, sf, mean, na.rm=T, df=TRUE) %>%
    select(ManganeseWs = 2)
  
  point_phosph <- terra::extract(phosph, df, na.rm = TRUE, df = TRUE) %>%
    select(PhosphSite = 2)
  
  ws_phosph <- terra::extract(phosph, sf, mean, na.rm=T, df=TRUE) %>%
    select(PhosphWs = 2)
  
  ws <- cbind(ws_aluminum, ws_calcium, ws_copper,
              ws_iron, ws_manganese, ws_phosph)
  sites <- cbind(point_aluminum, point_calcium, point_copper,
                 point_iron, point_manganese, point_phosph)
    
  table <- cbind(st_drop_geometry(sf), ws) %>%
    inner_join(cbind(df, sites), ., by = c("comid","site")) 
  
  return(table) 
  
}
