getOmernikWs <- function(df = sites, sf = site_watersheds){
  
  call <- "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip"
  
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  sf::sf_use_s2(FALSE)
  
  ecoregs <- sf::st_read(dsn = temp2) %>%
    sf::st_transform(4269) 
  
  lt3 <- ecoregs %>%
    group_by(NA_L3NAME) %>%
    summarize() %>%
    rowid_to_column()
  
  lt2 <- ecoregs %>%
    group_by(NA_L2NAME) %>%
    summarize() %>%
    rowid_to_column()
  
  lt1 <- ecoregs %>%
    group_by(NA_L1NAME) %>%
    summarize() %>%
    rowid_to_column()

  # Rasterize ecoregs
  l3 <- terra:::from_stars(stars::st_rasterize(lt3 %>% dplyr::select(NA_L3NAME, geometry), dx = 0.008333333, dy = 0.008333333))
  
  l2 <- terra:::from_stars(stars::st_rasterize(lt2 %>% dplyr::select(NA_L2NAME, geometry), dx = 0.008333333, dy = 0.008333333))

  l1 <- terra:::from_stars(stars::st_rasterize(lt1 %>% dplyr::select(NA_L1NAME, geometry), dx = 0.008333333, dy = 0.008333333))
  
  ws_omernik_iii <- terra::extract(l3, sf, modal, na.rm=T, df=TRUE) %>%
    select(rowid=2) %>%
    left_join(lt3, by ="rowid") %>%
    select(OmernikIIIWs=NA_L3NAME)
  
  ws_omernik_ii <- terra::extract(l2, sf, modal, na.rm=T, df=TRUE) %>%
    select(rowid=2) %>%
    left_join(lt2, by ="rowid") %>%
    select(OmernikIIWs=NA_L2NAME)
  
  ws_omernik_i <- terra::extract(l1, sf, modal, na.rm=T, df=TRUE) %>%
    select(rowid=2) %>%
    left_join(lt1, by ="rowid") %>%
    select(OmernikIWs=NA_L1NAME)
  
  table <- cbind(st_drop_geometry(sf), ws_omernik_iii, ws_omernik_ii, ws_omernik_i) %>%
    inner_join(df, ., by = c("site","comid"))
  
  return(table)
  
}
