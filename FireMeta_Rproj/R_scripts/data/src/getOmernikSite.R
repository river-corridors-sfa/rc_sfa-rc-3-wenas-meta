getOmernikSite <- function(df = sites){
  
  call <- "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3_state_boundaries.zip"
  
  #download omernik data from EPA 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  sf::sf_use_s2(FALSE)

  ecoregs <- sf::st_read(dsn = temp2) %>%
    sf::st_transform(4269) %>%
    group_by(NA_L3NAME,NA_L2NAME,NA_L1NAME) %>%
    summarize() %>%
    rowid_to_column()

  # Rasterize ecoregs
  l3 <- terra:::from_stars(stars::st_rasterize(ecoregs %>% dplyr::select(NA_L3NAME, geometry), dx = 0.008333333, dy = 0.008333333))
  
  point_omernik <- terra::extract(l3, df, na.rm = TRUE, df = TRUE) %>%
    select(rowid=2) %>%
    left_join(ecoregs, by ="rowid") %>%
    select(OmernikIIISite=NA_L3NAME,OmernikIISite=NA_L2NAME,OmernikISite=NA_L1NAME) 
  
  df <- df %>% cbind(point_omernik)
  
  return(df)
  
}
