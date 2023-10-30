getAridity <- function(df = sites, sf = site_watersheds){

# options(timeout = max(1000, getOption("timeout")))
# call <- "https://figshare.com/ndownloader/files/34377245"
# temp1 <- tempfile()
# download.file(paste0(call), destfile = temp1, method = "curl")
# unzip(temp1, exdir = "data/geo_extras")

sf::sf_use_s2(FALSE)
# downloaded this data on 2.22.23
aridity <- terra::rast("data/Global-AI_ET0_v3_annual/ai_v3_yr.tif") %>% terra::project(df)

point_aridity <- terra::extract(aridity, df, na.rm = TRUE, df = TRUE) %>%
  select(ai_et0 = 2) %>%
  mutate(AriditySite = ai_et0/10000) %>%
  select(AriditySite)

ws_aridity <- terra::extract(aridity, sf, mean, na.rm=T, df=TRUE) %>%
  select(ai_et0 = 2) %>%
  mutate(AridityWs = ai_et0/10000) %>%
  select(AridityWs)

table <- cbind(st_drop_geometry(sf), ws_aridity) %>%
  inner_join(cbind(df, point_aridity), ., by = c("comid","site")) 

return(table) 

}
