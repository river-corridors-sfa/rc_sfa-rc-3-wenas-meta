getPRISM <- function(df = sites){

# PRISM normals downloaded on 02.22.2023

tmean <- terra::rast('data/PRISM_tmean_30yr_normal_800mM4_annual_bil/PRISM_tmean_30yr_normal_800mM4_annual_bil.bil') %>% terra::project(., sites)
tmax  <- terra::rast('data/PRISM_tmax_30yr_normal_800mM4_annual_bil/PRISM_tmax_30yr_normal_800mM4_annual_bil.bil') %>% terra::project(., sites)
tmin <- terra::rast('data/PRISM_tmin_30yr_normal_800mM4_annual_bil/PRISM_tmin_30yr_normal_800mM4_annual_bil.bil') %>% terra::project(., sites)
ppt <- terra::rast('data/PRISM_ppt_30yr_normal_800mM4_annual_bil/PRISM_ppt_30yr_normal_800mM4_annual_bil.bil') %>% terra::project(., sites)

point_tmean <- terra::extract(tmean, df, na.rm=T, df=TRUE) %>%
  select(TmeanSite=2)

point_tmax <- terra::extract(tmax, df, na.rm=T, df=TRUE) %>%
  select(TmaxSite=2)

point_tmin <- terra::extract(tmin, df, na.rm=T, df=TRUE) %>%
  select(TminSite=2)

point_ppt <- terra::extract(ppt, df, na.rm=T, df=TRUE) %>%
  select(PrecipSite=2)


df <- cbind(df, point_tmean, point_tmax, point_tmin, point_ppt)

return(df)

}