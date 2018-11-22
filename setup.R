# load libraries ----
library(tidyverse)
library(here)
library(glue)
library(rmapshaper)
library(geojsonio)
library(lwgeom)
library(sf)
library(mregions) # install.packages("mregions")
library(leaflet)
library(seaaroundus) # install.packages("seaaroundus")
library(raster)
library(scales)
library(DT)
library(plotly)
library(lubridate)
here <- here::here
select <- dplyr::select

raw_url <- glue("https://raw.githubusercontent.com/sdgplus/{tolower(cty)}/master")

# custom functions ----
# TODO: move these functions into dedicated sdgtools R package with documentation

get_country <- function(cty){
  cty_csv <- here("data/country-codes.csv")
  cty_url <- "https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv"
  
  if (!file.exists(cty_csv)){
    d_cty <- read_csv(cty_url)
    write_csv(d_cty, cty_csv)
  }
  d_cty <- read_csv(cty_csv) # View(d_cty)
  
  country <- d_cty %>% 
    filter(FIFA == cty) %>% 
    pull(official_name_en)
}
get_eez <- function(cty, geojson = here(glue("data/eez_{cty}.geojson"))){
  
  if (!file.exists(geojson)){
    
    country <- get_country(cty)
    
    eez_ids <- mr_names_search(mr_eezs, country) %>% pull(id)
    eez_list <- lapply(eez_ids, function(id) 
      mr_features_get('MarineRegions:eez', id, format='json') %>% 
        as.json() %>% 
        geojson_sf())
    eez_sf <- do.call(rbind, eez_list) %>% 
      filter(pol_type == "200NM") %>% 
      st_make_valid() %>% 
      st_cast()
    
    #eez_sf <- ms_dissolve(eez_rbind)
    write_sf(eez_sf, geojson)
  } else {
    eez_sf <- read_sf(geojson) %>%
      st_make_valid() %>% 
      st_cast() 
  }
  eez_sf
}
map_eez <- function(eez){
  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>% 
    addPolygons(data = eez)}
get_fishing_eez_id <- function(cty){
  country <- get_country(cty)
  listregions('eez') %>% 
    filter(title == !!country) %>% 
    pull("id")
}
get_fishing_cells <- function(year, cty){
  # year = 2014; cty = "SEN"
  
  eez <- get_eez(cty) %>% 
    ms_simplify()
  wkt <- st_as_text(eez$geometry)
  cells <- getcells(wkt)
  getcelldata(year, cells) %>% 
    as_tibble()
}
get_fishing_empty_grid <- function(){
  # raster specifications for 0.5 degree global raster
  raster(
    xmn = -180, xmx = 180, ymn = -90, ymx = 90, 
    resolution=0.5, crs=leaflet:::epsg4326)
}
map_fishing_cells <- function(d_cells, expr) {
  expr <- enquo(expr)
  # expr <- quo(fishing_entity_name == country)
  
  d <- d_cells %>% 
    filter(!! expr) %>% 
    group_by(cell_id) %>% 
    summarize(
      tons = sum(catch_sum))
  
  r <- get_fishing_empty_grid()
  r[d$cell_id] <- d$tons
  r <- trim(r)
  
  pal <- colorNumeric("Spectral", values(r), na.color = "transparent")
  
  leaflet() %>% 
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    addRasterImage(r, colors = pal, opacity = 0.7) %>%
    addLegend(pal = pal, values = values(r), title = "Tons")
}
