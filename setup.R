# load libraries ----
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  DT, geojsonio, glue, here, leaflet, lubridate, lwgeom, mregions, 
  plotly, raster, rmapshaper, scales, sf, stringr, tidyverse)
here   <- here::here
select <- dplyr::select

basemap_opacity <- 0.6

# Sea Around Us Project (SAUP) manual install
if (!require(seaaroundus)){
  url_wkt <- "https://cran.r-project.org/src/contrib/Archive/wicket/wicket_0.4.0.tar.gz"
  wkt_gz <- here(glue("software/{basename(url_wkt)}"))
  install.packages(wkt_gz, repos = NULL, type="source")
  
  url_saup <- "https://cran.r-project.org/src/contrib/Archive/seaaroundus/seaaroundus_1.2.0.tar.gz"
  saup_gz <- here(glue("software/{basename(url_saup)}"))
  
  if (!file.exists(saup_gz)){
    download.file(url_saup, saup_gz)  
  }
  install.packages(saup_gz, repos = NULL, type="source")

  library(seaaroundus)
}


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
get_eez <- function(cty, geojson = here(glue("data/{cty}_eez.geojson"))){
  # cty = "BRA"; geojson = here(glue("data/eez_{cty}.geojson")
  
  if (!file.exists(geojson)){
    
    country <- get_country(cty)
    
    mr_eezs <- mr_names("MarineRegions:eez")
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
    leaflet() |>
    # add base: blue bathymetry and light brown/green topography
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = basemap_opacity)) |>
    # add reference: placename labels and borders
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = basemap_opacity)) |> 
    addPolygons(data = eez)}

get_fishing_eez_id <- function(cty){
  country <- get_country(cty)
  saup_eez_matches <- listregions('eez') %>% 
    filter(str_detect(title, glue(".*{country}.*"))) %>% 
    arrange(id)
  
  if (nrow(saup_eez_matches) == 0){
    msg <- glue("The country {country} (cty=='{cty})' did not match any SAUP regions in get_fishing_eez_id().")
    stop(msg)
  }
  
  saup_eez_match1 <- saup_eez_matches %>% 
    slice(1)
  
  if (nrow(saup_eez_matches) > 1){
    msg <- glue("The country {country} (cty=='{cty})' matched more than 1 SAUP region in get_fishing_eez_id(). 
                Choosing first one: '{saup_eez_match1$title}' ({saup_eez_match1$id}) 
                  of all: {paste(saup_eez_matches$title, collapse = ', ')}")
    warning(msg)
  }
  
  saup_eez_match1 %>% 
    pull("id")
}

get_fishing_cells <- function(cty){
  # year = 2014; cty = "BRA"
  
  eez <- get_eez(cty) %>% 
    st_union() %>% 
    ms_simplify()
  wkt <- st_as_text(eez)
  getcells(wkt)
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
    # addProviderTiles(providers$Stamen.TonerLite) %>% 
    # add base: blue bathymetry and light brown/green topography
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = basemap_opacity)) |>
    # add reference: placename labels and borders
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = basemap_opacity)) |>
    addRasterImage(r, colors = pal, opacity = 0.7) %>%
    addLegend(pal = pal, values = values(r), title = "Tons")
}
