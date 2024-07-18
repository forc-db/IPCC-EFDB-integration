# get the terrestrial area of forest types and stand age to use in barplot.


# clear environment ####
rm(list = ls())


# load libraries ####
library(tidyverse)
library(sf)
library(terra)


# load data ####
# stand_age <- rast("S:/Global Maps Data/Global 1km forest age/20247916913222_BGIForestAgeMPIBGC1.0.0.nc") # raster version
stand_age <- read.csv("S:/Global Maps Data/Global 1km forest age/age_class_area_GAMv2.csv")
FAO_ecozone <- st_read("S:/Global Maps Data/Shapefiles/FAO Global Eco_zone/gez_2010_wgs84.shp")
continents <- st_read("../forc/supplementary_resources/World Map data/Continents/World_Continents.shp", stringsAsFactors = F)
SYNMAP <- st_read("S:/Global Maps Data/SYNMAP_Hurtt/synmap_polygon.shp")
SYNMAP_codes <- read.csv("S:/Global Maps Data/SYNMAP_Hurtt/SYNMAP_Legend.csv")


# prepare data ####


## First SYNMAP so we can use it to mask other layers ####

### translate codes
SYNMAP_codes <- SYNMAP_codes %>% mutate(GRIDCODE = as.numeric(Value)) # the Values with "x" are not in SYNMAP anyways

SYNMAP <- SYNMAP %>% left_join(SYNMAP_codes)

SYNMAP <- SYNMAP %>% filter(!Tree.leaf.type %in% "-")

SYNMAP %>% dplyr::select(Life.forms, Tree.leaf.type, Tree.leaf.longevity) %>% st_drop_geometry() %>% unique

SYNMAP <- SYNMAP %>% mutate(
  dom_veg = case_when(Tree.leaf.type == "Needle" & Tree.leaf.longevity == "Evergreen" ~ "Needleleaf\nEvergreen", #"2TEN",
                      Tree.leaf.type == "Needle" & Tree.leaf.longevity == "Deciduous" ~ "Needleleaf\nDeciduous", #"2TDN",
                      Tree.leaf.type == "Needle" & Tree.leaf.longevity == "Mixed" ~ "Needleleaf", #"2TN",
                      Tree.leaf.type == "Broad" & Tree.leaf.longevity == "Evergreen" ~ "Broadleaf\nEvergreen", #"2TEB",
                      Tree.leaf.type == "Broad" & Tree.leaf.longevity == "Deciduous" ~  "Broadleaf\nDeciduous", #"2TDB",
                      Tree.leaf.type == "Broad" & Tree.leaf.longevity == "Mixed" ~  "Broadleaf\n(mixed E/D)",  #"2TB",
                      Tree.leaf.type == "Mixed" & Tree.leaf.longevity == "Evergreen" ~  "Evergreen", #"2TE",
                      Tree.leaf.type == "Mixed" & Tree.leaf.longevity == "Deciduous" ~ "Deciduous", #"2TD",
                      Tree.leaf.type == "Mixed" & Tree.leaf.longevity == "Mixed" ~ "Trees" # "2TREE"
  )
)

unique(SYNMAP$dom_veg)

### group some dom_veg under "Other" to bacth the barplot
SYNMAP <- SYNMAP %>% mutate(
  dom_veg_group = case_when(dom_veg %in% c("Needleleaf\nEvergreen",
                                           "Broadleaf\nDeciduous",
                                           "Broadleaf\nEvergreen",
                                           "Broadleaf\n(mixed E/D)") ~ dom_veg,
                            .default = "Other"))

unique(SYNMAP$dom_veg_group)

### get areas
sf::sf_use_s2(FALSE)
dom_veg_areas <- SYNMAP %>% mutate(Area = st_area(.))  %>% group_by(dom_veg_group) %>% summarize(Area = sum(Area)) %>% st_drop_geometry()
dom_veg_areas$Area


## Stand age ####

### sum  >100 years and old-growth forests and only keep
stand_age <- stand_age %>%
  mutate(name = c("<20 yrs", "20-100 yrs", ">100 yrs", ">100 yrs")) %>%
  summarize(Area = units::set_units(sum(area_2020_median) * 10000000000000, m2), .by = name) # * 10000000000000 it to convertbillion of hectares to m2 



# ###  classify (for raster version)
# 
# stand_age_class <- terra::classify(stand_age$ForestAge_TC010 , c(0, 20, 100, Inf), others = NA)
# stand_age_class <- stand_age_class %>% terra::mask(SYNMAP) # mask to where SYNMAP is, because we alreyad subseted that for trees)
# 
# 
# ### get areas
# stand_age_area <- freq(stand_age_class)
# 
# stand_age_area$Area <- stand_age_area$count * res(stand_age_class)[1]
# units(stand_age_area$Area) <- units(dom_veg_areas$Area)
# 
# stand_age_area$stand_age <- c("<20 yrs", "20-100 yrs", ">100 yrs" )


## FAO_ecozone ####

FAO_ecozone <- FAO_ecozone %>% st_intersection(SYNMAP) # mask to where SYNMAP is, because we alreyad subseted that for trees)
FAO_ecozone <- FAO_ecozone %>% 
  mutate(gez_abbrev_group = case_when(gez_abbrev %in% c("SCf", "TeM", "TAr", 
                                                        "Ba", "TeDc", "SM", 
                                                        "TAwa", "TM", "TeDo"
  ) ~ gez_abbrev,
  .default = "Other"))

### get areas
sf::sf_use_s2(FALSE)
FAO_ecozone_areas <- FAO_ecozone %>% mutate(Area = st_area(.))  %>% group_by(gez_abbrev_group) %>% summarize(Area = sum(Area)) %>% st_drop_geometry()
FAO_ecozone_areas$Area


## Continents ####

continents <- continents %>% st_intersection(SYNMAP) # mask to where SYNMAP is, because we alreyad subseted that for trees)


### get areas
sf::sf_use_s2(FALSE)
continents_areas <- continents %>% mutate(Area = st_area(.))  %>% group_by(CONTINENT) %>% summarize(Area = sum(Area)) %>% st_drop_geometry()
continents_areas$Area


# rename columns in  objests
dom_veg_areas <- dom_veg_areas %>% rename(name = dom_veg_group)
FAO_ecozone_areas <- FAO_ecozone_areas %>% rename(name = gez_abbrev_group)
continents_areas <- continents_areas %>% rename(name = CONTINENT)
# stand_age_area <- stand_age_area %>% rename(name = stand_age) %>% dplyr::select(name, Area) # for vector version


# save areas as object that we can bring somewhere else
save(list = c("dom_veg_areas",  "FAO_ecozone_areas", "continents_areas", "stand_age_area"), file = "scripts/X_area_of_forest_categories_for_barplot.RData")
