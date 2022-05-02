#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Update: 01-05-2022
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
rm(list=ls())
require(pacman) # require pacman
p_load(tidyverse,rio,sf,leaflet,viridis,osmdata,ggsn) # require and/or install packages

#==== Hoy veremos ====# 

## 1. Manipulating Simple Feature Geometries
   ## 1.1. Affine transformations
   ## 1.2. Cliping data
   ## 1.3. Buffer
   ## 1.4. Joining two feature sets based on geometries
   ## 1.5. Centroid   
   ## 1.6. Distances
## 2. Aplications: 

#==== Gen data ====#

# make points
housing = tibble(cod_apto=c("101","102","103","104"),
                 lat=c(4.676268724014856,4.6779544376424065,4.677911354610968,4.676429959532716),
                 lon=c(-74.05081358548767,-74.04782427039443,-74.05049129066947, -74.04636785215544)) 
housing = st_as_sf(x=housing,coords=c("lon","lat"),crs=4326)

leaflet() %>% addTiles() %>% addCircleMarkers(data=housing)

# get amenities
chico = getbb(place_name = "UPZ Chicó Lago Bogotá Colombia",featuretype = "boundary:administrative",format_out = "sf_polygon")
bar = opq(bbox = st_bbox(chico)) %>%
      add_osm_feature(key = "amenity", value = "bar") %>%
      osmdata_sf() %>% .$osm_points
leaflet() %>% addTiles() %>% addPolygons(data = chico)
leaflet() %>% addTiles() %>% addCircleMarkers(data=bar)

# gen lines
streets = opq(bbox = st_bbox(housing)) %>%
          add_osm_feature(key = "highway") %>%
          osmdata_sf() %>% .$osm_lines

leaflet() %>% addTiles() %>% addPolylines(data=streets ,col="red")

# gen polygons
park = getbb(place_name = "Parque de la 93",
             featuretype = "amenity",
             format_out = "sf_polygon") %>% mutate(name="Parque de la 93")

leaflet() %>% addTiles() %>% addPolygons(data=park)

# save data
# save(housing,streets,park,bar,file = "input/osm_data.rds")
# rm(list=ls())
# load("input/osm_data.rds")

#=============================================#
# [1.] Manipulating Simple Feature Geometries #
#=============================================#

## Help
vignette("sf3")
vignette("sf4")

## load data
load("input/mhv_blocks_bog.rds")
leaflet() %>% addTiles() %>% addPolygons(data=mhv_bog)

##=== 1.1. Affine transformations ===##
st_crs(mhv_bog)
st_crs(housing)
housing = st_transform(x=housing , crs=st_crs(mhv_bog))
st_crs(housing)

##=== 1.2. Cliping data ===##

# bares 
leaflet() %>% addTiles() %>% addCircleMarkers(data=bar,col="blue") %>% addCircleMarkers(data=housing,col="red")

bar_housing = st_crop(x=bar , y=st_bbox(housing))

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_housing,col="blue") %>% addCircleMarkers(data=housing,col="red")

# mhv
mhv_housing = st_crop(x=mhv_bog , y=st_bbox(housing))

leaflet() %>% addTiles() %>% addPolygons(data=mhv_housing,col="blue") %>% addCircleMarkers(data=housing,col="red")

##=== 1.3. Buffer ===##
mhv_housing_bf = st_buffer(x=mhv_housing , dist=10)

leaflet() %>% addTiles() %>% addPolygons(data=mhv_housing_bf,col="green") %>% addCircleMarkers(data=housing,col="red")

##=== 1.4. Joining two feature sets based on geometries ===##
leaflet() %>% addTiles() %>% 
addPolygons(data=mhv_housing_bf,col="green" , label=mhv_housing_bf$MANZ_CCNCT) %>% 
addCircleMarkers(data=housing , col="red" , label=housing$cod_apto)

housing
housing = st_join(x=housing , y=mhv_housing_bf)
housing

##=== 1.5. Centroid ===##
c_park = st_centroid(x = park)

leaflet() %>% addTiles() %>% addCircleMarkers(data=c_park,col="blue") %>% addCircleMarkers(data=housing,col="red")

##=== 1.6. Distances ===##

## Distances to park
leaflet() %>% addTiles() %>% addCircleMarkers(data=c_park,col="blue") %>% addCircleMarkers(data=housing,col="red")

dist_park = st_distance(x=housing , y=c_park)

housing$dist_park = dist_park

housing

## Distances to bar
leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_housing,col="blue") %>% addCircleMarkers(data=housing,col="red")

dist_bar = st_distance(x=housing , y=bar_housing)
dist_bar

min_dist = apply(dist_bar , 1 , min)
min_dist
housing$dist_bar = min_dist
housing

## Distance to street
foot_way = subset(streets,highway="foot_way")
leaflet() %>% addTiles() %>% addPolylines(data=foot_way,col="blue") %>% addCircleMarkers(data=housing,col="red")

dist_street = st_distance(x=housing , y=foot_way)
dist_street

min_dist = apply(dist_street , 1 , min)
min_dist
housing$dist_street = min_dist
housing

## save data 
export(housing,"output/housing.rds")

#==================#
# [2.] Aplications #
#==================#

# clean environment
rm(list=ls())

# load data
load("input/data_aplication_mhv_bogota.rds")

# get boundary
layer = getbb(place_name = "Bogotá Colombia",
              featuretype = "boundary:administrative",
              format_out = "sf_polygon")
layer = layer$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data=layer)

#=== 2.1 Affine transformations ===#

# change crs
layer = st_transform(layer , crs = st_crs(mhv_bog))
ips = st_transform(ips , crs = st_crs(mhv_bog))
bar = st_transform(bar , crs = st_crs(mhv_bog))
street = st_transform(street , crs = st_crs(mhv_bog))

#=== 2.2 Cliping data ===#

# cliping
ips_1 = st_crop(ips,layer) # Option 1 
ips_2 = st_intersection(ips,layer) # Option 2
ips = ips[layer,] # Option 3

# view data
leaflet() %>%
addTiles() %>%
addCircleMarkers(data=ips , label=ips$nombre_prestador)

#=== 2.3 Centroid ===#

# change geometry
point = st_centroid(x = mhv_bog , of_largest_polygon = T)

#=== 2.4 Buffer ===#

# Buffer
#buff = st_buffer(x = point , dist = 200)
buff = st_read("input/a.gpkg") %>% select(MANZ_CCNCT)

# view data
leaflet() %>%
addTiles() %>%
addPolygons(data=buff[1,]) %>%
addCircleMarkers(data=point,col="red")

#=== 2.5 Joining two feature sets based on geometries ===#

# Add MHV neighborhood
buff = st_join(buff,point)

st_geometry(buff) = NULL

buff = buff %>% 
       group_by(MANZ_CCNCT) %>%
       summarise(n_mhv = mean(mhv))

#=== 2.6 Distances ===#

# distance to ips
dist_ips = st_distance(point,ips)
min_dist_ips = apply(X = dist_ips , MARGIN = 1 , mean)
point$dist_ips = min_dist_ips

# distance to bar
dist_ameni = st_distance(point,bar)
min_dist_ameni = apply(X = dist_ameni , MARGIN = 1 , min)
point$dist_ameni = min_dist_ameni

# distance to main street  
dist_street = st_distance(point,street)
min_dist_street = apply(X = dist_street , MARGIN = 1 , min)
point$dist_street = min_dist_street

#=== 2.7. Join data ===#

# remove geometry
st_geometry(point) = NULL

# join data
point = point %>% select(MANZ_CCNCT,mhv,dist_ips,dist_ameni,dist_street)

df = left_join(point,buff,"MANZ_CCNCT") %>% as_tibble()
df

# export data
export(df,"output/mhv_bog.rds")





