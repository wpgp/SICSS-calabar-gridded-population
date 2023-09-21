library(tidyverse)
library(tmap)

tmap_mode('view')
tmap_options(check.and.fix = TRUE) 
sf::sf_use_s2(F)

# Defining the study area -------------------------------------------------
library(sf)

lga <- st_read('data/GRID3_Nigeria_-_Local_Government_Area_Boundaries/GRID3_Nigeria_-_Local_Government_Area_Boundaries.shp')

lga %>% 
  st_drop_geometry() %>% 
  View()

tm_shape(lga)+
  tm_polygons()

lga_Calabar <- lga %>% 
  filter(lga_name_x=='Calabar South')


tm_shape(lga_Calabar)+
  tm_borders(col='orange', lwd=5)+
  tm_shape(lga)+
  tm_borders()+
  tm_basemap('OpenStreetMap')

# Discovering the health facilities dataset -------------------------------


health_facilities <- st_read('data/GRID3_Nigeria_-_Health_Care_Facilities_/GRID3_Nigeria_-_Health_Care_Facilities_.shp')



# exploration

health_facilities %>% 
  st_drop_geometry() %>% 
  View()

health_facilities_Calabar <- health_facilities %>% 
  filter(lga_name=='Calabar South')


tm_shape(health_facilities_Calabar)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(lga_Calabar)+
  tm_borders(lwd=4)


# EXERCISE: How many health facilities are offering tertiary services in Calabar South?

# EXERCISE: What are the services that health facilities are offering in Calabar South?

# Discovering the gridded population dataset ------------------------------

library(terra)

pop <- rast('data/NGA_population_v2_0_gridded/NGA_population_v2_0_gridded.tif')

pop_Calabar <- crop(pop, lga_Calabar)
plot(pop_Calabar)

pop_Calabar <- mask(pop_Calabar, lga_Calabar)
plot(pop_Calabar)

tm_shape(health_facilities_Calabar)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(lga_Calabar)+
  tm_borders(lwd=4)



# Buffering points --------------------------------------------------------


library(units)

health_facilities_Calabar <- st_transform(health_facilities_Calabar, 'epsg:26393')
pop_Calabar <- project(pop_Calabar, 'epsg:26393')

health_facilities_Calabar_buffered <- st_buffer(health_facilities_Calabar, dist=set_units(1, km))

health_facilities_Calabar[1,] 


tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(health_facilities_Calabar_buffered[1,])+
  tm_borders()+
  tm_shape(health_facilities_Calabar[1,])+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')



# Computing the population ------------------------------------------------


health_facilities_Calabar_pop <- extract(pop_Calabar, health_facilities_Calabar_buffered, fun=sum, na.rm=T,df=T)

health_facilities_Calabar_buffered$pop <- health_facilities_Calabar_pop$NGA_population_v2_0_gridded

#EXERCISE: How many people are living in 1km of Calabar Anantigha Primary Health Care?


summary(health_facilities_Calabar_buffered$pop)
hist(health_facilities_Calabar_buffered$pop, breaks=20)

tm_shape(health_facilities_Calabar_buffered)+
  tm_fill('pop', style='pretty', id='pop')+
  tm_shape(health_facilities_Calabar)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

tm_shape(health_facilities_Calabar_buffered %>% 
           filter(pop<10000))+
  tm_fill( id='pop', alpha=0.5, col='grey20')+
  tm_shape(pop_Calabar)+
  tm_raster()+
  tm_basemap('OpenStreetMap')

#  How many people are not covered by health facilities? ------------------

health_facilities_Calabar_buffered_rasterized <- rasterize(health_facilities_Calabar_buffered, pop_Calabar, field=1)
plot(health_facilities_Calabar_buffered_rasterized)

pop_Calabar_masked <- mask(pop_Calabar, health_facilities_Calabar_buffered_rasterized)
plot(pop_Calabar_masked)

sum(pop_Calabar_masked[], na.rm=T)

#EXERCISE: How many people are not living in a 1km of an health facility?



# How many are not covered by a maternity home? ---------------------------


tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(health_facilities_Calabar)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_shape(health_facilities_Calabar %>% 
             filter(category=='Maternity Home'))+
  tm_dots(col='darkgreen', size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

# EXERCISE: How many maternity homes are listed in the LGA?


# EXERCISE: How many people are not living in a 1km distance of a maternity center?



# What is the furthest a woman has to travel to reach a maternity? --------
health_facilities_Calabar_maternity <- health_facilities_Calabar %>% 
  filter(category=='Maternity Home')

health_facilities_Calabar_maternity_distance <- distance(pop_Calabar, health_facilities_Calabar_maternity)
plot(health_facilities_Calabar_maternity_distance)


health_facilities_Calabar_maternity_distance_pop <- mask(health_facilities_Calabar_maternity_distance, pop_Calabar)
plot(health_facilities_Calabar_maternity_distance_pop)

summary(health_facilities_Calabar_maternity_distance_pop[])


#EXERCISE: How many people are living at more than 5km from a maternity?



# And how many woman of childbearing age?... -----------------------------------------------------

women <- rast('data/NGA_population_v2_0_agesex/NGA_population_v2_0_agesex_f15_49.tif')

#EXERCISE: How many women of childbearing age are living at more than 5km from a maternity?



# What about the number of women of childbearing age living at more than 5km from a maternity in the country?
pop <- project(pop, 'epsg:26393')
health_facilities <- st_transform(health_facilities, 'epsg:26393')
lga <- st_transform(lga, 'epsg:26393')


health_facilities_maternity <- health_facilities %>% 
  filter(category=='Maternity Home')
health_facilities_maternity_buffered5km <- st_buffer(health_facilities_maternity, dist=set_units(5, km))
health_facilities_maternity_buffered5km_rasterized <- rasterize(health_facilities_maternity_buffered5km, pop, field=1)
pop_masked_maternity_5km <- mask(pop, health_facilities_maternity_buffered5km_rasterized, inverse=T)

tm_shape(pop_masked_maternity_5km)+
  tm_raster()+
  tm_shape(health_facilities_maternity_buffered5km)+
  tm_borders(col='yellow')+
  tm_basemap('CartoDB.DarkMatter')

lga_pop_masked_maternity_5km <- extract(pop_masked_maternity_5km, lga, fun=sum, na.rm=T,df=T)

lga$nonCovered_women <- lga_pop_masked_maternity_5km$NGA_population_v2_0_gridded
lga$nonCovered_women_perc <- round(lga$nonCovered_women / lga$mean * 100, 2)

summary(lga$nonCovered_women_perc)

tm_shape(lga)+
  tm_polygons(col='nonCovered_women_perc', id='nonCovered_women_perc', title='%women per LGA')+
  tm_shape(health_facilities_maternity)+
  tm_dots(size=0.01, legend.show = T)+
  tm_add_legend(type='symbol', labels='Maternity', col='black')+
  tm_layout( main.title = 'Women of childbearing age at more than 5km from a maternity')
