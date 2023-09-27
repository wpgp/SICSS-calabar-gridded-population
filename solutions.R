library(tidyverse) # package for table wrangling
library(tmap) # package for interactive mapping
library(sf) # package for vector wrangling
library(terra) # package for raster wrangling
library(units) # package to define measurement units (km, m, ...)

tmap_mode('view') # set up interactive mapping as default
tmap_options(check.and.fix = TRUE) # correct errors in polygon boundaries
sf::sf_use_s2(F)

# 1. Defining the study area -------------------------------------------------

# We load the LGA boundaries
lga <- st_read('data/GRID3_Nigeria_-_Local_Government_Area_Boundaries/GRID3_Nigeria_-_Local_Government_Area_Boundaries.shp')

# We look at the attributes of the LGA boundaries
lga %>% 
  st_drop_geometry() %>% 
  View()

# We plot interactively the LGA boundaries
tm_shape(lga)+
  tm_polygons()

# We subset Calabar South LGA boundary
lga_Calabar <- lga %>% 
  filter(lga_name_x=='Calabar South')

# We plot interactively Calabar South boundary
tm_shape(lga_Calabar)+
  tm_borders(col='orange', lwd=5)+ # we define border color and weight
tm_shape(lga)+
  tm_borders()+
  tm_basemap('OpenStreetMap') # we use as basemap/background map OpenStreetMap

# 2. Discovering the health facilities dataset -------------------------------

# We load the health facilities point dataset
health_facilities <- st_read('data/GRID3_Nigeria_-_Health_Care_Facilities_/GRID3_Nigeria_-_Health_Care_Facilities_.shp')


# We look at their attributes
health_facilities %>% 
  st_drop_geometry() %>% 
  View()

# We subset the health facilities from Calabar South
health_facilities_Calabar <- health_facilities %>% 
  filter(lga_name=='Calabar South')

# EXERCISE: How many health facilities in Calabar South?
# ANSWER: We look at the size of the subset
dim(health_facilities_Calabar) # 74

# We plot interactively the health facilities
tm_shape(health_facilities_Calabar)+
  tm_dots(col='type', # colormap per facility type
          size=0.07, id='primary_na', # window on hoover with the primary_na
          popup.vars=c('category','functional','source'))+ # specify popup window information
  tm_basemap('OpenStreetMap')+
  tm_shape(lga_Calabar)+
  tm_borders(lwd=4)


# EXERCISE: How many health facilities are offering tertiary services in Calabar South?
# ANSWER: We tabulate the `type` attribute from the health facilities dataset
table(health_facilities_Calabar$type)

# EXERCISE: What are the services that health facilities are offering in Calabar South?
# ANSWER: We tabulate the `category` attribute from the health facilities dataset
table(health_facilities_Calabar$category)

# 3. Discovering the gridded population dataset ------------------------------

# We load the gridded population
pop <- rast('data/NGA_population_v2_0_gridded/NGA_population_v2_0_gridded.tif')

# We subset the gridded pop that is contained in Calabar South
pop_Calabar <- crop(pop, lga_Calabar)
pop_Calabar <- mask(pop_Calabar, lga_Calabar)
plot(pop_Calabar)

# We plot interactively the health facilities on top of the gridded population
tm_shape(health_facilities_Calabar)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(lga_Calabar)+
  tm_borders(lwd=4)



# 4. Buffering points --------------------------------------------------------

# We convert the spatial coordinate system such that they are more precise for the specific location
health_facilities_Calabar <- st_transform(health_facilities_Calabar, 'epsg:26393')
pop_Calabar <- project(pop_Calabar, 'epsg:26393')

# We buffer all health facilities with a 1km window 
health_facilities_Calabar_buffered <- st_buffer(health_facilities_Calabar, 
                                                dist=set_units(1, km))

# We visualise interactively one health facility and its buffer
tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(health_facilities_Calabar_buffered[1,])+ # select first observation
  tm_borders()+
  tm_shape(health_facilities_Calabar[1,])+ # select corresponding health facility
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')


# 5. Computing the population ------------------------------------------------

# We compute the population count for all health facility buffers
health_facilities_Calabar_pop <- extract(pop_Calabar, # gridded population
                                         health_facilities_Calabar_buffered,# buffers
                                         fun=sum, # aggregate by summing up all grid cells
                                         na.rm=T, # remove grid cells that have no population
                                         df=T)

# assign results to a column in the health facility buffer dataset
health_facilities_Calabar_buffered$pop <- health_facilities_Calabar_pop$NGA_population_v2_0_gridded

#EXERCISE: How many people are living in 1km of Calabar Anantigha Primary Health Care?
#ANSWER: We select from the health facilities buffer the relevant health center and select the population attribute
# 13463

health_facilities_Calabar_buffered |> 
  st_drop_geometry() |> # remove the vector attribute for readibility
  filter(primary_na =='Anantigha Primary Health Care')|> # filter the correct health care center
  select(c(primary_na, pop)) # select the `pop` column

# We look at the distribution of the number of people in each health facility buffer
summary(health_facilities_Calabar_buffered$pop)
hist(health_facilities_Calabar_buffered$pop, breaks=20)

# We plot interactively the health facilities buffer and their corresponding population total
tm_shape(health_facilities_Calabar_buffered)+
  tm_fill('pop', style='pretty', id='pop')+
  tm_shape(health_facilities_Calabar)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

# We zoom on the buffer that have the least number of people
tm_shape(health_facilities_Calabar_buffered %>% 
           filter(pop<10000))+ # filter the least populated health facility buffers
  tm_fill( id='pop', alpha=0.5, col='grey20')+
  tm_shape(pop_Calabar)+
  tm_raster()+
  tm_basemap('OpenStreetMap')
# We can see an artefact of spatial analysis so called the boundary effect: 
# this health facility has the least people from Calabar South but probably 
# people from Calabar are also visiting it

# 6. How many people are not covered by a health facility? ------------------

# We convert to grid cells the health facilities buffer
health_facilities_Calabar_buffered_rasterized <- rasterize(health_facilities_Calabar_buffered, 
                                                           pop_Calabar, # we use the population raster as a formatting model
                                                           field=1 # we assign the value 1 to all grid cells falling in a buffer 
                                                           )
plot(health_facilities_Calabar_buffered_rasterized)

# We subset all the populated grid cells that falls into at least one health facility buffer
pop_Calabar_masked <- mask(pop_Calabar, health_facilities_Calabar_buffered_rasterized)
plot(pop_Calabar_masked)

# We compute the number of people that are within a health facility buffer 
# by summing up the previous population subset
sum(pop_Calabar_masked[], na.rm=T)

#EXERCISE: How many people are not living in 1km of a health facility?

# first method:
# We compute the number of people living in Calabar South and 
# substract the number of people living at 1km or less than a health facility
sum(pop_Calabar[], na.rm=T) - sum(pop_Calabar_masked[], na.rm=T)

# second method:
# We create a raster of people not covered and we sum up all of its grid cells
pop_Calabar_notcovered <- sum(pop_Calabar, -pop_Calabar_masked, na.rm=T) # we use the sum function from the terra package
sum(pop_Calabar_notcovered[], na.rm=T)

# We plot interactively the gridded population representing people living at more than 1 km from a health facility
tm_shape(pop_Calabar_notcovered)+
  tm_raster()+
  tm_shape(health_facilities_Calabar)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')
# We see that they are located at the outskirt of the city and at the very remote tip of the LGA

# 7. How many are not covered by a maternity home? ---------------------------

# We reproduce the same analysis focusing on maternity homes

# We plot interactively the maternity homes
tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(health_facilities_Calabar)+ # all facilities
  tm_dots( size=0.08, id='primary_na', 
           popup.vars=c('category','functional','source'))+ 
  tm_shape(health_facilities_Calabar %>% 
             filter(category=='Maternity Home'))+ # maternity homes
  tm_dots(col='darkgreen', size=0.08, id='primary_na', # in green
          popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

# EXERCISE: How many maternity homes are listed in the LGA?
# ANSWER: We filter the maternity homes and see how many rows it contains
health_facilities_Calabar_buffered_maternity <- health_facilities_Calabar_buffered %>% 
  filter(category=='Maternity Home')
nrow(health_facilities_Calabar_buffered_maternity)

# We plot interactively with the selected buffered health facilities
tm_shape(health_facilities_Calabar_buffered_maternity )+
  tm_fill( id='pop', alpha=0.5, col='grey20')+
  tm_shape(pop_Calabar)+
  tm_raster()+
  tm_basemap('OpenStreetMap')
# We can already visualise the areas that are not covered by a maternity home

# EXERCISE: How many people are not living in a 1km distance of a maternity center?
# ANSWER: 
# 1. We convert to grid cells the buffered maternity homes
health_facilities_Calabar_buffered_maternity_rasterized <- rasterize(health_facilities_Calabar_buffered_maternity, pop_Calabar, field=1)
plot(health_facilities_Calabar_buffered_maternity_rasterized)

# 2. We extract the population living in those buffers
pop_Calabar_masked_maternity <- mask(pop_Calabar, health_facilities_Calabar_buffered_maternity_rasterized)
plot(pop_Calabar_masked_maternity)

# 3. We substract from the LGA population the sum of the population living in those buffers
sum(pop_Calabar[], na.rm=T) - sum(pop_Calabar_masked_maternity[], na.rm=T)



# 8. What is the furthest a woman has to travel to reach a maternity? --------

# We filter the health facilities point that are maternity homes
health_facilities_Calabar_maternity <- health_facilities_Calabar %>% 
  filter(category=='Maternity Home')

# We compute the distance from each grid cell to those points
health_facilities_Calabar_maternity_distance <- distance(pop_Calabar, # define the grid cell format
                                                         health_facilities_Calabar_maternity)
plot(health_facilities_Calabar_maternity_distance)

# We subtract from this distance raster only the populated grid cells
health_facilities_Calabar_maternity_distance_pop <- mask(health_facilities_Calabar_maternity_distance, pop_Calabar)
plot(health_facilities_Calabar_maternity_distance_pop)

# We compute the maximum people have to travel to reach a maternity
max(health_facilities_Calabar_maternity_distance_pop[], na.rm=T)


#EXERCISE: How many people are living at more than 5km from a maternity?
#ANSWER: 
# 1. We need to compute new 5km buffer window only for the maternity homes point
health_facilities_Calabar_maternity_buffered5km <- st_buffer(health_facilities_Calabar_maternity, 
                                                             dist=set_units(5, km))
# 2. We rasterize the buffered maternity homes
health_facilities_Calabar_maternity_buffered5km_rasterized <- rasterize(health_facilities_Calabar_maternity_buffered5km, pop_Calabar, field=1)
plot(health_facilities_Calabar_maternity_buffered5km_rasterized)

# 3. We remove from the population those living in the buffered maternity homes
pop_Calabar_masked_maternity_5km <- mask(pop_Calabar, 
                                         health_facilities_Calabar_maternity_buffered5km_rasterized,
                                         inverse=T) # this remove the population inside the buffer
plot(pop_Calabar_masked_maternity_5km)

# 4. We substract from the LGA population the sum of the population living in those buffers
sum(pop_Calabar_masked_maternity_5km[], na.rm=T)



# 9. And what about woman of childbearing age? -----------------------------------------------------

# We load the gridded population focused on women between the age of 15 and 49
women <- rast('data/NGA_population_v2_0_agesex/NGA_population_v2_0_agesex_f15_49.tif')

#EXERCISE: How many women of childbearing age are living at more than 5km from a maternity?
# ANSWER

# 1. We substract the women living in Calabar
women_Calabar <- crop(women, lga_Calabar)
women_Calabar <- mask(women_Calabar, lga_Calabar)
# 2. We change the coordinate system
women_Calabar <- project(women_Calabar, 'epsg:26393')

# 3. We remove the womean living in 5km or less of a maternity home
women_Calabar_masked_maternity_5km <- mask(women_Calabar, 
                                           health_facilities_Calabar_maternity_buffered5km_rasterized, 
                                           inverse=T)

# 4. We sum up the number of women living further than 5km from a maternity home
sum(women_Calabar_masked_maternity_5km[], na.rm=T)


# What about the number of women of childbearing age living at more than 5km from a maternity in the country?

# We need to prepare the data from the entire country
# WARNING: this section takes time because Nigeria is a huge country and thus a lot of grid cels

# We change the coordinate system
women <- project(women, 'epsg:26393') # this is very long 
health_facilities <- st_transform(health_facilities, 'epsg:26393')
lga <- st_transform(lga, 'epsg:26393')

# We filter the maternity from the health facilities
health_facilities_maternity <- health_facilities %>% 
  filter(category=='Maternity Home')
# We buffer the maternity homes by a 5km window to define maternity homes catchment areas
health_facilities_maternity_buffered5km <- st_buffer(health_facilities_maternity, 
                                                     dist=set_units(5, km))
# We convert to grid cells the maternity home buffer
health_facilities_maternity_buffered5km_rasterized <- rasterize(health_facilities_maternity_buffered5km, women, field=1)
# We remove the population the people living at 5km or less than a maternity
women_outside_maternity_5km <- mask(women, 
                                 health_facilities_maternity_buffered5km_rasterized, 
                                 inverse=T)

# We plot interactively the buffered maternity and the population not covered by a maternity
tm_shape(women_outside_maternity_5km)+
  tm_raster()+
  tm_shape(health_facilities_maternity_buffered5km)+
  tm_borders(col='yellow')+
  tm_basemap('CartoDB.DarkMatter')

# We sum up by lga the number of people not covered by a maternity
lga_women_masked_maternity_5km <- extract(women_outside_maternity_5km, lga, fun=sum, na.rm=T,df=T)
lga$nonCovered_women <- lga_women_masked_maternity_5km$NGA_population_v2_0_agesex_f15_49

# We compute the number of women per lga
lga$women <- extract(women, lga, fun=sum, na.rm=T,df=T)$NGA_population_v2_0_agesex_f15_49

# We compute the percentage of women that live further than 5km away of a maternity home
lga$nonCovered_women_perc <- round(lga$nonCovered_women / lga$women * 100, 2)

# We visualise the indicator of women access to maternal care per lga

tmap_mode('plot') # we change to static plot for easier export

tm_shape(lga)+
  tm_polygons(col='nonCovered_women_perc', id='nonCovered_women_perc', title='% women per LGA \n >5km of a maternity')+
  tm_shape(health_facilities_maternity)+ # add the maternity homes location
  tm_dots(size=0.01, legend.show = T)+
  tm_add_legend(type='symbol', labels='Maternity', col='black')+ 
  tm_layout( main.title = 'Access to maternal health care for 15-49 years old women')

# 10. Misc: how to add an image on an interactive plot

# We add an attribute with the image source and the html format
health_facilities_Calabar$link <- "<img src = https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Queen_Elizabeth_Hospital_Birmingham%2C_Edgbaston%2C_Birmingham%2C_England-7March2011.jpg/320px-Queen_Elizabeth_Hospital_Birmingham%2C_Edgbaston%2C_Birmingham%2C_England-7March2011.jpg>"

tm_shape(pop_Calabar)+
  tm_raster()+
  tm_shape(health_facilities_Calabar_buffered[1,])+
  tm_borders()+
  tm_shape(health_facilities_Calabar[1,])+
  tm_dots( size=0.08, id='primary_na', 
           popup.vars=c('category','functional','source', 'link'), # we add the img attribute to be represented
           popup.format = list(html.escape = F))+ # we need to tell tmap to show html code as it is
  tm_basemap('OpenStreetMap')

# Now when you click on the point you see the image