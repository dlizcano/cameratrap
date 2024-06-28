
library(reticulate)
library(R6)
library(processx)

library(rgee)
# ee_install(py_env = "rgee")  

# ee_Initialize(user = 'csaybar@gmail.com') # Use the argument email is not mandatory, but it's helpful to change of EE user.
# Initialize Earth Engine and GD
# ee_Initialize(user = 'csaybar@gmail.com', drive = TRUE)
# Initialize Earth Engine and GCS
# ee_Initialize(user = 'csaybar@gmail.com', gcs = TRUE)
# Initialize Earth Engine, GD and GCS
ee_Initialize(user = 'dj.lizcano@gmail.com', drive = TRUE, gcs = FALSE)
# 1. Initialize the Python Environment
ee_Initialize()
ee_check() # Check non-R dependencies


# 2. Install geemap in the same Python ENV that use rgee
# py_install("geemap")
gm <- import("geemap")

createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L) 
  ee$Image(year)$byte()$addBands(img)
}

# Use your TimeBand function to map it over the night-time lights collection.

collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)

# Compute a linear fit over the series of values at each pixel, so that you can visualize the y-intercept as green, and the positive/negative slopes as red/blue.

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)

# Let's visualize our map!

Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)



################

#Land Cover  100m global landcover dataset.
# https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_Landcover_100m_Proba-V-C3_Global#bands

LC_dataset <- ee$Image("COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019")$select('discrete_classification')
# plot the layer to see what it looks like before moving on: 
Map$setCenter(-88.6, 26.4, 1);
Map$addLayer(LC_dataset, {}, "Land Cover");

#### extract using sf object
habs <- ee_extract(LC_dataset, datos_PCF_sf) # datos_PCF_sf  coming from cameras
# Read in the LCC key
codes <- read.csv("C:/CodigoR/CameraTrapCesar/data/LCC_codes.csv")

# Use left join to label the codes
habs <- left_join(habs, codes)

# And update the locs_sf dataframe
datos_PCF_sf$lcc_habitats <- habs$hab_code

######################
# Let’s repeat this process with a continuous variable like biomass. We will use the Global Aboveground and Belowground Biomass Carbon Density Maps.

biomass <- ee$ImageCollection("NASA/ORNL/biomass_carbon_density/v1");

# Specify the layer we want to plot
bio = biomass$first()

# Set up the colors for the visualization
viz <- list(
  bands="agb",
  max = 129,
  min = 0,
  palette = c('#0c0c0c', '#071aff', '#ff0000', '#ffbd03', '#fbff05', '#fffdfd')
)

# Plot the map
Map$addLayer(eeObject = bio, visParams = viz)
# Extract the data using ee_extract:
tmp <- ee_extract(biomass, datos_PCF_sf)
# paste to datos_PCF_sf
datos_PCF_sf$biomass_agb <- tmp$X2010_agb

#############################
# Human Modification Index
#############################

hmi <- ee$ImageCollection("CSP/HM/GlobalHumanModification")

# select the first image of the collection which corresponds to the cumulative HM  
hm = hmi$first()
viz <- list(
  bands="gHM",
  max = 1,
  min = 0,
  palette = c('#0c0c0c', '#071aff', '#ff0000', '#ffbd03', '#fbff05', '#fffdfd')
)

Map$addLayer(eeObject = hm, visParams = viz)


######################################################
# WDPA: World Database on Protected Areas
######################################################

gpa  <- ee$FeatureCollection("WCMC/WDPA/current/polygons")
# Plot it (note you have to zoom in to see the data)
Map$addLayer(gpa, {}, "default display") 














