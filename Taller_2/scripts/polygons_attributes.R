
#clean global environment
rm(list = ls() )

# Load the required packages
require(pacman)

p_load(R.utils,
       rgdal,
       tidyverse,
       leaflet,
       sp,
       sf,
       rgbif,
       DBI)

# set working directory
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
setwd("../stores")
getwd()

#-------------------------------------------------------------------
#                     prepare the environment
#-------------------------------------------------------------------

# data load: this data has coordinates points
properties <- read.csv("cleandata.csv")

# Convert the spatial point data to an sf object
properties <- st_as_sf(
  properties, 
  coords = c("lon","lat"), # "coords" is in x/y order -- so longitude goes first
  crs = 4326  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
)

#bogota's geographical interesting points:
points_ngeo <- sf::st_read("ngeo.shp", 
                           promote_to_multi = TRUE)

#bogota's stratification by blocks polygons:
poly_blocks <- sf::st_read("ManzanaEstratificacion.shp", 
                           promote_to_multi = TRUE)


#bogota's sectors by blocks polygons:
poly_sector <- sf::st_read("sector.shp", 
                           promote_to_multi = TRUE)

#bogota's localidades by blocks polygons:
poly_localidad <- sf::st_read("Loca.shp", 
                              promote_to_multi = TRUE)

#bogota's crimes by blocks polygons:
poly_daiupz <- sf::st_read("daiupz.shp", 
                           promote_to_multi = TRUE)

#bogota´s weighted average floor by blocks
poly_floors <- sf::st_read("pisos2022.shp", 
                           promote_to_multi = TRUE)


# check the CRS of all objects
print(st_crs(properties))
print(st_crs(poly_blocks))
print(st_crs(poly_sector))
print(st_crs(poly_localidad))
print(st_crs(poly_daiupz))
print(st_crs(poly_floors))
print(st_crs(points_ngeo))


# copy the CRS to the objects
st_crs(poly_blocks) <- st_crs(properties)
st_crs(poly_sector) <- st_crs(properties)
st_crs(poly_localidad) <- st_crs(properties)
st_crs(poly_daiupz) <- st_crs(properties)
st_crs(poly_floors) <- st_crs(properties)
st_crs(points_ngeo) <- st_crs(properties)


#retry the transformation
poly_blocks <- st_transform(poly_blocks, crs = st_crs(properties))
poly_sector <- st_transform(poly_sector, crs = st_crs(properties))
poly_localidad <- st_transform(poly_localidad, crs = st_crs(properties))
poly_daiupz <- st_transform(poly_daiupz, crs = st_crs(properties))
poly_floors <- st_transform(poly_floors, crs = st_crs(properties))
points_ngeo <- st_transform(points_ngeo, crs = st_crs(properties))

#verify the objects' classes
class(poly_blocks)
class(properties)
class(poly_localidad) 
class(poly_daiupz)
class(poly_floors)
class(points_ngeo)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check for invalid geometries in properties
invalid_properties <- properties[!st_is_valid(properties), ]

# Check for invalid geometries in poly_blocks
invalid_poly_blocks <- poly_blocks[!st_is_valid(poly_blocks), ]


invalid_poly_sector <- poly_blocks[!st_is_valid(poly_sector), ]


invalid_poly_localidad <- poly_blocks[!st_is_valid(poly_localidad), ]


invalid_poly_daiupz <- poly_blocks[!st_is_valid(poly_daiupz), ]


invalid_poly_floors <- poly_blocks[!st_is_valid(poly_floors), ]


invalid_points_ngeo <- poly_blocks[!st_is_valid(points_ngeo), ]


# View the invalid geometries (if any)
print(invalid_properties)
print(invalid_poly_blocks)
print(invalid_poly_sector)
print(invalid_poly_localidad)
print(invalid_poly_daiupz)
print(invalid_poly_floors)
print(invalid_points_ngeo)

#Fix Invalid Geometries---------------------------------------------------------
# Fix invalid geometries in properties
properties <- st_make_valid(properties)

# Fix invalid geometries in poly_blocks
poly_blocks <- st_make_valid(poly_blocks)

poly_sector <- st_make_valid(poly_sector)

poly_localidad <- st_make_valid(poly_localidad)

poly_daiupz <- st_make_valid(poly_daiupz)

poly_floors <- st_make_valid(poly_floors)

points_ngeo <- st_make_valid(points_ngeo) 

# List of variables to be removed from poly_daiupz
variables_to_remove <- c(
  "CMH18CONT", "CMH19CONT", "CMH20CONT", "CMHVAR", "CMHTOTAL",
  "CMLP18CONT", "CMLP19CONT", "CMLP20CONT", "CMLPVAR", "CMLPTOTAL",
  "CMHP18CONT", "CMHP19CONT", "CMHP20CONT", "CMHPVAR", "CMHPTOTAL",
  "CMHR18CONT", "CMHR19CONT", "CMHR20CONT", "CMHRVAR", "CMHRTOTAL",
  "CMHA18CONT", "CMHA19CONT", "CMHA20CONT", "CMHAVAR", "CMHATOTAL",
  "CMHB18CONT", "CMHB19CONT", "CMHB20CONT", "CMHBVAR", "CMHBTOTAL",
  "CMHC18CONT", "CMHC19CONT", "CMHC20CONT", "CMHCVAR", "CMHCTOTAL",
  "CMHCE18CON", "CMHCE19CON", "CMHCE20CON", "CMHCEVAR", "CMHCETOTAL",
  "CMHM18CONT", "CMHM19CONT", "CMHM20CONT", "CMHMVAR", "CMHMTOTAL",
  "CMDS18CONT", "CMDS19CONT", "CMDS20CONT", "CMDSVAR", "CMDSTOTAL",
  "CMVI18CONT", "CMVI19CONT", "CMVI20CONT", "CMVIVAR", "CMVITOTAL",
  "CMH21CONT", "CMLP21CONT", "CMHP21CONT", "CMHR21CONT", "CMHA21CONT",
  "CMHB21CONT", "CMHCE21CON", "CMHM21CONT", "CMHC21CONT", "CMDS21CONT",
  "CMVI21CONT", "CMH22CONT", "CMLP22CONT", "CMHP22CONT", "CMHR22CONT",
  "CMHA22CONT", "CMHB22CONT", "CMHCE22CON", "CMHM22CONT", "CMHC22CONT", 
  "CMDS22CONT", "CMVI22CONT", "SHAPE_AREA", "SHAPE_LEN", "CMMES"
  
)

# remove the specified variables
poly_daiupz <- poly_daiupz[, !names(poly_daiupz) %in% variables_to_remove]

# view cleaned data
print(poly_daiupz)
#-------------------------------------------------------------------------------
ngeo_joined <- st_join( points_ngeo,
                        poly_blocks["ESTRATO"],
                        join = st_intersects,
                        left = TRUE,
                        largest = FALSE
)

#-------------------------------------------------------------------------------
properties_joined <- st_join( properties,
                              poly_blocks["ESTRATO"],
                              join = st_intersects,
                              left = TRUE,
                              largest = FALSE
)

#-------------------------------------------------------------------------------
properties_joined <- st_join( properties_joined,
                              poly_sector["SCANOMBRE"],
                              join = st_intersects,
                              left = TRUE,
                              largest = FALSE
)


#-------------------------------------------------------------------------------
properties_joined <- st_join( properties_joined,
                              poly_localidad["LocNombre"],
                              join = st_intersects,
                              left = TRUE,
                              largest = FALSE
)
#-------------------------------------------------------------------------------
properties_joined <- st_join( properties_joined,
                              poly_daiupz,
                              join = st_intersects,
                              left = TRUE,
                              largest = FALSE
)
#-------------------------------------------------------------------------------
properties_joined <- st_join( properties_joined,
                              poly_floors["ALTURA"],
                              join = st_intersects,
                              left = TRUE,
                              largest = FALSE
)


# find the nearest to the apartments from data of interesting bogota's places
nearest_feature_indices <- st_nearest_feature(properties_joined, points_ngeo)

# extract the indices of the nearest features from 'other_points_data'
nearest_indices <- nearest_feature_indices[, "nn"]


# Join the attributes from geographic names
points_data_nearest <- cbind(properties_joined, points_ngeo[nearest_feature_indices, ])

write.csv(properties_joined, file = "properties_joined.csv", row.names = FALSE ) 


cleandata <- read.csv("cleandata.csv")

# Variables to retrieve
variables_to_retrieve <- c("property_id", "ESTRATO", "SCANOMBRE", "LocNombre", 
                           "CMIUUPLA", "CMNOMUPLA", "CMH23CONT", "CMLP23CONT", 
                           "CMHP23CONT", "CMHR23CONT", "CMHA23CONT", 
                           "CMHB23CONT", "CMHCE23CON", "CMHM23CONT", "CMHC23CONT",
                           "CMDS23CONT", "CMVI23CONT", "ALTURA"
)

# Merge the dataframes based on 'property_id'
merged_data <- merge(cleandata, points_data_nearest[, variables_to_retrieve], 
                     by = "property_id", all.x = TRUE)

# View the resulting merged data
view(merged_data)

write.csv(merged_data, file = "db_property_merged.csv", row.names = FALSE ) 

# Save the 'merged_data' dataframe to the RData file
file_name <- "merged_data.RData"
save(merged_data, file = file_name)