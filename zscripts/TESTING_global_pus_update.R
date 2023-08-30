
#### Create a global polygon for ABNJ 

world_borders_sf <- ne_countries(scale = "medium", returnclass = "sf")
world_borders_rs <- as(world_borders_sf, "SpatVector")
plot(world_borders_rs)

rs <- raster(ncol = 720, nrow = 360)
rs[] <- 1:ncell(rs)

rs <- raster(ncol = 720, nrow = 360)
rs[] <- 1:ncell(rs)


terra::rast(c(xmin = -180, xmax = 180, ymax = 90, ymin = -90), res = 1, crs = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs")




#
library(terra)
library(sf)
library(rnaturalearth)
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # nolint
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs" # nolint
robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs" # nolint


world_borders_sf <- ne_countries(scale = "medium", returnclass = "sf")
world_borders_rs <- as(world_borders_sf, "SpatVector")
rs <- terra::rast(ncol = 1440, nrow = 720, extent = ext(c(-180, 180, -90, 90)))
rs[] <- 1:1036800

b <- terra::rasterize(world_borders_rs, rs)
b[] <- ifelse(is.na(b[]), 2, NA)
b2 <- terra::project(b, y = robin)

c <- terra::as.polygons(b2)
c <- st_as_sf(c)

# Area
CellArea <- 10000 # in km2
h_diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m
s_diameter <- sqrt(CellArea*1e6) # Diameter in m

# Creating an equal-area grid
PUs <- st_make_grid(c,
                    square = F,
                    cellsize = c(h_diameter, h_diameter),
                    what = "polygons",
                    crs = st_crs(c)) %>%
  st_sf()

plot(st_geometry(PUs))

# Get rid of "land" polygons
logi_PUs <- st_centroid(PUs) %>%
  st_intersects(c) %>% 
  lengths > 0 # Get logical vector instead of sparse geometry binary
PUs1 <- PUs[logi_PUs == TRUE, ]
plot(st_geometry(PUs1))

