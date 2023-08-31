# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# ncell_rs
# 0.10 = 3600, 1800
# 0.01 = 36000, 18000

equal_area_grid <- function(area_km2, ncell_rs, outdir) {
 
  library(terra)
  library(sf)
  library(rnaturalearth)
  library(ggplot2)  
  
# projections
  LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
  robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
  
  
  
  
}

library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)

LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # nolint
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs" # nolint
robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs" # nolint


world_borders_sf <- ne_countries(scale = "medium", returnclass = "sf")
world_borders_rs <- as(world_borders_sf, "SpatVector")
rs <- terra::rast(ncol = 36000, nrow = 18000, extent = ext(c(-180, 180, -90, 90)))
rs[] <- 1:6480000

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



# Get rid of "land" polygons
logi_PUs <- st_centroid(PUs) %>%
  st_intersects(c) %>% 
  lengths > 0 # Get logical vector instead of sparse geometry binary
PUs1 <- PUs[logi_PUs == TRUE, ]

# plot(st_geometry(PUs1))
# plot(st_geometry(world_borders_sf))

sphere <- ne_download(category = "physical", type = "wgs84_bounding_box", returnclass = "sf")
sphere_robin <- st_transform(sphere, crs = robin)

g1 <- ggplot() +
  geom_sf(data = sphere_robin, size = 0.05) +
  geom_sf(data = PUs1, size = 0.05) +
  geom_sf(data = world_borders_sf, size = 0.05, fill = "grey20") +
  theme_bw()

ggsave("PUs_trial.png", plot = g1, width = 30, height = 20, dpi = 600, limitsize = FALSE)

