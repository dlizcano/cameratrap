

# countur map
library(basemapR)
library(ggspatial)
library(gstat)


#   Let's define a bounding box (an AOI) based on the data (in Lat-Long)
bbox <- sf::st_bbox(CToperation_sf)
#   Expand box by 20% to give a little extra room
Dx <- (bbox[["xmax"]]-bbox[["xmin"]])*0.15
Dy <- (bbox[["ymax"]]-bbox[["ymin"]])*0.15
bbox["xmin"] <- bbox["xmin"] - Dx
bbox["xmax"] <- bbox["xmax"] + Dx
bbox["ymin"] <- bbox["ymin"] - Dy
bbox["ymax"] <- bbox["ymax"] + Dy
bb <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])

Base_basemapR <- basemapR::base_map(bb, basemap="mapnik", increase_zoom=2)

#   This is the best looking one.
CToperation_sf %>% 
  ggplot() +
  Base_basemapR +
  geom_sf()


##############################################

# Produces a ggplot object
Base_ggspatial <- ggplot(CToperation_sf) + 
  ggspatial::annotation_map_tile(zoomin=-1,
                                 progress="none") 

Base_ggspatial +
  geom_sf(inherit.aes = FALSE) +
  theme_minimal()


##########################################################
#   Density maps
##########################################################
googlecrs <- "EPSG:4326"

#   Lay down basemap
ggplot(species) +
  #   this is the best looking basemap
  Base_basemapR +
  #   Add points
  geom_sf(inherit.aes = FALSE) +
  #   Create filled density "contours" - the as.numeric converts from factor
  #   for the scale_fill_gradient command.
  stat_density_2d_filled(data=species, 
                         alpha = 0.4, 
                         n=50,
                         contour_var="ndensity",
                         aes(x=st_coordinates(species)[,1], 
                             y=st_coordinates(species)[,2], 
                            fill=as.numeric(..level..))
                         ) +
  #   Add actual contour lines
  stat_density_2d(data=species, 
                  alpha = 0.8, 
                  n=50,
                  contour=TRUE,
                  contour_var="density",
                  aes(x=st_coordinates(species)[,1], 
                      y=st_coordinates(species)[,2],)) +
  #   Twiddle the color scale
  scale_fill_gradient2("Species estimate S.chao1\ndensity", 
                       low = "white", 
                       mid = "yellow", 
                       high = "red", 
                       limits=c(0,10),
                       midpoint = 5) +
  #   Add a scale bar
  ggspatial::annotation_scale(data=species, aes(unit_category="metric",
                                              style="ticks"),
                              location="br", width_hint=0.2, bar_cols=1) +
  #   Add title
  ggtitle("Density of species estimate") +
  labs(subtitle="Density using stat_density_2d") +
  coord_sf(crs=googlecrs) # required 

##################################

localcrs <- "EPSG:21818" # Bogota 1975 / UTM zone 18N
#   First we create a box to build the grid inside of - in XY coordinates, not
#   lat long. 

#     Locations and rainfall by lat long
df_sf <- species# sf::st_as_sf(df, coords=c("Lon", "Lat"), crs=googlecrs, agr = "identity")
#     Locations and rainfall by X, Y in webcrs
# df_web <- sf::st_transform(df_sf, crs=webcrs)
#     Locations and rainfall by X, Y
df_xy <- sf::st_transform(df_sf, crs=localcrs)
#     Locations and rainfall by X, Y
# df_xy_2 <- sf::st_transform(df_sf, crs=localUTM)


bbox_xy = bbox %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs = localcrs) %>%
  sf::st_bbox()
#     sfc POINT file
grd_sf <- sf::st_make_grid(x=bbox_xy,
                           what="corners",
                           cellsize=1000,
                           crs=localcrs
)


#transform species
species_localcrs <- sf::st_transform(species, crs=localcrs)
#     Create interpolator and interpolate to grid

fit_IDW <- gstat::gstat( 
  formula = S.chao1 ~ 1,
  data = species_localcrs, 
  #nmax = 10, nmin = 3, # can also limit the reach with these numbers
  set = list(idp = 2) # inverse distance power
)

#   Use predict to apply the model fit to the grid (using the data frame grid
#   version)

#           We set debug.level to turn off annoying output
interp_IDW <- predict(fit_IDW, grd_sf, debug.level=0)

#   Convert to a stars object so we can use the contouring in stars
interp_IDW_stars <- stars::st_rasterize(interp_IDW %>% dplyr::select(S.chao1=var1.pred, geometry))

#   Quick sanity check, and can use to adjust the distance power Looks pretty
#   good - most input points look close to the output grid points, with some
#   notable exceptions. The red point to the north is possibly bad data. Easier
#   to judge from areal displays.
ggplot() +
  #   geom_stars is a good way to display a grid
  stars::geom_stars(data=interp_IDW_stars) +  
  geom_sf(data=df_xy, aes(color=S.chao1), size=5) +
  geom_sf(data=df_xy, color="Black", size=1) +
  #     This is for the raster color fill
  scale_fill_gradientn(colors=rainbow(5), limits=c(0,3)) +
  #     This is for the original data points
  scale_color_gradientn(colors=rainbow(5), limits=c(0,3)) +
  labs(title="Inverse Distance Weighting, Power = 2")


tmap::tmap_mode("plot") # set mode to static plots

#   Tweak the bounding box, it felt a little restrictive. But I would like the
#   raster coloring to stay smaller...
new_bb <- tmaptools::bb(bbox, relative=TRUE,
                        width=1.05,
                        height=1.05)

tmap::tm_shape(Base_basemapR, bbox=new_bb)+
  tmap::tm_rgb() +
  tmap::tm_graticules(col ="lightgray", line = TRUE) +
  tmap::tm_shape(interp_IDW_stars) +
  tmap::tm_raster(palette="Blues", alpha=0.5, style="cont") +
  tmap::tm_layout(frame = TRUE, inner.margins = 0, legend.outside=TRUE) +
  tmap::tm_shape(df_sf) + 
  tmap::tm_sf(col="black", size=0.3) +
  #tmap::tm_shape(Contours_LL) + 
  #tmap::tm_iso(col="black", text="focal_mean") +
  tmap::tm_scale_bar(breaks = c(0, 5, 10), text.size = 1,
                     position=c("right", "BOTTOM"))


