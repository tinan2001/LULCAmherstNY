library(terra)
library(sf)
library(ggplot2)
library(leaflet)

r10 <- rast("ArcProClassifications/Amherst_2010.tif") 
r15 <- rast("ArcProClassifications/Amherst_2015.tif") 
r17 <- rast("ArcProClassifications/Amherst_2017.tif") 
r20 <- rast("ArcProClassifications/Amherst_2020.tif") 
r24 <- rast("ArcProClassifications/Amherst_2024.tif") 

amherst <- vect("ArcProClassifications/BB.shp")
amherst <- project(amherst, crs(r17))
amherst_sf <- st_as_sf(amherst)

r10 <- mask(crop(r10, amherst[2,]), amherst)
r15 <- mask(crop(r15, amherst[2,]), amherst)
r17 <- mask(crop(r17, amherst[2,]), amherst)
r20 <- mask(crop(r20, amherst[2,]), amherst)
r24 <- mask(crop(r24, amherst[2,]), amherst)

rcl <- matrix(c(
  10, 1,
  20, 2,
  30, 3,
  60, 4
), ncol = 2, byrow = TRUE)


r10_reclass <- classify(r10, rcl)
r15_reclass <- classify(r15, rcl)
r17_reclass <- classify(r17, rcl)
r20_reclass <- classify(r20, rcl)
r24_reclass <- classify(r24, rcl)

r10_aligned <- r10_reclass
r15_aligned <- resample(r15_reclass, r10_reclass, method = "near")
r17_aligned <- resample(r17_reclass, r10_reclass, method = "near")
r20_aligned <- resample(r20_reclass, r10_reclass, method = "near")
r24_aligned <- resample(r24_reclass, r10_reclass, method = "near")

r10_aligned[r10_aligned == 255] <- NA
r15_aligned[r15_aligned == 255] <- NA
r17_aligned[r17_aligned == 255] <- NA
r20_aligned[r20_aligned == 255] <- NA
r24_aligned[r24_aligned == 255] <- NA

veg_to_dev_10_15 <- (r10_aligned %in% c(0,3)) & (r15_aligned %in% c(1,2))
plot(veg_to_dev_10_15, main = "Vegetation to Developed 2010 - 2015")

veg_to_dev_15_17 <- (r15_aligned %in% c(0,3)) & (r17_aligned %in% c(1,2))
plot(veg_to_dev_15_17, main = "Vegetation to Developed 2015 - 2017")

veg_to_dev_17_20 <- (r17_aligned %in% c(0,3)) & (r20_aligned %in% c(1,2))
plot(veg_to_dev_17_20, main = "Vegetation to Developed 2017 - 2020")

veg_to_dev_20_24 <- (r20_aligned %in% c(0,3)) & (r24_aligned %in% c(1,2))
plot(veg_to_dev_20_24, main = "Vegetation to Developed 2020 - 2024")

veg_to_dev_10_24 <- (r10_aligned %in% c(0,3)) & (r24_aligned %in% c(1,2))
plot(veg_to_dev_10_24, main = "Vegetation to Developed 2010 - 2024")

veg_to_dev_17_24 <- (r17_aligned %in% c(0,3)) & (r24_aligned %in% c(1,2))
plot(veg_to_dev_17_24, main = "Vegetation to Developed 2017 - 2024")

moving_window <- matrix(1, 5, 5)

#2010 - 2015 Hotspot 
veg_loss_hotspots_10_15 <- focal(veg_to_dev_10_15, w = moving_window, fun = sum, na.policy = "omit")
plot(veg_loss_hotspots_10_15, main = "Vegetation to Development Hotspots 2010 - 2015", col = terrain.colors(100))

strong_hotspots_10_15<- veg_loss_hotspots_10_15 > 10
plot(strong_hotspots_10_15, main = "Strong Vegetation Loss Hotspots 2010 - 2015", col = c("white", "red"))

n_hotspot_pixels_10_15 <- global(strong_hotspots_10_15, sum, na.rm = TRUE)

hotspot_area_m2_10_15 <- n_hotspot_pixels_10_15 * 900

print(paste("2010-2015 Total strong hotspot area (meters^2):", round(hotspot_area_m2_10_15, 2)))

#2015 - 2017 Hotspot 
veg_loss_hotspots_15_17 <- focal(veg_to_dev_15_17, w = moving_window, fun = sum, na.policy = "omit")
plot(veg_loss_hotspots_15_17, main = "Vegetation to Development Hotspots 2015 - 2017", col = terrain.colors(150))

strong_hotspots_15_17<- veg_loss_hotspots_15_17 > 15
plot(strong_hotspots_15_17, main = "Strong Vegetation Loss Hotspots 2015 - 2017", col = c("white", "red"))

n_hotspot_pixels_15_17 <- global(strong_hotspots_15_17, sum, na.rm = TRUE)

hotspot_area_m2_15_17 <- n_hotspot_pixels_15_17 * 900

print(paste("2015-2017 Total strong hotspot area (meters^2):", round(hotspot_area_m2_15_17, 2)))


#2017 - 2020 Hotspot 
veg_loss_hotspots_17_20 <- focal(veg_to_dev_17_20, w = moving_window, fun = sum, na.policy = "omit")
plot(veg_loss_hotspots_17_20, main = "Vegetation to Development Hotspots 2017 - 2020", col = terrain.colors(170))

strong_hotspots_17_20<- veg_loss_hotspots_17_20 > 17
plot(strong_hotspots_17_20, main = "Strong Vegetation Loss Hotspots 2017 - 2020", col = c("white", "red"))

n_hotspot_pixels_17_20 <- global(strong_hotspots_17_20, sum, na.rm = TRUE)

hotspot_area_m2_17_20 <- n_hotspot_pixels_17_20 * 900

print(paste("2017-2020 Total strong hotspot area (meters^2):", round(hotspot_area_m2_17_20, 2)))

#2020 - 2024 Hotspot 
veg_loss_hotspots_20_24 <- focal(veg_to_dev_20_24, w = moving_window, fun = sum, na.policy = "omit")
plot(veg_loss_hotspots_20_24, main = "Vegetation to Development Hotspots 2020 - 2024", col = terrain.colors(170))

strong_hotspots_20_24<- veg_loss_hotspots_20_24 > 17
plot(strong_hotspots_20_24, main = "Strong Vegetation Loss Hotspots 2020 - 2024", col = c("white", "red"))

n_hotspot_pixels_20_24 <- global(strong_hotspots_20_24, sum, na.rm = TRUE)

hotspot_area_m2_20_24 <- n_hotspot_pixels_20_24 * 900

print(paste("2020-2024 Total strong hotspot area (meters^2):", round(hotspot_area_m2_20_24, 2)))

#2010 - 2024 Hotspot 
veg_loss_hotspots_10_24 <- focal(veg_to_dev_10_24, w = moving_window, fun = sum, na.policy = "omit")
plot(veg_loss_hotspots_10_24, main = "Vegetation to Development Hotspots 2010 - 2024", col = terrain.colors(170))

strong_hotspots_10_24<- veg_loss_hotspots_10_24 > 15
plot(strong_hotspots_10_24, main = "Strong Vegetation Loss Hotspots 2010 - 2024", col = c("white", "red"))

n_hotspot_pixels_10_24 <- global(strong_hotspots_10_24, sum, na.rm = TRUE)

hotspot_area_m2_10_24 <- n_hotspot_pixels_10_24 * 900

print(paste("2010-2024 Total strong hotspot area (meters^2):", round(hotspot_area_m2_10_24, 2)))

#2017 - 2024 Hotspot 
veg_loss_hotspots_17_24 <- focal(veg_to_dev_17_24, w = moving_window, fun = sum, na.policy = "omit")
plot(veg_loss_hotspots_17_24, main = "Vegetation to Development Hotspots 2017 - 2024", col = terrain.colors(170))

strong_hotspots_17_24<- veg_loss_hotspots_17_24 > 15
plot(strong_hotspots_17_24, main = "Strong Vegetation Loss Hotspots 2017 - 2024", col = c("white", "red"))

n_hotspot_pixels_17_24 <- global(strong_hotspots_17_24, sum, na.rm = TRUE)

hotspot_area_m2_17_24 <- n_hotspot_pixels_17_24 * 900

print(paste("2010-2024 Total strong hotspot area (meters^2):", round(hotspot_area_m2_17_24, 2)))

#2017 - 2024 Hotspot 
strong_hotspots_proj_17_24 <- project(strong_hotspots_17_24, "EPSG:3857")
# 1. Convert logical values (FALSE/TRUE) to numeric (0/1)
strong_hotspots_proj_17_24_numeric <- as.numeric(strong_hotspots_proj_17_24)

# 2. Mask out 0 and NA values (only keep the 1's)
strong_hotspots_proj_17_24_numeric_no_zeros <- mask(strong_hotspots_proj_17_24_numeric, strong_hotspots_proj_17_24_numeric, maskvalues = c(0, NA))

# 3. Set color palette for 0 (no hotspot) as transparent and 1 (hotspot) as red
pal_hotspots <- colorNumeric(
  palette = c("transparent", "red"),  # Transparent for no hotspot (0), red for hotspot (1)
  domain = c(0, 1)
)

# 4. Create the Leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addRasterImage(
    strong_hotspots_proj_17_24_numeric_no_zeros,
    colors = pal_hotspots,
    opacity = 0.50,  # Show hotspots with full opacity
    project = FALSE
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_hotspots,
    values = c(0, 1),
    title = "Vegetation Loss Hotspots (2017-2024)",
    labels = c("No Hotspot", "Hotspot")
  )


#2010 - 2024 Hotspot 
strong_hotspots_proj_10_24 <- project(strong_hotspots_10_24, "EPSG:3857")
# 1. Convert logical values (FALSE/TRUE) to numeric (0/1)
strong_hotspots_proj_10_24_numeric <- as.numeric(strong_hotspots_proj_10_24)

# 2. Mask out 0 and NA values (only keep the 1's)
strong_hotspots_proj_10_24_numeric_no_zeros <- mask(strong_hotspots_proj_10_24_numeric, strong_hotspots_proj_10_24_numeric, maskvalues = c(0, NA))

# 3. Set color palette for 0 (no hotspot) as transparent and 1 (hotspot) as red
pal_hotspots <- colorNumeric(
  palette = c("transparent", "red"),  # Transparent for no hotspot (0), red for hotspot (1)
  domain = c(0, 1)
)

# 4. Create the Leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addRasterImage(
    strong_hotspots_proj_10_24_numeric_no_zeros,
    colors = pal_hotspots,
    opacity = 0.50,  # Show hotspots with full opacity
    project = FALSE
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_hotspots,
    values = c(0, 1),
    title = "Vegetation Loss Hotspots (2010-2024)",
    labels = c("No Hotspot", "Hotspot")
  )

