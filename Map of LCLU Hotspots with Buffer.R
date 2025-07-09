library(sf)
library(dplyr)
library(kableExtra)
library(ggplot2)

hotspots <- read_sf("ArcProClassifications/Hotspot_Points.shp")
UB <- read_sf("ArcProClassifications/UB.shp")
amherst <- read_sf("ArcProClassifications/BB.shp")


num_hotspots <- length(hotspots)


df <- as.data.frame(hotspots)

df <- df %>%
  mutate(Name = ifelse(Name == "Commerical", "Commercial", Name)) %>%
  mutate(Name = ifelse(Name == "Apartment", "Apartments", Name))%>%
  mutate(Name = ifelse(Name == "Residental", "Residential", Name)) %>%
  group_by(Name) %>%
  summarize(Count = n())

hotspots <- hotspots %>%
  mutate(Name = ifelse(Name == "Commerical", "Commercial", Name)) %>%
  mutate(Name = ifelse(Name == "Apartment", "Apartments", Name)) %>%
  mutate(Name = ifelse(Name == "Residental", "Residential", Name))

kable(df, caption = " 2024 Hotspot Land Use")%>% 
  kable_styling(bootstrap_options = 'striped')

amherst$Source <- "Amherst"
UB$Source <- "UB"
hotspots$Source <- "Hotspots"

ub_utm <- st_transform(UB, crs = 32618)
buffer_2mile <- st_buffer(ub_utm, dist = 3218.68)

ggplot() +
  geom_sf(data = amherst, aes(fill = Source), color = "black", alpha = 0.3) +
  geom_sf(data = UB, aes(shape = Source), color = "blue", size = 4) +  # UB points
  geom_sf(data = hotspots, aes(color = Name), size = 2, alpha = 0.8) +
  geom_sf(data = buffer_2mile, fill = "lightblue", alpha = 0.5) +
  scale_shape_manual(values = c("UB" = 16)) + 
  scale_fill_manual(values = c("Amherst" = "lightgray")) +
  scale_color_manual(values = c(
    "Residential" = "red",
    "Apartments" = "purple",
    "Commercial" = "orange",
    "Hospital" = "green",
    "Parking Lot" = "brown",
    "In Progress" = "pink",
    "Solar" = "black",
    'Other' = 'gray'
  )) +
  theme_minimal() +
  labs(
    title = "Hotspots in Amherst and UB Area",
    fill = "Polygon Layer",
    color = "Hotspot Type",
    shape = "UB Symbol"
  )

hotspots_utm <- st_transform(hotspots, crs = st_crs(buffer_2mile))
hotspots_within_buffer <- st_intersection(hotspots_utm, buffer_2mile)

df2 <- as.data.frame( hotspots_within_buffer)

df2 <- df2 %>%
  group_by(Name) %>%
  summarize(Count = n())

kable(df2, caption = "2024 Hotspot Land Use in 2 Mile Buffer of University at Buffalo")%>% 
  kable_styling(bootstrap_options = 'striped')






