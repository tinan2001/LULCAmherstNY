library(terra)
library(sf)
library(ggplot2)
library(leaflet)
library(dplyr)

# Load the two classified rasters
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


plot(r10, main = "2010")
plot(r15, main = "2015")
plot(r17, main = "2017")
plot(r20, main = "2020")
plot(r24, main = "2024")


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

r10_aligned <- resample(r10_reclass, r10_reclass, method = "near")
r15_aligned <- resample(r15_reclass, r10_reclass, method = "near")
r17_aligned <- resample(r17_reclass, r10_reclass, method = "near")
r20_aligned <- resample(r20_reclass, r10_reclass, method = "near")
r24_aligned <- resample(r24_reclass, r10_reclass, method = "near")

r10_aligned[r10_aligned == 255] <- NA
r15_aligned[r15_aligned == 255] <- NA
r17_aligned[r17_aligned == 255] <- NA
r20_aligned[r20_aligned == 255] <- NA
r24_aligned[r24_aligned == 255] <- NA

freq(r24_aligned)

plot(r10_aligned, main = '2010')
plot(r15_aligned, main = '2015')
plot(r17_aligned, main = '2017')
plot(r20_aligned, main = '2020')
plot(r24_aligned, main = '2024')


change_10_15 <- r10_aligned * 100 + r15_aligned
change_15_17 <- r15_aligned * 100 + r17_aligned
change_15_20 <- r15_aligned * 100 + r20_aligned
change_17_20 <- r17_aligned * 100 + r20_aligned
change_20_24 <- r20_aligned * 100 + r24_aligned


plot(change_10_15, main = "2010 - 2015")
plot(change_15_17, main = "2015 - 2017")
plot(change_15_20, main = "2015 - 2020")
plot(change_17_20, main = "2015 - 2020")
plot(change_20_24, main = "2020 - 2024")

freq(change_10_15)
freq(change_15_17)
freq(change_15_20)
freq(change_17_20)
freq(change_20_24)

get_change_summary <- function(r1, r2, label) {
  change_map <- r1 * 100 + r2
  tab <- as.data.frame(freq(change_map))
  tab$from <- floor(tab$value / 100)
  tab$to <- tab$value %% 100
  tab$interval <- label
  tab <- tab[, c("interval", "from", "to", "count")]
  return(tab)
}

# Apply for each interval
tab_10_15 <- get_change_summary(r10_aligned, r15_aligned, "2010–2015")
tab_15_17 <- get_change_summary(r15_aligned, r17_aligned, "2015–2017")
tab_15_20 <- get_change_summary(r15_aligned, r20_aligned, "2015-2020")
tab_17_20 <- get_change_summary(r17_aligned, r20_aligned, "2017–2020")
tab_20_24 <- get_change_summary(r20_aligned, r24_aligned, "2020–2024")

# Combine all
all_changes <- rbind(tab_10_15, tab_15_20, tab_20_24)

all_changes <- all_changes %>%
  mutate(
    from = case_when(
      from %in% c(1, 2) ~ "1",
      TRUE ~ as.character(from)
    ),
    to = case_when(
      to %in% c(1, 2) ~ "1",
      TRUE ~ as.character(to)
    )
  ) %>%
  group_by(interval, from, to) %>%
  summarise(count = sum(count), .groups = "drop")

class_labels <- c(
  "0" = "Water",
  "1" = "Urban",
  "3" = "Vegetation"
)


ggplot(all_changes, aes(x = interval, y = count, fill = factor(to))) +
  geom_bar(stat = "identity") +
  facet_wrap(~from, ncol = 3, labeller = as_labeller(class_labels)) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set2"), labels = class_labels) +
  labs(
    title = "Land Cover Change",
    x = "Time Interval",
    y = "Pixel Count",
    fill = "To Class"
  ) +
  theme_minimal()