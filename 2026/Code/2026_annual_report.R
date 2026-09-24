# Summarizing wading bird sampling for 2026 for the annual wading bird report
# Brittany Mason

library(tidyverse)
library(sf)
library(ggspatial)  
library(prettymapr)
library(sf)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(patchwork)
library(ggrepel)
library(bbmle)
library(maptiles)
library(terra)
library(RStoolbox)

# define select and slice as the dplyr package
select <- dplyr::select
slice <- dplyr::slice

# read in aerial and ground survey data
ag_data <- read_csv("2026/Data/Wading_Bird_Aerial_Ground_Survey_Database.csv") %>%
  mutate(Date=mdy(Date)) %>% 
  filter(Date > mdy("01-01-2026"))

# read in nest check data
nest_data <- read_csv("2026/Data/Wading_Bird_Nest_Check_Database.csv") %>%
  mutate(Date=mdy(Date)) %>% 
  filter(Date > mdy("01-01-2026"))

# read in nest location data
nest_locations <- read_csv("2026/Data/2026_Colonies.csv")

# historic nesting counts
historic <- read_csv("2026/Data/Historic_Nest_Counts_Lake_O.csv")

# Aerial Survey Summary ---------------------------------------------------

# Dates of flights
aerial_surveys <- ag_data %>% 
  filter(`Survey Type`=="Aerial",
         Date > mdy("01-01-2026"))
unique(aerial_surveys$Date)

## Colony Summary ---------------------------------------------------------------------

### Peak Numbers ---------------------------------------------------------------------

# Let's just examine the photograph numbers
photo_count <- ag_data %>% 
  filter(Method %in% c("Photograph Count", "Targeted Ground"),
         complete.cases(`Nest Count`),
         `Nest Count` > 0) %>%
  mutate(Month=month(Date, label = TRUE, abb = TRUE)) 

# get the average count of bird by species for each month
avg_month <- photo_count %>%
  group_by(Month, `Colony Name`, `Species Code`) %>%
  summarise(mean_count = ceiling(mean(`Nest Count`, na.rm = TRUE)), .groups = "drop")

# get the maximum count per species by colony
max_counts <- avg_month %>%
  group_by(`Colony Name`, `Species Code`) %>%
  summarise(max_count = max(mean_count, na.rm=TRUE)) %>%
  pivot_wider(
    names_from = `Species Code`,
    values_from = max_count,
    values_fill = 0  
  ) %>%
  # order columns to match wading bird reports
  select("Colony Name", "GREG", "WHIB", "SNEG", "ROSP", "GBHE", "LBHE", "TRHE", "GLIB", "SMDK",
         "SMWT",  "CAEG", "ANHI")

# next determine the month of peak nesting for all species (except for non-wading birds: DCCO, CAEG, ANHI)
nest_total_by_month <- avg_month %>%
  filter(!`Species Code` %in% c("DCCO", "CAEG", "ANHI")) %>%
  group_by(Month, `Colony Name`) %>%
  summarise(count=sum(mean_count))

(peak_month <- nest_total_by_month %>%
  group_by(`Colony Name`) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
    select(-count))

# add this to the max_counts data frame
max_counts <- left_join(max_counts, peak_month, by=c("Colony Name"))

# fix encoding issues in the nest_locations file
nest_locations <- nest_locations %>%
  mutate(Colony = gsub("\u00A0", " ", Colony))

# add nest location data
max_counts <- left_join(max_counts, nest_locations %>% select(-`Nest Checks`), by=c("Colony Name"="Colony"))

# clean up the order of columns
max_counts <- max_counts %>%
  select(
    `Colony Name`,
    Month,
    Latitude,
    Longitude,
    everything()   # keeps all remaining columns in their current order
  )

# finally, let's get total counts of species-specific peak nest abundances (except for non-wading birds: DCCO, CAEG, ANHI)
max_counts <- max_counts %>%
  mutate(Total = rowSums(across(GREG:SMWT), na.rm=TRUE))

max_counts

# now save the table 
write_csv(max_counts, "2026/Tables/Table_2.csv")

# how many colonies?
nrow(max_counts)

# colonies by colony type
colony_type <- historic %>%
  mutate(`Colony Name`=ifelse(`Colony Name`=="Clewiston Spit", "Clewiston Spit 1", `Colony Name`)) %>%
  group_by(`Colony Name`) %>%
  summarize(`Colony Type`=first(`Colony Type`))
  

max_counts_colony_type <- left_join(max_counts, colony_type, by="Colony Name")

max_counts_colony_type <- max_counts_colony_type %>%
  mutate(
    `Colony Type` = case_when(
      `Colony Name` == "Lemkin Creek" ~ "Off-Lake Island",
      str_starts(`Colony Name`, "Rock Island") ~ "Spoil Island",
      TRUE ~ `Colony Type`
    )
  )

max_counts_colony_type %>% 
  group_by(`Colony Type`) %>% 
  summarize(count=n())

# total number of nesting birds recorded
sum(max_counts$Total)

# get the total species counts
species_cols <- names(max_counts)[which(sapply(max_counts, is.numeric))]

col_sums <- numeric(length(species_cols))
names(col_sums) <- species_cols

for (col in species_cols) {
  col_sums[col] <- sum(max_counts[[col]], na.rm = TRUE)
}

col_sums

# total wading bird nests
sum(max_counts$Total)

# let's just look at the total for GREG, SNEG, and WHIB since that is a number we will want to report on. 
sum(sum(max_counts$GREG), sum(max_counts$WHIB), sum(max_counts$SNEG))

# calculate historic mean and sd

# for this, we will use the clean data
historic_clean <- read_csv("2026/Data/historic_nest_totals.csv")

# filter to 2008-2025
historic_clean_focal <- historic_clean %>%
  filter(year >= 2008 & year <= 2025)

mean(historic_clean_focal$focal_white)
sd(historic_clean_focal$focal_white)

# determine the percentage change from the average between 2008 and 2025
(mean(historic_clean_focal$focal_white)-sum(sum(max_counts$GREG), sum(max_counts$WHIB), sum(max_counts$SNEG)))/mean(historic_clean_focal$focal_white)*100

# let's look at just GREG
hist_sp_greg <- historic %>%
  filter(Species %in% c("Great Egret"),
         Number != "P") %>%
  mutate(Number=as.numeric(Number)) %>%
  group_by(`Report Year`) %>%
  summarise(count=sum(Number))

mean(hist_sp_greg$count)
sd(hist_sp_greg$count)  

sum(max_counts$GREG)

# determine the percentage change from the average between 2008 and 2023 which is 3,095
(mean(hist_sp_greg$count)-sum(max_counts$GREG))/mean(hist_sp_greg$count)*100

# let's look at just SNEG
hist_sp_sneg <- historic %>%
  filter(Species %in% c("Snowy Egret"),
         Number != "P") %>%
  mutate(Number=as.numeric(Number)) %>%
  group_by(`Report Year`) %>%
  summarise(count=sum(Number))

mean(hist_sp_sneg$count)
sd(hist_sp_sneg$count)  

sum(max_counts$SNEG)

# determine the percentage change from the average between 2008 and 2023 which is 3,095
(mean(hist_sp_sneg$count)-sum(max_counts$SNEG))/mean(hist_sp_sneg$count)*100

# let's look at just WHIB
hist_sp_whib <- historic %>%
  filter(Species %in% c("White Ibis"),
         Number != "P") %>%
  mutate(Number=as.numeric(Number)) %>%
  group_by(`Report Year`) %>%
  summarise(count=sum(Number))

mean(hist_sp_whib$count)
sd(hist_sp_whib$count)  

sum(max_counts$WHIB)

# determine the percentage change from the average between 2008 and 2023 which is 3,095
(mean(hist_sp_whib$count)-sum(max_counts$WHIB))/mean(hist_sp_whib$count)*100


#### Clean max count data to add to historic data excel ----------------------

# read in species codes
species_codes <- read_csv("2026/Data/species_codes.csv")
colnames(species_codes) <- c("Species", "Wading Bird Code")

max_counts_colony_type_clean <- max_counts_colony_type %>% 
  pivot_longer(
    cols = -c(`Colony Name`, Month, Latitude, Longitude, Total, `Colony Type`),
    names_to   = "Species",
    values_to  = "Count"
  ) %>%
  left_join(species_codes, by="Species") %>%
  mutate(`Report Year` = 2026) %>%
  select(`Report Year`, `Colony Name`, `Colony Type`, `Wading Bird Code`, Count) %>%
  rename(Species=`Wading Bird Code`)

### Timing and nest numbers for species ---------------------------------------------------------------------

# get species totals by month
(sp_by_month <- avg_month %>%
  group_by(Month, `Species Code`) %>%
  summarise(total_count=sum(mean_count)) %>% 
  pivot_wider(names_from = `Species Code`, values_from = total_count) %>%
  replace_na(list(
    GREG = 0, WHIB = 0, SNEG = 0, ROSP = 0, GBHE = 0, LBHE = 0,
    TRHE = 0, GLIB = 0, SMDK = 0, SMWT = 0, 
    CAEG = 0, ANHI = 0
  )) %>%
  mutate(Month = month.name[match(Month, month.abb)]) %>%
  select("Month", "GREG", "WHIB", "SNEG", "ROSP", "GBHE", "LBHE", "TRHE", "GLIB",  "SMDK",
         "SMWT",  "CAEG", "ANHI") %>%
  mutate(Total = rowSums(across(GREG:SMWT), na.rm=TRUE)))

# now save the table 
write_csv(sp_by_month, "2026/Tables/Table_3.csv")

## Map - Aerial Survey Route ---------------------------------------------------------------------

# Make map of aerial survey route

# Start by reading in the data
aerial_tracks_wgs84 <- st_read("2026/Data/aerial_survey_route.shp")

# get satellite imagery

# start by created a buffered boundary box around the aerial tracks
bbox_orig <- st_bbox(aerial_tracks_wgs84)

# Expand by 0.05 degrees
buffer <- 0.1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=10, provider = "Esri.WorldImagery", crop = TRUE)

# Plot satellite map with aerial tracks

# Ensure raster and points CRS match
aerial_tracks_wgs84 <- st_transform(aerial_tracks_wgs84, crs(sat_map))

# Plot
imagery_map <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = aerial_tracks_wgs84,
          color = "white", lwd = 2, inherit.aes = FALSE) +
  annotation_scale(location="bl", width_hint=0.3,
                   pad_x=unit(0.5,"in"), pad_y=unit(0.4,"in")) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x=unit(0.5,"in"), pad_y=unit(0.6,"in"),
                         style=north_arrow_fancy_orienteering) +
  theme_void()

imagery_map


# Inset map of Florida

# Florida state polygon
us_states <- ne_states(country = "United States of America", returnclass = "sf")
florida <- us_states[us_states$name == "Florida", ]

# Survey centroid for inset point
survey_point <- st_centroid(st_union(aerial_tracks_wgs84))

inset_map <- ggplot() +
  geom_sf(data = florida, fill = "grey80", color = "grey50") +
  geom_sf(data = survey_point, color = "black", size = 4) +
  theme_void()

# Combine maps and inset

# do the same for transects
final_plot_transects <- ggdraw() +
  draw_plot(imagery_map) +
  draw_plot(
    ggdraw() + 
      draw_plot(inset_map) + 
      theme(plot.background = element_rect(fill = "white", color = "black")),
    x = 0, y = 0.7, width = 0.3, height = 0.3
  )

final_plot_transects

ggsave("2026/Figures/Figure_1.jpeg", height=6, width=6, units="in")

## Map - Colony Locations ---------------------------------------------------------------------

# create a shapefile
nc_locs_sf <- st_as_sf(nest_locations, coords=c("Longitude", "Latitude"), crs=4326)

# add a column for nest type
nc_locs_sf <- nc_locs_sf %>%
  mutate(`Colony Type` = case_when(Colony == "Clewiston Spit 1" ~ "Spoil Island",
                                   Colony == "Clewiston Spit 2" ~ "Spoil Island",
                                   Colony == "Eagle Bay Island" ~ "Marsh Island",
                                   Colony == "Lemkin Creek" ~ "Off-Lake Island",
                                   Colony == "Gun Range" ~ "Off-Lake Island",
                                   Colony == "Rock Island 5" ~ "Spoil Island",
                                   Colony == "Rock Island 6" ~ "Spoil Island",
                                   Colony == "Rock Island 7" ~ "Spoil Island",
                                   Colony == "Rock Island 8" ~ "Spoil Island",
                                   Colony == "Rock Island 9" ~ "Spoil Island",
                                   Colony == "Rock Island 10" ~ "Spoil Island",
                                   Colony == "Rock Island 11" ~ "Spoil Island",
                                   Colony == "Rock Island 12" ~ "Spoil Island",
                                   Colony == "Rock Island 13" ~ "Spoil Island",
                                   Colony == "Rock Island 14" ~ "Spoil Island",
                                   Colony == "Rock Island 15" ~ "Spoil Island",
                                   Colony == "Little Bear Beach" ~ "Spoil Island",
                                   TRUE ~ NA_character_ ))

# start by created a buffered boundary box around the aerial tracks
bbox_orig <- st_bbox(aerial_tracks_wgs84)

# Expand by 0.05 degrees
buffer <- 0.1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=10, provider = "Esri.WorldImagery", crop = TRUE)

# Plot satellite map with colony locations

# Add lon/lat columns for labeling
nc_locs_sf_coords <- nc_locs_sf %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  )


# Add lon/lat columns for labeling
nc_locs_sf_coords <- nc_locs_sf %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  )

# Create a single label point for Rock Islands 5–15
rock_island_label <- nc_locs_sf %>%
  filter(Colony %in% paste0("Rock Island ", 5:15)) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  mutate(
    label = "Rock Island 5–15",
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# Remove individual Rock Island labels
nc_locs_sf_coords_no_rock <- nc_locs_sf_coords %>%
  filter(!Colony %in% paste0("Rock Island ", 5:15))

# Plot
colony_locs <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  
  # Plot all colony locations, including Rock Islands
  geom_sf(
    data = nc_locs_sf,
    color = "white",
    size = 3,
    inherit.aes = FALSE
  ) +
  
  # Labels for all colonies except Rock Islands
  geom_text_repel(
    data = nc_locs_sf_coords_no_rock,
    aes(x = lon, y = lat, label = Colony),
    size = 4,
    box.padding = 0.5,
    point.padding = 0.2,
    color = "white"
  ) +
  
  # Single label for Rock Islands 5–15
  geom_text_repel(
    data = rock_island_label,
    aes(x = lon, y = lat, label = label),
    size = 4,
    color = "white",
    box.padding = 0.5,
    point.padding = 0.2,
    nudge_x = 0.03,
    nudge_y = -0.01
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.4, "in")
  ) +
  
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.6, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  
  theme_void()

colony_locs

# let's make an inset map just for Rock Island colonies
rock_islands <- nc_locs_sf %>%
  filter(Colony %in% paste0("Rock Island ", 5:15))

rock_bbox <- st_bbox(rock_islands)

# Expand by 0.05 degrees
# Expand by 0.05 degrees
buffer <- 0.015
bbox_expanded <- rock_bbox
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
rock_sat_map <- get_tiles(bbox_sfc, zoom=16, provider = "Esri.WorldImagery", crop = TRUE)

# 3. Create inset map
rock_inset <- ggRGB(
  rock_sat_map,
  r = 1, g = 2, b = 3
) +
  geom_sf(
    data = rock_islands,
    color = "white",
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_void() 

rock_inset

# 4. Add inset to main map
colony_locs_inset <- colony_locs +
  annotation_custom(
    grob = ggplotGrob(rock_inset),
    xmin = -80.5,   # adjust these to position inset
    xmax = -80.1,
    ymin = 25.8,
    ymax = 26.2
  )

colony_locs_inset

# combine with inset map from previous section
final_map <- ggdraw() +
  draw_plot(colony_locs) +  # main map fills the canvas
  draw_plot(ggdraw() + 
              draw_plot(inset_map) + 
              theme(plot.background = element_rect(fill = "white", color = "black")),
            x = 0.8, y = 0.8, width = 0.2, height = 0.2)  # inset top-right
final_map

final_map / rock_inset

# save it
ggsave("2026/Figures/Figure_4.jpeg", height=9, width=9, units="in")


## Map - Previous Colony Locations ---------------------------------------------------------------------

# use historic nest count file to summarize previous colony locations by colony type

# get colony locations based on mean latitude and longitude
col_locs <- historic %>%
  filter(complete.cases(Latitude),
         complete.cases(Longitude)) %>%
  group_by(`Colony Name`) %>%
  summarise(Latitude=mean(Latitude), Longitude=mean(Longitude),
            `Colony Type`=first(`Colony Type`))

# now get a summary of colony name and nest check
col_nest_check <- historic %>%
  group_by(`Report Year`, `Colony Name`) %>%
  summarise(`Nest Check`=first(`Nest Check`)) %>%
  ungroup() %>%
  filter(`Nest Check` == "Y") %>%
  count(`Colony Name`, name = "nest_checks")

col_locs_nc <- left_join(col_locs, col_nest_check, by="Colony Name")

# Convert to sf object
col_locs_nc_sf <- st_as_sf(
  col_locs_nc,
  coords = c("Longitude", "Latitude"),
  crs = 4326,      # WGS84
  remove = FALSE   # keep original lat/long columns
)

# start by created a buffered boundary box around the aerial tracks
bbox_orig <- st_bbox(aerial_tracks_wgs84)

# Expand by 0.05 degrees
buffer <- 0.1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=11, provider = "Esri.WorldImagery", crop = TRUE)

# Plot satellite map with aerial tracks
colony_locs_hist <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  
  geom_sf(
    data = col_locs_nc_sf %>%
      mutate(
        has_checks = ifelse(is.na(nest_checks), "No", "Yes"),
        plot_checks = ifelse(is.na(nest_checks), 1, nest_checks)
      ),
    aes(
      color = `Colony Type`,
      shape = has_checks,
      size  = plot_checks
    ),
    inherit.aes = FALSE
  ) +
  annotation_scale(location="bl", width_hint=0.3,
                   pad_x=unit(0.5,"in"), pad_y=unit(0.4,"in")) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x=unit(0.5,"in"), pad_y=unit(0.6,"in"),
                         style=north_arrow_fancy_orienteering) +
  
  scale_color_manual(values = c(
    "Marsh Island"    = "#E69F00",
    "Off-Lake Island" = "#009E73",
    "Spoil Island"    = "#0072B2"
  )) +
  scale_shape_manual(
    values = c("Yes" = 16, "No" = 1),
    name = "Nest checks\nconducted"
  ) +
  scale_size(
    range  = c(2, 5),
    name   = "Number of\nnest checks",
    breaks = seq(1, 13, by = 3)   # every 3rd number
  ) +
  
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    size  = guide_legend(order = 3)
  ) +
  
  theme_void()


colony_locs_hist

# combine with inset map from previous section
final_map <- ggdraw() +
  draw_plot(colony_locs_hist) +  # main map fills the canvas
  draw_plot(ggdraw() + 
              draw_plot(inset_map) + 
              theme(plot.background = element_rect(fill = "white", color = "black")),
            x = 0.03, y = 0.75, width = 0.2, height = 0.2)  # inset top-right
final_map

# save it
ggsave("2026/Figures/Figure_3.jpeg", height=6, width=7, units="in")


# Nest Check Summary ---------------------------------------------------

# Dates when nest checks were conducted by colony
nest_summary <- nest_data %>%
  distinct(`Colony Name`, Date) %>%
  arrange(`Colony Name`, Date) %>%
  group_by(`Colony Name`) %>%
  mutate(
    date_number = row_number(),
    Date = format(Date, "%d-%b")
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = `Colony Name`,
    values_from = Date,
    id_cols = date_number
  ) %>%
  select(-date_number)

nest_summary

# now save the table 
write_csv(nest_summary, "2026/Tables/Table_1.csv")

## Map - Nest Check Locations ---------------------------------------------------------------------

# select only the locations that we did nest checks
nc_locs <- nest_locations %>%
  dplyr::filter(`Nest Checks`=="Y")

nc_locs_sf <- st_as_sf(nc_locs, coords=c("Longitude", "Latitude"), crs=4326)

# Create a proper bbox vector for map
# start by created a buffered boundary box around the aerial tracks
bbox_orig <- st_bbox(aerial_tracks_wgs84)

# Expand by 0.05 degrees
buffer <- 0.1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=10, provider = "Esri.WorldImagery", crop = TRUE)

# Plot satellite map with aerial tracks

# Add lon/lat columns for labeling
nc_locs_sf_coords <- nc_locs_sf %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  )

# define colors for the points
cols <- c("Clewiston Spit 1" = "#1b9e77",      
          "Little Bear Beach" = "#d95f02",   
          "Rock Island 11" = "#7570b3",       
          "Clewiston Spit 2" = "#1f78b4",
          "Rock Island 12" = "#CC79A7")  

# Plot
nest_locations_plot <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = nc_locs_sf, aes(color = Colony), size = 5, inherit.aes = FALSE) +
  geom_text_repel(
    data =  nc_locs,
    aes(x = Longitude, y = Latitude, label = Colony),
    size = 4,
    box.padding = 0.5,
    point.padding = 0.2,
    color = "white"
  ) +
  scale_color_manual(values=cols) +
  annotation_scale(location="bl", width_hint=0.3,
                   pad_x=unit(0.2,"in"), pad_y=unit(0.1,"in")) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x=unit(0.2, "in"), pad_y=unit(0.2,"in"),
                         style=north_arrow_fancy_orienteering) +
  theme_void() +
  theme(legend.position = "none")  # remove legend
nest_locations_plot


### Little Bear Beach -------------------------------------------------------

# create four inset maps to go with this map

# read in transects
lbb <- st_read("2026/Data/nest_check_spatial_files/LBB_track.gpx", layer = "tracks", quiet = TRUE)

# read in polygon of island
lbb_polygon <- st_read("2026/Data/nest_check_spatial_files/LBB.shp")

# Create a proper bbox vector for map
# start by created a buffered boundary box around the aerial tracks
bbox_poly <- st_bbox(lbb_polygon)

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_poly)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=19, provider = "Esri.WorldImagery", crop = TRUE)

lbb_plot <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = lbb, color = "white", lwd = 2, inherit.aes = FALSE) +
  theme_void() +
  ggtitle("Little Bear Beach") 
lbb_plot

### Clewiston Spit -------------------------------------------------------

# read in transects
cs <- st_read("2026/Data/nest_check_spatial_files/CS1_Track.gpx", layer = "tracks", quiet = TRUE)

# read in polygon of island
cs_polygon <- st_read("2026/Data/nest_check_spatial_files/CS1.shp")

# Create a proper bbox vector for map
# start by created a buffered boundary box around the aerial tracks
bbox_poly <- st_bbox(cs_polygon)

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_poly)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=19, provider = "Esri.WorldImagery", crop = TRUE)

cs_plot <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = cs, color = "white", lwd = 2, inherit.aes = FALSE) +
  theme_void() +
  ggtitle("Clewiston Spit 1") 
cs_plot

### Clewiston Spit 2 -------------------------------------------------------

# read in transects
cs2 <- st_read("2026/Data/nest_check_spatial_files/CS2_Track.gpx", layer = "tracks", quiet = TRUE)

# read in polygon of island
cs2_polygon <- st_read("2026/Data/nest_check_spatial_files/CS2.shp")

# Create a proper bbox vector for map
# start by created a buffered boundary box around the aerial tracks
bbox_poly <- st_bbox(cs2_polygon)

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_poly)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=19, provider = "Esri.WorldImagery", crop = TRUE)

cs2_plot <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = cs2, color = "white", lwd = 2, inherit.aes = FALSE) +
  theme_void() +
  ggtitle("Clewiston Spit 2") 
cs2_plot

### Rock Island 11 -------------------------------------------------------

# read in transects
ri11 <- st_read("2026/Data/nest_check_spatial_files/RI11_Track.gpx", layer = "tracks", quiet = TRUE)

# read in polygon of island
ri_polygon11 <- st_read("2026/Data/nest_check_spatial_files/RI11.shp")

# Create a proper bbox vector for map
# start by created a buffered boundary box around the aerial tracks
bbox_poly <- st_bbox(ri_polygon11)

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_poly)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=19, provider = "Esri.WorldImagery", crop = FALSE)

ri_plot11 <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = ri11, color = "white", lwd = 2, inherit.aes = FALSE) +
  theme_void() +
  ggtitle("Rock Island 11") 
ri_plot11

### Rock Island 12 -------------------------------------------------------

# read in transects
ri12 <- st_read("2026/Data/nest_check_spatial_files/RI12_Track.gpx", layer = "tracks", quiet = TRUE)

# read in polygon of island
ri_polygon12 <- st_read("2026/Data/nest_check_spatial_files/RI12.shp")

# Create a proper bbox vector for map
# start by created a buffered boundary box around the aerial tracks
bbox_poly <- st_bbox(ri_polygon12)

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_poly)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom=19, provider = "Esri.WorldImagery", crop = FALSE)

ri_plot12 <- ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = ri12, color = "white", lwd = 2, inherit.aes = FALSE) +
  theme_void() +
  ggtitle("Rock Island 12") 
ri_plot12


### Combine plots -------------------------------------------------------

# Add black border to each small plot
lbb_map_box <- lbb_plot + 
  theme_void() +
  theme(
    plot.background = element_rect(color = "#d95f02", size = 3, fill = NA),
    panel.background = element_rect(fill = NA),
    plot.title = element_text(hjust = 0.5),       # horizontally center title
    plot.title.position = "plot"                  # center relative to entire plot area
  )

cs_map_box <- cs_plot + 
  theme_void() +
  theme(
    plot.background = element_rect(color = "#1b9e77", size = 3, fill = NA),
    panel.background = element_rect(fill = NA),
    plot.title = element_text(hjust = 0.5),       # horizontally center title
    plot.title.position = "plot"                  # center relative to entire plot area
  )

cs2_map_box <- cs2_plot + 
  theme_void() +
  theme(
    plot.background = element_rect(color = "#1f78b4", size = 3, fill = NA),
    panel.background = element_rect(fill = NA),
    plot.title = element_text(hjust = 0.5),       # horizontally center title
    plot.title.position = "plot"                  # center relative to entire plot area
  )

ri11_map_box <- ri_plot11 + 
  theme_void() +
  theme(
    plot.background = element_rect(color = "#7570b3", size = 3, fill = NA),
    panel.background = element_rect(fill = NA),
    plot.title = element_text(hjust = 0.5),       # horizontally center title
    plot.title.position = "plot"                  # center relative to entire plot area
  )

ri12_map_box <- ri_plot12 + 
  theme_void() +
  theme(
    plot.background = element_rect(color = "#CC79A7", size = 3, fill = NA),
    panel.background = element_rect(fill = NA),
    plot.title = element_text(hjust = 0.5),       # horizontally center title
    plot.title.position = "plot"                  # center relative to entire plot area
  )

# Main map with black border
nest_locations_box <- nest_locations_plot +
  coord_sf(xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
           ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]), 
           expand = FALSE,   # adds a small buffer around geometries
           clip = "on") +  # prevent clipping
  theme_void() +
  theme(legend.position = "none")

# Combine maps with patchwork
combined_map <- nest_locations_box + 
  (ri11_map_box / ri12_map_box / cs_map_box) + (cs2_map_box / lbb_map_box) +  
  plot_layout(widths = c(2, 1, 1))  # main map 3/4 width, small maps 1/4

# Display
combined_map

ggsave("2026/Figures/Figure_2.jpeg", height=7.5, width=10, units="in")

# Hydrology ---------------------------------------------------

# start by summarizing rainfall data
rainfall <- read_csv("2026/Data/rainfall.csv") 

# define date column then filter to data before July 1st
rainfall_clean <- rainfall %>%
  mutate(date = parse_date_time(TIMESTAMP, orders = c("mdy")),
         month=month(date),
         day=day(date)) %>%
  filter(date < mdy("7/1/2026") &
           date >= mdy("1/1/2026"),
         complete.cases(date))

rainfall_sum <- rainfall_clean %>%
  group_by(month, day) %>%
  summarise(rainfall=mean(VALUE, na.rm=TRUE)) %>%
  mutate(month_day = make_date(2000, month, day))

ggplot(rainfall_sum, aes(x=month_day, y=rainfall)) +
  geom_col() 

# get historic rainfall
# define date column, create day and month columns and filter out months after June
rainfall_clean_hist <- rainfall %>%
  mutate(date=parse_date_time(TIMESTAMP, orders = c("mdy", "mdy_HM")),
         month=month(date),
         day=day(date)) %>%
  filter(month < 7,
         complete.cases(date))

rainfall_hist <- rainfall_clean_hist %>%
  filter(date >= mdy("1/1/2008") & date <= mdy("7/1/2025"),
         STATION %in% c("L001", "L005", "L006", "LZ40")) %>%
  group_by(month, day) %>%
  summarise(rainfall = mean(VALUE, na.rm = TRUE), .groups = "drop",
            rainfall_sd = sd(VALUE, na.rm = TRUE)) %>%
  mutate(month_day = make_date(2000, month, day))  %>%  
  filter(month_day != mdy("02/29/2000"))

# now let's summarize water level data
water <- read_csv("2026/Data/lake_stage.csv") 

# define date column, create day and month columns and filter out months after June
water_clean <- water %>%
  mutate(date=parse_date_time(TIMESTAMP, orders = c("mdy")),
         month=month(date),
         day=day(date)) %>%
  filter(month < 7,
         complete.cases(date))

# now, we will provide three summaries... water level this year,
# average water level since 2008, and average water level from 1977-2007 
# Add a dummy year to create a proper Date column
cur_hist <- water_clean %>%
  filter(date >= mdy("1/1/2026") & date < mdy("7/1/2026"),
         STATION %in% c("L001", "L005", "L006", "LZ40"),
         is.na(CODE)) %>%
  group_by(month, day) %>%
  summarise(water_level = mean(VALUE, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_day = make_date(2000, month, day))  # dummy year

rec_hist <- water_clean %>%
  filter(date >= mdy("1/1/2008") & date <= mdy("7/1/2025"),
         STATION %in% c("L001", "L005", "L006", "LZ40")) %>%
  group_by(month, day) %>%
  summarise(water_level = mean(VALUE, na.rm = TRUE), .groups = "drop",
            water_level_sd = sd(VALUE, na.rm = TRUE)) %>%
  mutate(month_day = make_date(2000, month, day))  %>%  
  filter(month_day != mdy("02/29/2000"))

# Combine datasets for plotting
water_stage_dat <- bind_rows(
  cur_hist %>% mutate(period = "2026 Lake Stage"),
  rec_hist %>% mutate(period = "Historic Lake Stage (2008-2025)"))

# remove feb 29 since this is a leap year date
water_stage_dat <- water_stage_dat %>%
  filter(month_day != mdy("02/29/2000"))

# Plot
ggplot(water_stage_dat, aes(x = month_day, y = water_level, color = period)) +
  geom_line() +
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 month") +
  labs(x = "Month-Day", y = "Mean Water Level", color = "Period") +
  theme_minimal()

# combine rainfall and water levels into one figure
# Find scaling factor between ranges

wl_min <- 10
wl_max <- 16
rf_min <- 0
rf_max <- 5

scale_factor <- (wl_max - wl_min) / (rf_max - rf_min)


ggplot() +
  # Rainfall bars
  geom_rect(data = rainfall_sum,
            aes(xmin = month_day - 0.5,
                xmax = month_day + 0.5,
                ymin = wl_min,
                ymax = wl_min + rainfall * scale_factor,
                linetype = "Rainfall"),  # for legend
            fill = "grey50", color = NA,
            show.legend = TRUE) +
  # Water level ribbon (±SD)
  geom_ribbon(data = rec_hist,
              aes(x = month_day,
                  ymin = water_level - water_level_sd,
                  ymax = water_level + water_level_sd),
              fill = "grey30",
              alpha = 0.2) +
  
  # Water level lines
  geom_line(data = water_stage_dat,
            aes(x = month_day, y = water_level, linetype = period),
            color = "black",
            lwd = 1.5) +
  
  scale_y_continuous(
    name = "Lake Stage (ft)",
    limits = c(wl_min, wl_max),
    sec.axis = sec_axis(~ (. - wl_min) / scale_factor,
                        name = "Rainfall (in)",
                        breaks = seq(0, 5, 1))
  ) +
  
  scale_linetype_manual(values = c(
    "2026 Lake Stage" = "solid",
    "Historic Lake Stage (2008-2025)" = "dotted",
    "Rainfall" = "solid"
  )) +
  
  labs(x = "", linetype = NULL) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw(base_size=24) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  guides(
    linetype = guide_legend(
      nrow = 4,
      byrow = TRUE,
      override.aes = list(
        fill = c(NA, NA, "grey50"),
        size = c(0.8, 0.8, 5)
      ),
      keywidth = unit(3, "cm")
    )
  )

ggsave("2026/Figures/Figure_6.jpeg", height=10, width=12, units="in")

## Recessions and Reversals ---------------------------------------------------

# here we will document recession rates and reversals of interest

# we will make a function to do this
# Function to calculate weekly slope in ft/week
weekly_slope <- function(data, start_date, end_date, period_name) {
  # Filter data for date range and period
  rec <- data %>%
    filter(month_day >= mdy(start_date) & month_day < mdy(end_date),
           period == period_name)
  
  # Check if there are at least 2 rows
  if (nrow(rec) < 2) {
    stop("Not enough data in this range to calculate slope.")
  }
  
  # Calculate slope: (first - last) / weeks
  slope <- (rec[1,]$water_level - rec[nrow(rec),]$water_level) /
    (as.numeric(difftime(rec[nrow(rec),]$month_day, rec[1,]$month_day, units = "days")) / 7)
  
  return(slope)
}

weekly_slope(water_stage_dat, start_date = "1/1/2000", end_date = "5/1/2000", period_name = "2026 Lake Stage")
weekly_slope(water_stage_dat, start_date = "1/1/2000", end_date = "5/1/2000", period_name = "Historic Lake Stage (2008-2025)")
weekly_slope(water_stage_dat, start_date = "5/1/2000", end_date = "7/1/2000", period_name = "2026 Lake Stage")

# daily slope in in/day
daily_slope <- function(data, start_date, end_date, period_name) {
  # Filter data for date range and period
  rec <- data %>%
    filter(month_day >= mdy(start_date) & month_day < mdy(end_date),
           period == period_name)
  
  # Check if there are at least 2 rows
  if (nrow(rec) < 2) {
    stop("Not enough data in this range to calculate slope.")
  }
  
  # Calculate slope: (first - last) / weeks
  slope <- (rec[1,]$water_level - rec[nrow(rec),]$water_level)*12 /
    (as.numeric(difftime(rec[nrow(rec),]$month_day, rec[1,]$month_day, units = "days")))
  
  return(slope)
}

# March and April recession rate
daily_slope(water_stage_dat, start_date = "3/1/2000", end_date = "4/30/2000", period_name = "2026 Lake Stage")

# March and April water level
ma_dat <- water_stage_dat %>%
               filter(period=="2026 Lake Stage",
                month_day >= ymd("2000-03-01") & month_day <= ymd("2000-04-30"))
mean(ma_dat$water_level)
  

rec_2021 <- water_clean %>%
  filter(date >= mdy("1/1/2021") & date <= mdy("7/1/2021"),
         STATION %in% c("L001", "L005", "L006", "LZ40")) %>%
  group_by(month, day) %>%
  summarise(water_level = mean(VALUE, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_day = make_date(2000, month, day),
         period="2021")  # dummy year

weekly_slope(rec_2021, start_date = "2/18/2000", end_date = "4/11/2000", period_name = "2021")

# ending water level
end_2026 <- water_stage_dat %>% filter(period=="2026 Lake Stage")
end_2026[nrow(end_2026),]$water_level

end_rec_hist <- water_stage_dat %>% filter(period=="Historic Lake Stage (2008-2025)")
end_rec_hist[nrow(end_rec_hist),]$water_level
end_rec_hist[nrow(end_rec_hist),]$water_level_sd

# total rainfall between Jan 1 and May 1
interest_rainfall <- rainfall_sum %>%
  filter(month_day >= mdy("1/1/2000") & month_day <= mdy("5/1/2000")) 
sum(interest_rainfall$rainfall)

# historic rainfall between Jan 1 and May 1
hist_rainfall <- rainfall_hist %>%
  filter(month_day >= mdy("1/1/2000") & month_day <= mdy("5/1/2000")) 
sum(hist_rainfall$rainfall)
sqrt(sum(hist_rainfall$rainfall_sd^2, na.rm=TRUE))


# Nest Survival and Productivity ---------------------------------------------------

# number of monitored nests 
length(unique(nest_data$`Nest ID`))

# some nests had multiple species that used them, so let's calculate total number of nests based on nest ID and species
nest_data %>%
  distinct(`Nest ID`, Species, `Colony Name`) %>%
  nrow()

# number of nests monitored by species
nest_data %>%
  distinct(`Nest ID`, Species, `Colony Name`) %>%
  group_by(Species) %>%
  summarise(
    number_of_nests = n(),
    `Colony Name` = paste(
      sort(unique(`Colony Name`)),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(number_of_nests))

# number of nests where we began monitoring at egg stage
egg_stage <- nest_data %>%
  filter(complete.cases(`Number of Eggs`))

(egg_stage %>%
    distinct(`Nest ID`, Species, `Colony Name`) %>%
    nrow()) /
  (nest_data %>%
  distinct(`Nest ID`, Species, `Colony Name`) %>%
  nrow()) * 100

# number of nests monitored by species starting at egg stage
egg_stage  %>%
  distinct(`Nest ID`, Species, `Colony Name`) %>%
  group_by(Species) %>%
  summarise(
    number_of_nests = n(),
    `Colony Name` = paste(
      sort(unique(`Colony Name`)),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(number_of_nests))

# number of eggs
egg_stage %>%
  mutate(
    individuals = rowSums(
      select(
        .,
        `Number of Eggs`,
        `Number of Chicks`,
        `Number of Fledglings`,
        `Number of Dead Eggs`,
        `Number of Dead Chicks`
      ),
      na.rm = TRUE
    ),
    Species=case_when(Species=="SNEG" ~ "SNEG/TRHE",
                      Species=="TRHE" ~ "SNEG/TRHE",
                      TRUE ~ Species)
  ) %>%
  group_by(`Nest ID`, Species, `Colony Name`) %>%
  summarise(
    individuals = max(individuals),
    .groups = "drop"
  ) %>%
  group_by(Species) %>%
  summarise(
    mean_indiv = mean(individuals, na.rm = TRUE),
    sd_indiv = sd(individuals, na.rm = TRUE),
    .groups = "drop"
  )

# number of nests monitored by species and colony
egg_stage %>%
  distinct(`Nest ID`, Species, `Colony Name`) %>%
  mutate(Species=ifelse(Species %in% c("SNEG", "TRHE"), "SNEG/TRHE", Species)) %>%
  group_by(Species, `Colony Name`) %>%
  summarise(
    number_of_nests = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(number_of_nests)) %>%
  arrange(Species) %>%
  print(n=30)

## Nest Initiation ---------------------------------------------------------

# to calculate nest initiation, we need to determine the hatch date then substract 
# 21 days for SNEG/TRHE, GLIB, and WHIB but 25 days for GREG and GBHE and 24 for ROSP
# hatch date will be the date between the day we observed the first hatchling and
# the previous nest check

# get nest initiation date by species
nest_hatched <- nest_data %>%
  filter(complete.cases(`Number of Chicks`)) %>%
  arrange(`Nest ID`, Date) %>%  # ensure it's ordered by date within each nest
  group_by(`Nest ID`, Species) %>%
  filter(Date == first(Date)) %>%
  slice(1) %>%  # In case there are ties on the first date
  ungroup() 
# let's only examine the data where there are just eggs, and no chicks/fledglings

previous_check <- nest_data %>%
  filter(complete.cases(`Number of Eggs`),
         is.na(`Number of Chicks`),
         is.na(`Number of Fledglings`)) %>%
  arrange(`Nest ID`, Date) %>%  # ensure it's ordered by date within each nest
  group_by(`Nest ID`, Species) %>%
  filter(Date == last(Date)) %>%
  slice(1) %>%  # In case there are ties on the first date
  ungroup() %>%
  dplyr::select(`Nest ID`, Species, Date) %>%
  rename(previous_check_date=Date)

nest_int <- left_join(nest_hatched, previous_check, by=c("Nest ID", "Species"))

# now get the mean date
nest_int$hatch_date <- nest_int$previous_check_date + 
  (nest_int$Date - nest_int$previous_check_date) / 2

# now substract hatch date by 21 for all species except GREG which should be subtracted by 25 and ROSP which should be substracted by 24
nest_int <- nest_int %>%
  mutate(initiation_date = case_when(
    Species == "GREG" ~ hatch_date - 25,
    Species == "GBHE" ~ hatch_date - 25,
    Species == "ROSP" ~ hatch_date - 24,
    TRUE              ~ hatch_date - 21
  )) %>%
  filter(complete.cases(initiation_date))

# now get a count of nests per week
# start by getting the mid week date
nest_int$week_mid_date <- floor_date(nest_int$initiation_date, unit = "week") + days(3)

# now summarise the count of nests by week
nest_week <- nest_int %>%
  group_by(Species, week_mid_date) %>%
  summarise(count=n()) %>%
  mutate(Species = recode(Species,
                          "GREG" = "Great Egret",
                          "SNEG" = "Snowy Egret",
                          "TRHE" = "Tricolored Heron",
                          "WHIB" = "White Ibis",
                          "GLIB" = "Glossy Ibis",
                          "ROSP" = "Roseate Spoonbill",
                          "CAEG" = "Cattle Egret",
                          "ANHI" = "Anhinga",
                          "GBHE" = "Great Blue Heron"))

# for plotting, we will just look at wetland wading bird species
nest_week_focal <- nest_week %>%
  filter(!Species %in% c("Cattle Egret", "Anhinga"))

# get the median date for each species
(median_nest <- nest_int %>%
  group_by(Species) %>%
  summarise(median=median(week_mid_date),
            iqr=IQR(week_mid_date)) %>%
  mutate(Species = recode(Species,
                          "GREG" = "Great Egret",
                          "SNEG" = "Snowy Egret",
                          "TRHE" = "Tricolored Heron",
                          "WHIB" = "White Ibis",
                          "GLIB" = "Glossy Ibis",
                          "ROSP" = "Roseate Spoonbill",
                          "CAEG" = "Cattle Egret",
                          "ANHI" = "Anhinga",
                          "GBHE" = "Great Blue Heron")))

median_nest_focal <- median_nest %>%
  filter(!Species %in% c("Cattle Egret", "Anhinga"))

# let's calculate average historic median
historic_timing <- read_csv("2026/Data/historic_timing.csv")

historic_timing <- historic_timing %>%
  # Convert Initiation_date to a Date (dummy year, 2026)
  mutate(Initiation_date2 = dmy(paste0(Initiation_date, "-2026"))) %>%
  # Extract month-day as numeric day-of-year
  mutate(day_of_year = yday(Initiation_date2)) %>%
  group_by(Species) %>%
  summarise(
    median_day = round(median(day_of_year, na.rm = TRUE))
  ) %>%
  mutate(median_date = as.Date(median_day - 1, origin = "2026-01-01")) %>%
  mutate(Species = recode(Species,
                          "GREG" = "Great Egret",
                          "SNEG" = "Snowy Egret",
                          "TRHE" = "Tricolored Heron",
                          "WHIB" = "White Ibis",
                          "SMHE" = "Small Herons",
                          "GLIB" = "Glossy Ibis",
                          "ROSP" = "Roseate Spoonbill")) 
historic_timing

# data on all species
median_nest %>%
  left_join(historic_timing, by="Species") %>%
  mutate(diff=median-median_date) %>%
  mutate(
    diff_days = round(abs(as.numeric(diff, units = "days"))),
    diff_label = case_when(
      as.numeric(diff, units = "days") > 0 ~ paste0("+", diff_days, " days\n from historic\naverage"),
      as.numeric(diff, units = "days") < 0 ~ paste0("-", diff_days, " days\n from historic\naverage"),
      as.numeric(diff, units = "days") == 0 ~ paste0(diff_days, " days\n from historic\naverage")
    ),
    diff_label = ifelse(Species %in% c("Great Blue Heron", "Roseate Spoonbill", "Glossy Ibis"),
                        "No historic\naverage\navailable",
                        diff_label)
  ) %>% 
  mutate(label_x = case_when(
    Species == "Roseate Spoonbill" ~ mdy("05/01/2026"),
    Species == "Glossy Ibis"       ~ mdy("03/09/2026"),
    Species == "Snowy Egret"       ~ mdy("02/20/2026"),
    Species == "Tricolored Heron"  ~ mdy("03/09/2026"),
    Species == "White Ibis"       ~ mdy("03/09/2026"),
    Species == "Great Blue Heron" ~ mdy("05/01/2026"),
    TRUE                           ~ mdy("04/12/2026")))

# data on focal species
diff_int <- median_nest_focal %>%
  left_join(historic_timing, by="Species") %>%
  mutate(diff=median-median_date) %>%
  mutate(
    diff_days = round(abs(as.numeric(diff, units = "days"))),
    diff_label = case_when(
      as.numeric(diff, units = "days") > 0 ~ paste0("+", diff_days, " days\n from historic\naverage"),
      as.numeric(diff, units = "days") < 0 ~ paste0("-", diff_days, " days\n from historic\naverage"),
      as.numeric(diff, units = "days") == 0 ~ paste0(diff_days, " days\n from historic\naverage")
    ),
    diff_label = ifelse(Species %in% c("Great Blue Heron", "Roseate Spoonbill", "Glossy Ibis"),
                        "No historic\naverage\navailable",
                        diff_label)
  ) %>% 
  mutate(label_x = case_when(
    Species == "Roseate Spoonbill" ~ mdy("05/01/2026"),
    Species == "Glossy Ibis"       ~ mdy("03/09/2026"),
    Species == "Snowy Egret"       ~ mdy("02/20/2026"),
    Species == "Tricolored Heron"  ~ mdy("03/09/2026"),
    Species == "White Ibis"       ~ mdy("03/09/2026"),
    Species == "Great Blue Heron" ~ mdy("05/01/2026"),
    TRUE                           ~ mdy("04/12/2026")))

diff_int

ggplot(nest_week_focal, aes(x = week_mid_date, y = count)) +
  geom_col(width = 5, fill="grey60") +
  # Facet by species
  facet_wrap(~ Species, scales = "free_y", ncol=2) +
  # Add vertical median lines
  geom_vline(
    data = median_nest_focal,
    aes(xintercept = median),
    color = "black",
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_text(
    data = diff_int,
    aes(
      x = label_x,
      y = Inf,  # Position at top of plot
      label = diff_label
    ),
    vjust = 1.2,  # Adjust vertical position slightly above plot area
    hjust = 0.3,
    inherit.aes = FALSE,
    size = 3.5
  ) +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d") +
  labs(x = "Week", y = "Count of Initiated Nests") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave("2026/Figures/Figure_5.jpeg", height=6, width=6.8, units="in")

## Nest Survival -----------------------------------------------------------

## Nests successfully fledged one bird -----------------------------------------------------------

# we will use the Mayfield method to calculate survival by colony 

# determine nest survival to fledgling
nest_sur <- nest_data %>%
  group_by(`Nest ID`, Species) %>%
  summarise(`Nest Status`=ifelse(any(`Nest Status` == "Empty"), "Empty", "Destroyed"),
            `Number of Fledglings`=ifelse(any(complete.cases(`Number of Fledglings`)), max(`Number of Fledglings`, na.rm=TRUE), NA),
            `Number of Chicks`=ifelse(any(complete.cases(`Number of Chicks`)), max(`Number of Chicks`, na.rm=TRUE), NA),
            `Number of Dead Eggs`=ifelse(any(complete.cases(`Number of Dead Eggs`)), max(`Number of Dead Eggs`, na.rm=TRUE), NA),
            `Number of Dead Chicks`=ifelse(any(complete.cases(`Number of Dead Chicks`)), max(`Number of Dead Chicks`, na.rm=TRUE), NA))

nest_survival <- nest_sur %>%
  filter(!Species %in% c("ANHI", "CAEG", "UNKN")) %>%
  group_by(`Nest ID`, Species) %>%
  filter(
    `Number of Fledglings` >= 1 |
      (`Nest Status` == "Empty" & `Number of Chicks` >= 1),
    !( !is.na(`Number of Chicks`) & !is.na(`Number of Dead Chicks`) & 
         `Number of Chicks` == `Number of Dead Chicks` )) %>%
  distinct(`Nest ID`, Species) 

# to start, we need to prepare the data so we have nest_id, species, start stage, end stage, days between
# checks, and fate. Fate is defined by nest_survival above
nest_survival_intervals <- nest_data %>%
  filter(!Species %in% c("CAEG", "ANHI")) %>%
  # Define stage hierarchy
  rowwise() %>%
  mutate(stage = case_when(
    `Number of Fledglings` > 0 ~ "fledgling",
    `Number of Chicks` > 0 ~ "hatchling",
    `Number of Eggs` > 0 ~ "eggs",
    TRUE ~ "unknown"
  )) %>%
  ungroup() %>%
  arrange(`Nest ID`, Date) %>%
  group_by(`Nest ID`, Species) %>%
  mutate(
    `Colony Name` = first(`Colony Name`),
    stage_start = stage,
    stage_end = lead(stage),
    days_between = as.numeric(lead(Date) - Date),
    # Assign fate based on survived nests vector
    fate = ifelse(
      paste(`Nest ID`, Species) %in%
        paste(nest_survival$`Nest ID`, nest_survival$Species),
      1, 0
    )
  ) %>%
  filter(!is.na(stage_end)) %>%
  ungroup() %>%
  mutate(Species = ifelse(Species %in% c("SNEG", "TRHE") | Species == "SNEG/TRHE", "SMHE", Species),
         Species = ifelse(Species == "SMWT", "SMHE", Species)) %>%
  select(Date, `Nest ID`, Species, stage_start, stage_end, days_between, fate)

nest_stage_summary <- nest_survival_intervals %>%
  # remove any unknown stages so they don't get picked as start or end
  filter(stage_start != "unknown", stage_end != "unknown") %>%
  group_by(`Nest ID`, Species, fate) %>%
  summarise(
    start_date = min(Date),
    end_date = max(Date + days_between),
    start_stage = first(stage_start[Date == min(Date)]),
    end_stage = first(stage_end[Date == max(Date)]),
    .groups = "drop"
  ) %>%
  mutate(days_between = as.numeric(end_date - start_date)) %>%
  select(`Nest ID`, Species, start_stage, end_stage, days_between, fate)

nest_stage_summary

# Count failures and total exposure
mayfield_results <- nest_stage_summary %>%
  group_by(Species) %>%
  summarise(
    total_exposure = sum(days_between, na.rm = TRUE),
    failures = sum(fate == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    daily_mortality = failures / total_exposure,
    daily_survival = 1 - daily_mortality,
    SE = sqrt(failures) / total_exposure
  )

mayfield_results

# from Frederick et al. (1989), we will use time to fledgling to get the survival rate
days_to_fledge <- data.frame(Species=c("WHIB", "GLIB", "SMHE", "GREG", "ROSP", "GBHE"),
                             days_to_fledgling = c(14+21, 14+21, 14+21, 21+25, 22+24, 21+25))

# joint this with mayfield_results
survival_calc <- left_join(mayfield_results, days_to_fledge, by=c("Species"))

survival_calc %>% 
  mutate(survival_rate = daily_survival^days_to_fledgling,
         survival_se = days_to_fledgling*(daily_survival^(days_to_fledgling-1))*SE,
         survival_rate = ifelse(Species %in% nest_survival$Species | Species == "SMHE", survival_rate, 0),
         survival_se = ifelse(Species %in% nest_survival$Species | Species == "SMHE", survival_se, 0)) %>%
  select(Species, survival_rate, survival_se)

### By colony ---------------------------------------------------------------------------------------

# create a function to calculate fledgling success by colony
calculate_mayfield_survival <- function(nest_data, colony_name) {
  
  # 1. Summarise nest outcomes by Nest ID and Species
  nest_sur <- nest_data %>%
    filter(`Colony Name` == colony_name) %>%
    group_by(`Nest ID`, Species) %>%
    summarise(
      `Nest Status` = ifelse(
        any(`Nest Status` == "Empty"),
        "Empty",
        "Destroyed"
      ),
      `Number of Fledglings` = ifelse(
        any(complete.cases(`Number of Fledglings`)),
        max(`Number of Fledglings`, na.rm = TRUE),
        NA
      ),
      `Number of Chicks` = ifelse(
        any(complete.cases(`Number of Chicks`)),
        max(`Number of Chicks`, na.rm = TRUE),
        NA
      ),
      `Number of Dead Eggs` = ifelse(
        any(complete.cases(`Number of Dead Eggs`)),
        max(`Number of Dead Eggs`, na.rm = TRUE),
        NA
      ),
      `Number of Dead Chicks` = ifelse(
        any(complete.cases(`Number of Dead Chicks`)),
        max(`Number of Dead Chicks`, na.rm = TRUE),
        NA
      ),
      .groups = "drop"
    )
  
  # 2. Identify nests that survived to fledgling
  nest_survival <- nest_sur %>%
    filter(!Species %in% c("ANHI", "CAEG", "UNKN")) %>%
    filter(
      `Number of Fledglings` >= 1 |
        (`Nest Status` == "Empty" & `Number of Chicks` >= 1),
      !(
        !is.na(`Number of Chicks`) &
          !is.na(`Number of Dead Chicks`) &
          `Number of Chicks` == `Number of Dead Chicks`
      )
    ) %>%
    distinct(`Nest ID`, Species)
  
  # 3. Build nest survival intervals
  nest_survival_intervals <- nest_data %>%
    filter(
      !Species %in% c("CAEG", "ANHI"),
      `Colony Name` == colony_name
    ) %>%
    rowwise() %>%
    mutate(
      stage = case_when(
        `Number of Fledglings` > 0 ~ "fledgling",
        `Number of Chicks` > 0 ~ "hatchling",
        `Number of Eggs` > 0 ~ "eggs",
        TRUE ~ "unknown"
      )
    ) %>%
    ungroup() %>%
    arrange(`Nest ID`, Date) %>%
    group_by(`Nest ID`, Species) %>%
    mutate(
      stage_start = stage,
      stage_end = lead(stage),
      days_between = as.numeric(lead(Date) - Date),
      
      # Fate requires both Nest ID AND Species to match
      fate = ifelse(
        paste(`Nest ID`, Species) %in%
          paste(nest_survival$`Nest ID`, nest_survival$Species),
        1,
        0
      )
    ) %>%
    filter(!is.na(stage_end)) %>%
    ungroup() %>%
    mutate(
      Species = ifelse(
        Species %in% c("SNEG", "TRHE", "SNEG/TRHE"),
        "SMHE",
        Species
      ),
      Species = ifelse(
        Species == "SMWT",
        "SMHE",
        Species
      )
    ) %>%
    select(
      Date, `Nest ID`, Species,
      stage_start, stage_end,
      days_between, fate
    )
  
  # 4. Collapse to start/end stages and total exposure
  nest_stage_summary <- nest_survival_intervals %>%
    filter(
      stage_start != "unknown",
      stage_end != "unknown"
    ) %>%
    group_by(`Nest ID`, Species, fate) %>%
    summarise(
      start_date = min(Date),
      end_date = max(Date + days_between),
      start_stage = first(stage_start[Date == min(Date)]),
      end_stage = first(stage_end[Date == max(Date)]),
      .groups = "drop"
    ) %>%
    mutate(
      days_between = as.numeric(end_date - start_date)
    ) %>%
    select(
      `Nest ID`, Species,
      start_stage, end_stage,
      days_between, fate
    )
  
  # 5. Calculate Mayfield estimates by species
  mayfield_results <- nest_stage_summary %>%
    group_by(Species) %>%
    summarise(
      total_exposure = sum(days_between, na.rm = TRUE),
      failures = sum(fate == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      daily_mortality = failures / total_exposure,
      daily_survival = 1 - daily_mortality,
      SE = sqrt(failures) / total_exposure
    )
  
  # 6. Days to fledgling from Frederick et al. (1989)
  days_to_fledge <- data.frame(
    Species = c(
      "WHIB",
      "GLIB",
      "SMHE",
      "GREG",
      "ROSP",
      "GBHE"
    ),
    days_to_fledgling = c(
      14 + 21,
      14 + 21,
      14 + 21,
      21 + 25,
      22 + 24,
      21 + 25
    )
  )
  
  # 7. Calculate final survival rate and SE
  survival_calc <- left_join(
    mayfield_results,
    days_to_fledge,
    by = "Species"
  ) %>%
    mutate(
      survival_rate = daily_survival^days_to_fledgling,
      survival_se = days_to_fledgling *
        (daily_survival^(days_to_fledgling - 1)) *
        SE,
      survival_rate = ifelse(Species %in% nest_survival$Species | Species == "SMHE", survival_rate, 0),
      survival_se = ifelse(Species %in% nest_survival$Species | Species == "SMHE", survival_se, 0)
    ) %>%
    select(
      Species,
      survival_rate,
      survival_se
    )
  
  return(survival_calc)
}

calculate_mayfield_survival(nest_data, "Little Bear Beach")
calculate_mayfield_survival(nest_data, "Clewiston Spit")
calculate_mayfield_survival(nest_data, "Clewiston Spit 2")
calculate_mayfield_survival(nest_data, "Rock Island 11")
calculate_mayfield_survival(nest_data, "Rock Island 12")

## Compare to historic average survival rates -------------------------------------------------------

# Compare to average rates from prevous annual reports
historic_success <- read_csv("2026/Data/historic_nest_success.csv")

historic_success <- historic_success %>%
  filter(complete.cases(Species)) %>%
  group_by(Species) %>%
  summarise(historic_survival=mean(Success)*100,
            historic_survival_dev=sd(Success)*100)
historic_success

## Incubation success -----------------------------------------------------------

# we will use the Mayfield method to calculate survival by colony 

# determine nest survival to fledgling
nest_sur <- nest_data %>%
  group_by(`Nest ID`, Species) %>%
  summarise(`Nest Status`=ifelse(any(`Nest Status` == "Empty"), "Empty", "Destroyed"),
            `Number of Fledglings`=ifelse(any(complete.cases(`Number of Fledglings`)), max(`Number of Fledglings`, na.rm=TRUE), NA),
            `Number of Chicks`=ifelse(any(complete.cases(`Number of Chicks`)), max(`Number of Chicks`, na.rm=TRUE), NA),
            `Number of Dead Eggs`=ifelse(any(complete.cases(`Number of Dead Eggs`)), max(`Number of Dead Eggs`, na.rm=TRUE), NA),
            `Number of Dead Chicks`=ifelse(any(complete.cases(`Number of Dead Chicks`)), max(`Number of Dead Chicks`, na.rm=TRUE), NA),
            .groups = "drop")

nest_survival <- nest_sur %>%
  filter(!Species %in% c("ANHI", "CAEG", "UNKN")) %>%
  group_by(`Nest ID`, Species) %>%
  filter(`Number of Chicks` >= 1 | `Number of Fledglings` >= 1,
         !( !is.na(`Number of Chicks`) & !is.na(`Number of Dead Chicks`) & 
              `Number of Chicks` == `Number of Dead Chicks` )) %>%
  distinct(`Nest ID`, Species) 

# to start, we need to prepare the data so we have nest_id, species, start stage, end stage, days between
# checks, and fate. Fate is defined by nest_survival above
nest_survival_intervals <- nest_data %>%
  filter(Species %in% c("SNEG", "TRHE", "WHIB", "GREG", "SMWT", "GLIB", "ROSP", "GBHE")) %>%
  # Define stage hierarchy
  rowwise() %>%
  mutate(stage = case_when(
    `Number of Fledglings` > 0 ~ "fledgling",
    `Number of Chicks` > 0 ~ "hatchling",
    `Number of Eggs` > 0 ~ "eggs",
    TRUE ~ "unknown"
  )) %>%
  ungroup() %>%
  arrange(`Nest ID`, Date) %>%
  group_by(`Nest ID`, Species) %>%
  mutate(
    `Colony Name` = first(`Colony Name`),
    stage_start = stage,
    stage_end = lead(stage),
    days_between = as.numeric(lead(Date) - Date),
    fate = ifelse(
      paste(`Nest ID`, Species) %in%
        paste(nest_survival$`Nest ID`, nest_survival$Species),
      1, 0
    )
  ) %>%
  filter(stage_start == "eggs", stage_end == "hatchling" | stage_end == "eggs") %>%   # <-- only egg → hatchling
  ungroup() %>%
  mutate(
    Species = ifelse(Species %in% c("SNEG", "TRHE"), "SMHE", Species),
    Species = ifelse(Species == "SMWT", "SMHE", Species)
  ) %>%
  select(Date, `Colony Name`, `Nest ID`, Species, stage_start, stage_end, days_between, fate)


nest_stage_summary <- nest_survival_intervals %>%
  group_by(`Nest ID`, Species, fate) %>%
  summarise(
    start_date = min(Date),
    end_date = max(Date + days_between),
    start_stage = first(stage_start[Date == min(Date)]),
    end_stage = first(stage_end[Date == max(Date)]),
    .groups = "drop"
  ) %>%
  mutate(days_between = as.numeric(end_date - start_date)) %>%
  select(`Nest ID`, Species, start_stage, end_stage, days_between, fate)

nest_stage_summary

# Count failures and total exposure
mayfield_results <- nest_stage_summary %>%
  group_by(Species) %>%
  summarise(
    total_exposure = sum(days_between, na.rm = TRUE),
    failures = sum(fate == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    daily_mortality = failures / total_exposure,
    daily_survival = 1 - daily_mortality,
    SE = sqrt(failures) / total_exposure
  )

mayfield_results

# get incubation time for each species
# get the survival rate
days_to_hatching <- data.frame(Species=c("WHIB", "GLIB", "SMHE", "GREG", "ROSP", "GBHE"),
                             days_to_hatching = c(21, 21, 21, 25, 24, 25))

# joint this with mayfield_results
survival_calc <- left_join(mayfield_results, days_to_hatching, by=c("Species"))

survival_calc %>% 
  mutate(survival_rate = daily_survival^days_to_hatching,
         survival_se = days_to_hatching*(daily_survival^(days_to_hatching-1))*SE,
         survival_rate = ifelse(Species %in% nest_survival$Species | Species == "SMHE", survival_rate, 0),
         survival_se = ifelse(Species %in% nest_survival$Species | Species == "SMHE", survival_se, 0)) %>%
  select(Species, survival_rate, survival_se)

## Total nest initiations against recession rate ---------------------------------------------------

# In this section, we will examine the trend between recession rate and initiation date
# to do this, we will be utilizting the weekly_slope function created earlier

# The year "2000" is used as a dummy year, data is not from the year 2000

# start by creating a sequence of weeks that go from Sun-Sat
# define start and end of the whole period
start_period <- mdy("1/6/2000")
end_period   <- mdy("6/28/2000")

# generate weekly start dates
weekly_starts <- seq.Date(from = start_period, to = end_period, by = "7 days")

# generate corresponding end dates (6 days after start)
weekly_ends <- weekly_starts + days(6)

weekly_results <- map2_dfr(weekly_starts, weekly_ends, ~{
  tibble(
    start_date = .x,
    end_date   = .y,
    recession_rate = weekly_slope(
      water_stage_dat,
      start_date = format(.x, "%m/%d/%Y"),
      end_date   = format(.y, "%m/%d/%Y"),
      period_name = "2026 Lake Stage"
    )
  )
})
weekly_results

# get median date
weekly_results <- weekly_results %>%
  mutate(week_mid_date = start_date + (end_date - start_date) / 2) %>%
  mutate(week_mid_date = update(week_mid_date, year = 2025))

# get total nest initiations per week
nest_int_rate <- nest_week_focal %>%
  group_by(week_mid_date) %>%
  summarize(count=sum(count))

# join with nest initiation data
nest_int_wl <- left_join(weekly_results %>%
                           select(week_mid_date, recession_rate),
                         nest_int_rate, 
                         by=c("week_mid_date"))

# moving window recession rate
# start by creating a sequence of weeks that go from Sun-Sat
# define start and end of the whole period
start_period <- mdy("1/1/2000")
end_period   <- mdy("6/17/2000")

# generate weekly start dates
start_dates <- seq.Date(from = start_period, to = end_period, by = "1 days")

# generate corresponding end dates (6 days after start)
end_dates <- start_dates + days(13)

weekly_results <- map2_dfr(start_dates, end_dates, ~{
  tibble(
    start_date = .x,
    end_date   = .y,
    recession_rate = weekly_slope(
      water_stage_dat,
      start_date = format(.x, "%m/%d/%Y"),
      end_date   = format(.y, "%m/%d/%Y"),
      period_name = "2026 Lake Stage"
    )
  )
})
weekly_results

# get median date
weekly_results <- weekly_results %>%
  mutate(week_mid_date = start_date + (end_date - start_date) / 2) %>%
  mutate(week_mid_date = update(week_mid_date, year = 2026)) %>%
  select(week_mid_date, recession_rate)

# make the plot

# join datasets
plot_dat <- weekly_results %>%
  left_join(nest_int_rate, by = "week_mid_date")

# fix total_count scale to 0–50
count_max <- 50
rec_min <- min(plot_dat$recession_rate, na.rm = TRUE)
rec_max <- max(plot_dat$recession_rate, na.rm = TRUE)

scale_factor <- (rec_max - rec_min) / count_max

ggplot(plot_dat, aes(x = week_mid_date)) +
  # Bars for total_count (baseline at rec_min)
  geom_rect(aes(xmin = week_mid_date - 3.5, 
                xmax = week_mid_date + 3.5,
                ymin = rec_min, 
                ymax = rec_min + count * scale_factor),
            fill = "grey30", alpha = 0.5) +
  # Line for recession_rate
  geom_line(aes(y = recession_rate), color = "black", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size=0.7) +
  scale_y_continuous(
    name = "Two Week Recession Rate (ft/wk)",
    sec.axis = sec_axis(~ (. - rec_min) / scale_factor,
                        name = "Nest Initiations (per week)",
                        breaks = seq(0, 50, 10))
  ) +
  scale_x_date(
    date_breaks = "1 month",        # break every month
    date_labels = "%b"           # show as "Mar 2025", "Apr 2025", etc.
  ) +
  labs(x = "Date") +
  theme_bw(base_size = 14)

ggsave("2026/Figures/Figure_7.jpeg", height=5, width=7, units="in")

# Historic Nest Counts ---------------------------------------------------

# read in data
historic <- read_csv("2026/Data/Historic_Nest_Counts_Lake_O.csv")
max_counts <- read_csv("2026/Tables/Table_2.csv")

# format this years data to add to the historic counts
nest_counts_cur <- max_counts %>%
  pivot_longer(GREG:ANHI, names_to = "Wading Bird Code", values_to = "Number")

# read in species codes table
species_codes <- read_csv("2026/Data/species_codes.csv")

nest_counts_cur <- left_join(nest_counts_cur, species_codes, by=c("Wading Bird Code"))

# add colony type and select relevant data columns
nest_counts_cur <- nest_counts_cur %>%
  mutate(`Colony Type` = case_when(`Colony Name` == "Clewiston Spit 1" ~ "Spoil Island",
                                             `Colony Name` == "Clewiston Spit 2" ~ "Spoil Island",
                                             `Colony Name` == "Eagle Bay Island" ~ "Marsh Island",
                                             `Colony Name` == "Lemkin Creek" ~ "Off-Lake Island",
                                             `Colony Name` == "Gun Range" ~ "Off-Lake Island",
                                             `Colony Name` == "Rock Island 5" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 6" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 7" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 8" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 9" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 10" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 11" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 12" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 13" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 14" ~ "Spoil Island",
                                             `Colony Name` == "Rock Island 15" ~ "Spoil Island",
                                             `Colony Name` == "Little Bear Beach" ~ "Spoil Island",
                                             TRUE ~ NA_character_ ),
         `Report Year`=2026) %>%
  select(`Report Year`, `Colony Name`, `Colony Type`, Species, Number)

# select relevant columns from the historic data
historic_clean <- historic %>%
  mutate(`Report Year`=as.numeric(`Report Year`),
         Number=as.numeric(Number)) %>%
  select(`Report Year`, `Colony Name`, `Colony Type`, Species, Number)

nest_counts_hist <- rbind(nest_counts_cur, historic_clean)

# wood storks
gator_farm <- nest_counts_hist %>% 
  filter(`Colony Name`=="Gator Farm", 
         !Species %in% c("Wood Stork", "Anhinga", "Cattle Egret")) %>% 
  group_by(`Report Year`) %>% 
  summarise(count=sum(Number, na.rm=TRUE))
mean(gator_farm[gator_farm$`Report Year`!="2026",]$count)

# save this data, so we can append to it again in the future
# write_csv(nest_counts_hist, "2026/Data/Historic_Nest_Counts_Lake_O_with_2026.csv")

# let's remove non-wading bird species from the dataset
nest_counts_hist <- nest_counts_hist %>%
  filter(!Species %in% c("Cattle Egret", "Double-crested Cormorant", "Anhinga"))

# now let's get the total nest counts by Colony Type and Year
# remove colony type off-lake island since that is not of interest
nest_col_year <- nest_counts_hist %>%
  filter(`Colony Type` != "Off-Lake Island") %>%
  group_by(`Report Year`, `Colony Type`) %>%
  summarise(nest_counts=sum(Number, na.rm=TRUE))

# add 0 counts for years where a colony type was not found 
nest_col_year_complete <- nest_col_year %>%
  ungroup() %>%  # <-- remove grouping
  tidyr::complete(`Report Year`, `Colony Type`, fill = list(nest_counts = 0)) %>%
  arrange(`Report Year`, `Report Year`)

# plot the data
ggplot(nest_col_year_complete, aes(x = `Report Year`, y = nest_counts, color = `Colony Type`)) +
  geom_line(size = 1.2) +
  geom_point(
    data = subset(nest_col_year, `Report Year` == 2026),
    aes(x = `Report Year`, y = nest_counts, color = `Colony Type`),
    size = 6,        
    shape = 17,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = unique(nest_col_year$`Report Year`)) +
  scale_color_manual(values = c(
    "Marsh Island" = "#E69F00",
    "Spoil Island" = "#0072B2"
  )) +
  labs(
    x = "Year",
    y = "Number of Nests",
    color = "Colony Type"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),   # title
    axis.title = element_text(size = 16),                                # x/y axis labels
    axis.text = element_text(size = 14),                                 # tick labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("2026/Figures/Figure_8.jpeg", height=4, width=8, units="in")

## Historic summary of ROSP and WOST ----------------------------------

# Roseate Spoonbills and Wood Storks are of conservation concern, so let's compare these year's counts of these species
# to previous years

# observed ROSP
sum(max_counts$ROSP)

# number of colonies where ROSP were observed
nrow(max_counts %>% filter(ROSP > 0))

# average count of ROSP 
rosp_hist <- historic %>%
  filter(Species == "Roseate Spoonbill", Number != "P") %>%
  mutate(Number = as.numeric(Number)) %>%
  group_by(`Report Year`) %>%
  summarise(count = sum(Number), .groups = "drop") %>%
  complete(`Report Year` = seq(min(historic$`Report Year`), 
                               max(historic$`Report Year`)), 
           fill = list(count = 0)) %>%
  rename(report_year=`Report Year`)

mean(rosp_hist$count)
sd(rosp_hist$count)

# what is the mean in the last 5 years of historic data?
rosp_hist_rec <- rosp_hist %>%
  filter(report_year %in% c(2021:2025))

mean(rosp_hist_rec$count)
sd(rosp_hist_rec$count)

# average count of WOST 
wost_hist <- historic %>%
  filter(Species == "Wood Stork", Number != "P") %>%
  mutate(Number = as.numeric(Number)) %>%
  group_by(`Report Year`) %>%
  summarise(count = sum(Number), .groups = "drop") %>%
  complete(`Report Year` = seq(min(historic$`Report Year`), 
                               max(historic$`Report Year`)), 
           fill = list(count = 0)) %>%
  rename(report_year=`Report Year`)

mean(wost_hist$count)
sd(wost_hist$count)

# what is the mean in the last 5 years of historic data?
wost_hist_rec <- wost_hist %>%
  filter(report_year %in% c(2021:2025))
mean(wost_hist_rec$count)
sd(wost_hist_rec$count)

# Area of Willow index ----------------------------------------------------

# Area of willow index calculation
calc_willow_index <- function(year, data) {
  
  ref_date <- as.Date(paste0(year - 1L, "-01-01"))
  
  window_730 <- data %>%
    filter(
      date < ref_date,
      date >= ref_date - lubridate::days(730)
    )
  
  if (nrow(window_730) < 730) {
    return(NA_real_)
  }
  
  high_thresh <- 15.4
  low_thresh  <- 12.8
  
  high_days <- sum(window_730$stage >= high_thresh, na.rm = TRUE)
  low_days  <- sum(window_730$stage <= low_thresh, na.rm = TRUE)
  
  dplyr::case_when(
    high_days >= 540 ~ 1,
    low_days  >= 170 ~ 3,
    TRUE             ~ 2
  )
}

# read in lake stage data
water <- read_csv("2026/Data/lake_stage.csv") 

water_willow_index <- water %>%
  select(TIMESTAMP, VALUE) %>%
  rename(stage = VALUE) %>%
  mutate(
    date = as.Date(parse_date_time(TIMESTAMP, orders = "mdy"))
  ) %>%
  filter(
    !is.na(date),
    date >= as.Date("2005-01-01")
  ) %>%
  group_by(date) %>%
  summarise(
    stage = mean(stage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Year = year(date)
  ) %>%
  arrange(date)

willow_index_df <- water_willow_index %>%
  distinct(Year) %>%
  arrange(Year) %>%
  mutate(
    willow_index = map_dbl(Year, calc_willow_index, data = water_willow_index)
  )

# now add to historic nesting data
historic_annual <- historic %>%
  filter(!Species %in% c("Double-crested Cormorant", 
                         "Cattle Egret",
                         "Anhinga")) %>%
  group_by(`Report Year`) %>%
  summarise(wading_birds=sum(Number, na.rm=TRUE))

willow_index_df <- left_join(willow_index_df, historic_annual, by=c("Year"="Report Year"))

willow_index_df <- willow_index_df %>%
  filter(Year > 2007) %>%
  # add data for 2026
  mutate(wading_birds=case_when(Year == 2026 ~ sum(max_counts$Total, na.rm = TRUE),
                                TRUE ~ wading_birds))

ggplot(willow_index_df, 
       aes(x = factor(willow_index), y = wading_birds)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, fill="#E69F00") +
  geom_jitter(width = 0.1, height = 0, size = 2.5, alpha = 0.7) +
  labs(
    x = "Area of Willow Index",
    y = "Nest Count"
  ) +
  theme_classic(base_size=16)

ggsave("2026/Figures/Figure_11.jpeg", height=4, width=6, units="in")




