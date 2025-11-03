# The goal is to find bus routes with long headways and then
# to find their less active endpoints, which should represent
# rural and remote settlements with poor bus connections.
# Optional: also exclude enpoints with train stations in 500 m buffer zone.

#----start----
# import libraries
library(tidyverse)
library(tidytransit)
library(sf)
library(leaflet)
library(gtfsrouter)
library(ggspatial)
library(rosm)
library(osmextract)
library(rnaturalearth)
#install.packages("osmextract")

# import GTFS
gtfs_path <- "~/30days/gtfs.zip"
gtfs <- read_gtfs(gtfs_path)

#----1. filter bus routes on thursday---
# select one day
target_date <- as.Date("2025-10-23")
target_dow <- tolower(weekdays(target_date))

# normal planned services
services_planned <- gtfs$calendar %>%
  filter(
    !!sym(target_dow) == 1,
    start_date <= target_date,
    end_date >= target_date
  ) %>%
  pull(service_id)

# additional services
services_added <- gtfs$calendar_dates %>%
  filter(
    date == target_date,
    exception_type == 1
  ) %>%
  pull(service_id)

# cancelled services
services_cancelled <- gtfs$calendar_dates %>%
  filter(
    date == target_date,
    exception_type == 2
  ) %>%
  pull(service_id)

# all services on this day
services_all <- setdiff(
  union(services_planned, services_added),
  services_cancelled
)

# only bus routes
gtfs_filtered <- gtfs
gtfs_filtered$routes <- gtfs$routes %>%
  filter(route_type == 3)

# only trips belonging to planned services AND bus routes
gtfs_filtered$trips <- gtfs$trips %>%
  filter(
    service_id %in% services_planned,
    route_id %in% gtfs_filtered$routes$route_id
  )

# only stop_times belonging to those trips
gtfs_filtered$stop_times <- gtfs$stop_times %>%
  filter(trip_id %in% gtfs_filtered$trips$trip_id)

#----2. headways and other frequency indicators----
gtfs_full <- gtfs
gtfs <- gtfs_filtered

# first stop times
stop_times_first <- gtfs$stop_times %>%
  mutate(departure_secs = period_to_seconds(hms(departure_time))) %>%
  left_join(gtfs$trips %>% select(trip_id, route_id),
            by = "trip_id") %>%
  group_by(trip_id) %>%
  slice_min(stop_sequence, n = 1) %>%
  ungroup()

# headways
headways <- stop_times_first %>%
  group_by(route_id) %>%
  arrange(departure_secs) %>%
  mutate(
    headway_min = (departure_secs - lag(departure_secs)) / 60
  ) %>%
  filter(!is.na(headway_min), headway_min > 0)

# frequency indicators
route_headways <- headways %>%
  group_by(route_id) %>%
  summarise(
    mean_headway = mean(headway_min),
    median_headway = median(headway_min),
    min_headway = min(headway_min),
    max_headway = max(headway_min),
    n_departures = n() + 1,  # because we lose one trip when differencing
    .groups = "drop"
  ) %>%
  left_join(gtfs_filtered$routes %>% select(route_id, route_short_name, route_type),
            by = "route_id") %>%
  left_join(gtfs_filtered$routes %>% select(route_id, agency_id), by = "route_id") %>%
  left_join(gtfs_filtered$agency %>% select(agency_id, agency_name), by = "agency_id")

#----3. select endpoints----
# routes with low frequency
route_low_freq <- route_headways %>%
  filter(agency_name != "Flixbus" & agency_name != "-") %>%
  filter(median_headway >= 60 & n_departures <= 10)

# select route_ids
routes_selected <- route_low_freq %>%
  pull(route_id)

# stop_times
low_freq_stop_times <- gtfs$stop_times %>%
  inner_join(gtfs$trips %>% select(trip_id, route_id),
             by = "trip_id") %>%
  filter(route_id %in% routes_selected)

# enpoints
endpoints <- low_freq_stop_times %>%
  group_by(route_id, trip_id) %>%
  summarise(
    first_stop_id = first(stop_id[order(as.numeric(stop_sequence))]),
    last_stop_id  = last(stop_id[order(as.numeric(stop_sequence))]),
    .groups = "drop"
  ) %>%
  group_by(route_id) %>%
  slice(1) %>%   # keep just one trip per route
  ungroup() %>%
  filter(first_stop_id != last_stop_id)

endpoints <- endpoints %>%
  pivot_longer(
    cols = c(first_stop_id, last_stop_id),
    names_to = "first_last",
    values_to = "stop_id"
  ) %>%
  mutate(first_last = ifelse(first_last == "first_stop_id", "first", "last"))

# routes per stop
routes_per_stop <- gtfs$stop_times %>%
  left_join(gtfs$trips %>% select(trip_id, route_id),
            by = "trip_id") %>%
  distinct(stop_id, route_id) %>%
  group_by(stop_id) %>%
  summarise(n_routes = n(), .groups = "drop")

# add number of routes
endpoints <- endpoints %>%
  left_join(routes_per_stop, by = "stop_id")

# stop points with least number of routes
points <- endpoints %>%
  group_by(route_id) %>%
  filter(n_routes == min(n_routes)) %>% 
  ungroup()

#----4. combine and visualise----
# combine points with
points <- points %>%
  left_join(route_headways, by = "route_id")

# stop names and coords
points <- points %>%
  left_join(
    gtfs$stops %>%
      select(stop_id, stop_name, stop_lat, stop_lon),
    by = "stop_id"
  )

# check if these stop names are met twice. if so, exclude
duplicate_names <- gtfs$stops %>%
  group_by(stop_name) %>%
  summarise(n_ids = n_distinct(stop_id), .groups = "drop") %>%
  filter(n_ids > 1) %>%
  pull(stop_name)

points1 <- points %>%
  filter(!stop_name %in% duplicate_names)

# check if these points are inside a 500 m buffer from a train station
stations <- gtfs_full$routes %>%
  filter(route_type == 2) %>%
  left_join(gtfs_full$trips, by = "route_id") %>%
  left_join(gtfs_full$stop_times, by = "trip_id") %>%
  distinct(stop_id) %>%
  left_join(gtfs_full$stops, by = "stop_id") %>%
  select(stop_id, stop_name, stop_lon, stop_lat)

stations1 <- stations %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(25832)

buffers <- st_buffer(stations1, dist = 500) %>%
  st_union()

points2 <- points1 %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(25832)

hits <- st_intersects(points2, buffers)
inside <- lengths(hits) > 0
points3 <- points2[inside, ] %>%
  st_transform(4326) 

# country plot
ggplot() +
  annotation_map_tile(type = "osm") +
  geom_point(
    data = points1,
    aes(x = as.numeric(stop_lon), y = as.numeric(stop_lat)),
    color = "red", size = 1, alpha = 0.8
  ) +
  coord_sf(default_crs = sf::st_crs(4326), expand = FALSE) +
  theme_void() +  # removes axes, grid, background
  theme(
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Low-Frequency Route Endpoints",
    subtitle = "Endpoints with few connecting routes"
  )

#----5. zoom on Lower Saxony----
# Lower Saxony polygon
lower_saxony <- ne_states(country = "Germany", returnclass = "sf") %>%
  filter(name_en == "Lower Saxony")

# points to sf
points_sf <- points1 %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE)

# only points inside Lower Saxony
points_ls <- points_sf[lower_saxony, ]

# bbox
bbox <- st_bbox(st_as_sf(points_ls, coords = c("stop_lon", "stop_lat"), crs = 4326))

# highways within bbox
oe_providers()
oe_match("Niedersachsen")
match <- oe_match("Niedersachsen")
match$url

roads <- oe_get(
  "Niedersachsen",
  provider = "geofabrik",
  layer = "lines", 
  extra_tags = "highway"
)

roads <- roads[!is.na(roads$highway), ]
roads_simple <- st_simplify(roads, dTolerance = 0.001)

main_roads <- roads_simple %>%
  filter(highway %in% c("motorway", "trunk", "primary"#, "secondary", "tertiary"
))

# boundary
ls_boundary <- ne_states(country = "Germany", returnclass = "sf") %>%
  filter(name == "Niedersachsen")

# black plot
p <- ggplot() +

  geom_sf(data = ls_boundary, fill = NA, color = "grey30", size = 0.4) +

  geom_sf(data = main_roads, aes(color = highway), size = 0.3, alpha = 0.6) +
  scale_color_manual(values = c(
    motorway = "grey70",
    trunk = "grey40",
    primary = "grey25"
  ), guide = "none") +

  geom_sf(
    data = points_ls,
    #aes(x = as.numeric(stop_lon), y = as.numeric(stop_lat)),
    color = "gold", size = 1.3, alpha = 0.9
  ) +
  geom_sf(
    data = points_ls,
    #aes(x = as.numeric(stop_lon), y = as.numeric(stop_lat)),
    color = "yellow", size = 0.6, alpha = 0.6
  ) +

  coord_sf(default_crs = st_crs(4326), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "white"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey70"),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey70",
                                face = "italic", margin = margin(t = 10))
  ) +
  labs(
    title = "Low-frequency bus route endpoints in Lower Saxony",
    subtitle = "Stops where your bus will come in 60 minutes or later",
    caption = "Sources: GTFS data (2025), OpenStreetMap contributors"
  )

ggsave(filename = "1-points.jpg",
       width = 2000,
       height = 1600,
       dpi = 300,
       units = "px"
)
