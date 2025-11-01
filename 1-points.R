# import libraries
library(tidyverse)
library(tidytransit)
library(sf)
library(leaflet)
library(gtfsrouter)

# import GTFS
gtfs_path <- "~/30days/gtfs.zip"
gtfs <- read_gtfs(gtfs_path)

# select one day
target_date <- as.Date("2025-10-23")
target_dow <- tolower(weekdays(target_date))

calendar_active <- gtfs$calendar %>%
  filter(
    !!sym(target_dow) == 1,
    start_date <= target_date,
    end_date >= target_date
  ) %>%
  pull(service_id)

calendar_dates_active <- gtfs$calendar_dates %>%
  filter(
    date == target_date,
    exception_type == 1
  ) %>%
  pull(service_id)

calendar_dates_removed <- gtfs$calendar_dates %>%
  filter(
    date == target_date,
    exception_type == 2
  ) %>%
  pull(service_id)

active_services <- setdiff(union(calendar_active, calendar_dates_active),
                           calendar_dates_removed)

gtfs_filtered <- gtfs
gtfs_filtered$trips <- gtfs$trips %>%
  filter(service_id %in% active_services)

gtfs_filtered$stop_times <- gtfs$stop_times %>%
  filter(trip_id %in% gtfs_filtered$trips$trip_id)

# find headways
gtfs_full <- gtfs
gtfs <- gtfs_filtered
stop_times <- gtfs$stop_times %>%
  mutate(
    departure_secs = period_to_seconds(hms(departure_time))
  )

stop_times <- stop_times %>%
  left_join(
    gtfs$trips %>%
      select(trip_id, route_id),
    by = "trip_id"
  ) %>%
  group_by(trip_id) %>%
  slice_min(stop_sequence, n = 1) %>% # first stops
  ungroup()

headways <- stop_times %>%
  group_by(route_id) %>%
  arrange(departure_secs) %>%
  mutate(headway_min = (departure_secs - lag(departure_secs)) / 60) %>%
  filter(!is.na(headway_min), headway_min > 1) %>%
  left_join(gtfs$routes %>% select(route_id, agency_id, route_short_name, route_type),
           by = "route_id") %>%
  filter(route_type == 3)

headways %>%
  group_by(headway_min) %>%
  summarise(n = n()) %>%
  filter(n > 5)

route_headways <- headways %>%
  group_by(route_id, route_short_name, agency_id) %>%
  summarise(
    mean_headway = mean(headway_min, na.rm = TRUE),
    median_headway = median(headway_min, na.rm = TRUE),
    min_headway = min(headway_min, na.rm = TRUE),
    max_headway = max(headway_min, na.rm = TRUE),
    n_departures = n()
  ) %>%
  arrange(mean_headway)

table(route_headways$n_departures)

route_headways_sel <- route_headways %>%
  filter(n_departures =< 10 ) %>%
  left_join(agencies, by = "agency_id") %>%
  select(route_short_name, agency_name, n_departures) %>%
  filter(agency_name = "")

View(route_headways_sel %>%
  group_by(agency_name) %>%
  summarise(n = n()))

agencies <- gtfs$agency
names(agencies)

hist(headways_filtered$headway_hour,
     breaks = 50,
     main = "Distribution of route headways",
     xlab = "Headway (hours)")

route_id <- tibble(route_id = headways_filtered$route_id)
route_ids <- route_id %>%
  st_drop_geometry() %>%
  left_join(gtfs$routes %>% select(route_id, route_type),
            join_by(route_id)) %>%
  filter(route_type == 3) %>% # bus
  distinct(route_id) %>%
  pull(route_id)

length(route_ids)

# Function to visualise on Leaflet
plot_gtfs_routes <- function(gtfs, route_ids) {
  required <- c("routes", "trips", "stops", "stop_times")
  if (!all(required %in% names(gtfs))) {
    stop("GTFS must include tables: routes, trips, stops and stop_times")
  }

  trips_sel <- gtfs$trips %>%
    filter(route_id %in% route_ids)

  stop_times_sel <- gtfs$stop_times %>%
    filter(trip_id %in% trips_sel$trip_id) %>%
    left_join(gtfs$stops, by = "stop_id") %>%
    arrange(trip_id, stop_sequence) %>%
    mutate(
      stop_lat = as.numeric(stop_lat),
      stop_lon = as.numeric(stop_lon)
    ) %>%
    filter(!is.na(stop_lat) & !is.na(stop_lon))

  # build geometries
  routes_sf <- stop_times_sel %>%
    group_by(trip_id) %>%
    summarise(
      geometry = st_sfc(st_linestring(as.matrix(cbind(stop_lon, stop_lat)))),
      .groups = "drop"
    ) %>%
    mutate(route_id = trips_sel$route_id[match(trip_id, trips_sel$trip_id)]) %>%
    st_as_sf(crs = 4326) %>%
    group_by(route_id) %>%
    summarise(geometry = st_combine(geometry), .groups = "drop")  # use st_combine instead of st_union

  pal <- colorFactor(rainbow(length(unique(routes_sf$route_id))),
                     domain = routes_sf$route_id)

  leaflet(routes_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(color = ~pal(route_id),
                 weight = 3,
                 opacity = 0.9,
                 label = ~paste("Route ID:", route_id)) %>%
    addCircleMarkers(data = gtfs$stops %>%
                       mutate(
                         stop_lat = as.numeric(stop_lat),
                         stop_lon = as.numeric(stop_lon)
                       ) %>%
                       filter(!is.na(stop_lat) & !is.na(stop_lon)),
                     lng = ~stop_lon, lat = ~stop_lat,
                     radius = 3, color = "blue", opacity = 0.6,
                     label = ~stop_name) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~route_id,
              title = "Routes",
              opacity = 1)
}



route_ids[1]

# Apply the function
plot_gtfs_routes(gtfs, route_ids[1])

  