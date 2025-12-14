library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(readr)

crs_proj <- 26915

valid_trips <- readRDS("../Dashboard/data/valid_trips.rds")
trip_proximity <- readRDS("../Dashboard/data/trip_proximity.rds")
same_route_trips_expanded <- readRDS("../Dashboard/data/same_route_trips_expanded.rds")
start_segments <- readRDS("../Dashboard/data/start_points.rds") 
end_segments <- readRDS("../Dashboard/data/end_points.rds") 
stops_sf <- readRDS("../Dashboard/data/stops_sf.rds")
road_centerlines <- readRDS("../Dashboard/data/road_centerlines.rds")
tracts_census_sf <- readRDS("../Dashboard/data/tracts_census_sf.rds")
boardings_sf <- readRDS("../Dashboard/data/stop_boardings_sf.rds")
stop_times <- readRDS("../Dashboard/data/stop_times.rds")
trips_gtfs <- readRDS("../Dashboard/data/trips_gtfs.rds")
routes_gtfs <- readRDS("../Dashboard/data/routes_gtfs.rds")
google_results <- readRDS("../Data/google_results.rds")
google_results_wide <- readRDS("../Data/google_results_wide.rds")
google_results_long <- readRDS("../Data/google_results_long.rds")

if (!dir.exists("data/app")) dir.create("data/app")

start_segments   <- st_as_sf(start_segments)   %>%  st_transform(crs_proj)
end_segments     <- st_as_sf(end_segments)     %>%  st_transform(crs_proj)
stops_sf         <- st_as_sf(stops_sf)         %>%  st_transform(crs_proj)
road_centerlines <- st_as_sf(road_centerlines) %>%  st_transform(crs_proj)
tracts_census_sf <- st_as_sf(tracts_census_sf) %>%  st_transform(crs_proj)
boardings_sf     <- st_as_sf(boardings_sf)     %>%  st_transform(crs_proj)

start_points <- start_segments %>% 
  mutate(geometry = st_centroid(start_geometry)) %>% 
  select(trip_id, geometry)

end_points <- end_segments %>% 
  mutate(geometry = st_centroid(end_geometry)) %>% 
  select(trip_id, geometry)

start_points_ll <- st_transform(start_points, 4326)
end_points_ll   <- st_transform(end_points,   4326)

start_points_map <- st_centroid(start_segments) %>%  st_transform(4326)
end_points_map   <- st_centroid(end_segments)   %>%  st_transform(4326)
stops_ll         <- st_transform(stops_sf, 4326)

#  Behaviour classification
sub_one_ids <- unique(same_route_trips_expanded$trip_id)

behavior_df <- trip_proximity %>% 
  mutate(
    behavior_type = case_when(
      trip_type == "sub_candidate" & trip_id %in% sub_one_ids ~ "sub_one",
      trip_type == "sub_candidate" & !trip_id %in% sub_one_ids ~ "sub_multi",
      TRUE ~ trip_type
    )
  )

trips_with_behavior <- valid_trips %>% 
  left_join(behavior_df %>%  select(trip_id, behavior_type), by = "trip_id") %>% 
  mutate(
    start_time = suppressWarnings(ymd_hms(StartTime)),
    date       = as_date(start_time),
    hour       = hour(start_time),
    weekday    = wday(start_time, label = TRUE, week_start = 1),
    day_type   = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  ) %>% 
  filter(!is.na(start_time))

trips_core <- trips_with_behavior %>% 
  select(trip_id, behavior_type, start_time, date, hour,
         weekday, day_type, TripDuration)

behavior_counts <- trips_core %>% 
  count(behavior_type) %>% 
  mutate(
    pct = n / sum(n),
    behavior_type = factor(
      behavior_type,
      levels = c("none", "first_mile", "last_mile", "sub_multi", "sub_one")
    )
  )

hourly_normalized <- trips_core %>% 
  group_by(day_type, behavior_type, date, hour) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(day_type, behavior_type, hour) %>% 
  summarise(avg_trips = mean(n), .groups = "drop")

hourly_share <- trips_core %>% 
  group_by(day_type, date, hour, behavior_type) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(day_type, date, hour) %>% 
  mutate(hour_total = sum(n),
         share      = n / hour_total) %>% 
  group_by(day_type, behavior_type, hour) %>% 
  summarise(avg_share = mean(share), .groups = "drop")

saveRDS(behavior_counts,    "data/app/app_behavior_counts.rds")
saveRDS(hourly_normalized,  "data/app/app_hourly_normalized.rds")
saveRDS(hourly_share,       "data/app/app_hourly_share.rds")

# Distance to nearest stop, downsample
start_points_beh <- start_points %>% 
  left_join(behavior_df %>% select(trip_id, behavior_type), by = "trip_id")

nearest_idx <- st_nearest_feature(start_points_beh, stops_sf)

dist_to_stop <- st_distance(start_points_beh, stops_sf[nearest_idx, ], by_element = TRUE)

start_points_beh <- start_points_beh %>% 
  mutate(dist_to_nearest_stop = as.numeric(dist_to_stop))

# Drop geometry + heavy size, keep sample per behaviour
set.seed(123)

start_points_beh_small <- start_points_beh %>% 
  group_split(behavior_type, .keep = TRUE) %>% 
  lapply(function(df) {
    n_rows <- nrow(df)
    if (n_rows <= 20000) {
      df
    } else {
      df[sample.int(n_rows, 20000), , drop = FALSE]
    }
  }) %>% 
  bind_rows()

saveRDS(
  start_points_beh_small %>%  st_drop_geometry(),
  "data/app/start_points_beh_small.rds"
)


# Leakinees
stop_routes <- stop_times %>% 
  select(trip_id, stop_id) %>% 
  distinct()  %>% 
  left_join(trips_gtfs %>% select(trip_id, route_id), by = "trip_id") %>%
  distinct(stop_id, route_id)

stop_boardings <- boardings_sf %>%
  st_drop_geometry() %>%
  select(stop_id, boardings)

route_boardings <- stop_routes %>%
  left_join(stop_boardings, by = "stop_id") %>%
  group_by(route_id) %>%
  summarise(route_boardings = sum(boardings, na.rm = TRUE), .groups = "drop")

route_sub_trips <- same_route_trips_expanded %>%
  distinct(trip_id, route_id) %>%
  count(route_id, name = "route_sub_trips")

route_stats <- route_boardings %>%
  left_join(route_sub_trips, by = "route_id") %>%
  left_join(routes_gtfs %>% select(route_id, route_short_name, route_long_name),
            by = "route_id") %>%
  mutate(
    route_sub_trips = if_else(is.na(route_sub_trips), 0L, route_sub_trips),
    leakiness_raw   = if_else(route_boardings > 0,
                              route_sub_trips / route_boardings,
                              NA_real_)
  )

# Only compare busy routes so 700 express routes downtown don't explode the ratio
busy_threshold <- quantile(route_stats$route_boardings, 0.5, na.rm = TRUE)

route_stats <- route_stats %>%
  mutate(is_busy = route_boardings >= busy_threshold)

top_leaky <- route_stats %>%
  filter(
    is_busy,
    route_sub_trips >= 20,
    !is.na(leakiness_raw)
  ) %>%
  arrange(desc(leakiness_raw)) %>%
  slice_head(n = 10)

priority_routes <- route_stats %>%
  filter(
    is_busy,
    route_sub_trips >= 20,
    !is.na(leakiness_raw)
  ) %>%
  mutate(
    priority_score = leakiness_raw * log1p(route_boardings)
  ) %>%
  arrange(desc(priority_score)) %>%
  slice_head(n = 10)

saveRDS(top_leaky,       "data/app/app_top_leaky.rds")
saveRDS(priority_routes, "data/app/app_priority_routes.rds")

# Tract level stats and priority score
start_points_cent <- st_centroid(start_points_beh)

start_in_tracts <- st_join(
  start_points_cent,
  tracts_census_sf,
  join = st_within
)

trips_by_tract <- start_in_tracts %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(
    all_trips     = n(),
    sub_one_trips = sum(behavior_type == "sub_one", na.rm = TRUE),
    gap_trips     = sum(behavior_type == "none",    na.rm = TRUE),
    .groups = "drop"
  )

tracts_stats <- tracts_census_sf %>%
  left_join(trips_by_tract, by = "GEOID") %>%
  mutate(
    all_trips     = if_else(is.na(all_trips),     0L, all_trips),
    sub_one_trips = if_else(is.na(sub_one_trips), 0L, sub_one_trips),
    gap_trips     = if_else(is.na(gap_trips),     0L, gap_trips),
    sub_share     = if_else(all_trips > 0, sub_one_trips / all_trips, NA_real_),
    gap_share     = if_else(all_trips > 0, gap_trips     / all_trips, NA_real_)
  )

stops_by_tract <- st_join(tracts_stats, boardings_sf) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(total_boardings = sum(boardings, na.rm = TRUE), .groups = "drop")

tracts_stats <- tracts_stats %>%
  left_join(stops_by_tract, by = "GEOID") %>%
  mutate(
    total_boardings   = if_else(is.na(total_boardings), 0, total_boardings),
    area_km2          = as.numeric(st_area(tracts_stats)) / 1e6,
    boardings_density = if_else(area_km2 > 0,
                                total_boardings / area_km2,
                                NA_real_)
  )

# Priority scores
tracts_priority <- tracts_stats |>
  filter(
    !is.na(gap_share),
    !is.na(median_income),
    !is.na(zero_car_share),
    !is.na(boardings_density),
    !is.na(population)
  ) |>
  mutate(
    d_gap    = ntile(gap_share,          10),  
    d_zero   = ntile(zero_car_share,     10),  
    d_income = ntile(-median_income,     10),  
    d_trans  = ntile(-boardings_density, 10),  
    d_pop    = ntile(population,         10)   
  ) |>
  mutate(
    # give extra weight to gap_share and income
    gap_priority_score = 1.5 * d_gap +
      1.5 * d_income +
      d_zero +
      d_trans +
      d_pop
  ) |>
  arrange(desc(gap_priority_score))

priority_gap_tracts <- tracts_priority %>%
  slice_head(n = 10) %>%
  st_drop_geometry() %>%
  select(
    GEOID,
    population,
    median_income,
    zero_car_share,
    boardings_density,
    gap_share,
    gap_priority_score
  )

# Priority scores (substitution)
tracts_priority_sub <- tracts_stats %>%
  filter(!is.na(sub_share),
         !is.na(population),
         !is.na(boardings_density)) %>%
  mutate(
    d_sub   = ntile(sub_share,      10),
    d_pop   = ntile(population,     10),
    d_trans = ntile(boardings_density, 10)
  ) %>%
  mutate(
    sub_priority_score = d_sub + d_pop + d_trans
  ) %>%
  arrange(desc(sub_priority_score))

priority_sub_tracts <- tracts_priority_sub %>%
  slice_head(n = 10) %>%
  st_drop_geometry() %>%
  select(GEOID, population, median_income,
         boardings_density, sub_share, sub_priority_score)

saveRDS(priority_gap_tracts, "data/app/app_priority_gap_tracts.rds")
saveRDS(priority_sub_tracts,"data/app/app_priority_sub_tracts.rds")

# ---- Simplified tracts for maps ----
tracts_stats_small <- tracts_stats %>%
  filter(all_trips > 0) %>% 
  select(GEOID, sub_share, gap_share,
         population, median_income, zero_car_share,
         boardings_density, geometry)

tracts_ll_small <- tracts_stats_small %>%
  st_transform(4326) %>%
  st_simplify(dTolerance = 50, preserveTopology = TRUE)

saveRDS(tracts_ll_small, "data/app/app_tracts_ll_small.rds")

# ---- Substitution explorer: downsample per route ----
set.seed(123)

same_route_small <- same_route_trips_expanded %>%
  group_split(route_short_name, .keep = TRUE) %>%
  lapply(function(df) {
    n_rows <- nrow(df)
    if (n_rows <= 1000) {
      df
    } else {
      df[sample.int(n_rows, 1000), , drop = FALSE]
    }
  }) %>%
  bind_rows()

ids_keep <- unique(same_route_small$trip_id)

start_points_map_small <- start_points_ll %>%
  dplyr::filter(trip_id %in% ids_keep)

end_points_map_small <- end_points_ll %>%
  dplyr::filter(trip_id %in% ids_keep)

# ---- Google API travel-time summaries ----

google_wide <- google_results_long %>%
  filter(mode %in% c("scooter", "transit")) %>%
  select(trip_id, mode, duration_min) %>%
  tidyr::pivot_wider(
    names_from = mode,
    values_from = duration_min,
    names_prefix = "time_"
  )

trips_with_times <- trips_with_behavior |>
  left_join(google_wide, by = "trip_id")

time_by_behavior <- trips_with_times |>
  group_by(behavior_type) |>
  summarise(
    n_with_times   = sum(!is.na(time_scooter) & !is.na(time_transit)),
    median_scooter = median(time_scooter, na.rm = TRUE),
    median_transit = median(time_transit, na.rm = TRUE),
    median_diff    = median(time_transit - time_scooter, na.rm = TRUE),
    .groups        = "drop"
  )

saveRDS(time_by_behavior, "../Dashboard/data/app/app_time_by_behavior.rds")

time_diff_dist <- trips_with_times |>
  filter(!is.na(time_scooter), !is.na(time_transit)) |>
  mutate(
    diff_min = time_transit - time_scooter, # + = transit slower
  ) |>
  select(trip_id, behavior_type, diff_min)

saveRDS(time_diff_dist, "../Dashboard/data/app/app_time_diff_dist.rds")



saveRDS(same_route_small,       "data/app/app_same_route_small.rds")
saveRDS(start_points_map_small, "data/app/app_start_points_map_small.rds")
saveRDS(end_points_map_small,   "data/app/app_end_points_map_small.rds")
saveRDS(stops_ll,               "data/app/app_stops_ll.rds")

cat("Done. Small app_* RDS files written to data/app/\n")

