# Figure 1 follows the human-authored R_scripts/archive/10_Map.Rmd.
# The original map input is restricted to fires in the current annual model;
# study metadata supplies coordinates where that earlier map had no fire entry.
library(tidyverse)
library(here)
library(sf)

root <- here("agent_workflows", "vibe_coding")
figure_dir <- file.path(root, "output/paper/figures/figure_1_maps")
table_dir <- file.path(root, "output/paper/tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# The original script mapped fire coordinates from Map_input.csv. Identify
# which of those fires still contribute burned watersheds to the current fits.
model_table <- read_csv(file.path(root, "data/derived/lasso_model_table.csv"),
                        show_col_types = FALSE) %>%
  filter(is.finite(lnRR_mean))
current_fires <- model_table %>%
  group_by(Study_ID, Fire_ID_Analysis) %>%
  summarise(number_of_burn = n_distinct(Pair_Burn),
            n_pairs = n_distinct(candidate_pair_id),
            analytes = paste(sort(unique(response_var)), collapse = " + "),
            .groups = "drop")

original_map <- read_csv(here("inputs", "Studies_Summary", "Map_input.csv"),
                         na = c("-9999", "N/A"), show_col_types = FALSE) %>%
  select(Study_ID, Fire_name, latitude, longitude) %>%
  mutate(latitude = parse_number(as.character(latitude)),
         longitude = parse_number(as.character(longitude)))
original_matches <- current_fires %>%
  left_join(original_map, by = "Study_ID", relationship = "many-to-many") %>%
  mutate(fire_key = Fire_ID_Analysis %>%
           str_remove("_[0-9]{4}$") %>%
           str_remove_all("[^[:alnum:]]") %>%
           str_to_lower(),
         map_key = Fire_name %>%
           str_remove_all("[^[:alnum:]]") %>%
           str_to_lower()) %>%
  filter(!is.na(map_key), str_detect(map_key, fixed(fire_key))) %>%
  group_by(Study_ID, Fire_ID_Analysis) %>%
  slice(1) %>%
  ungroup() %>%
  select(Study_ID, Fire_ID_Analysis,
         Fire_name_map = Fire_name,
         latitude_map = latitude, longitude_map = longitude)

study_metadata <- read_csv(file.path(root, "data/source/Sites_meta_data.csv"),
                           show_col_types = FALSE,
                           locale = locale(encoding = "Latin1")) %>%
  select(Study_ID, latitude, longitude) %>%
  mutate(latitude = parse_number(as.character(latitude)),
         longitude = parse_number(as.character(longitude)),
         latitude = if_else(between(latitude, -90, 90), latitude, NA_real_),
         longitude = if_else(between(longitude, -180, 180), longitude, NA_real_)) %>%
  distinct(Study_ID, .keep_all = TRUE)

# Rhea's study-level coordinate describes Hayman and lies outside High Park.
# Use the local High Park fire boundary for that missing original-map point.
high_park_boundary <- st_read(here(
  "gis_data/Fire_Perimeters/high_park/mtbs/2012/co4058910540420120609/high_park_burn_bndy.shp"),
  quiet = TRUE)
high_park_center <- high_park_boundary %>%
  st_union() %>%
  st_centroid() %>%
  st_transform(4269) %>%
  st_coordinates()

coords <- current_fires %>%
  left_join(original_matches, by = c("Study_ID", "Fire_ID_Analysis")) %>%
  left_join(study_metadata, by = "Study_ID") %>%
  mutate(Fire_name = coalesce(
           Fire_name_map,
           str_replace_all(str_remove(Fire_ID_Analysis, "_[0-9]{4}$"), "_", " ")),
         coordinate_source = if_else(!is.na(latitude_map) & !is.na(longitude_map),
                                     "Original Map_input.csv",
                                     "Study metadata (approximate)"),
         latitude = coalesce(latitude_map, latitude),
         longitude = coalesce(longitude_map, longitude),
         coordinate_source = if_else(Study_ID == "Rhea et al. 2021" &
                                       Fire_ID_Analysis == "High_Park_Fire_2012",
                                     "High Park boundary centroid", coordinate_source),
         latitude = if_else(Study_ID == "Rhea et al. 2021" &
                              Fire_ID_Analysis == "High_Park_Fire_2012",
                            high_park_center[1, "Y"], latitude),
         longitude = if_else(Study_ID == "Rhea et al. 2021" &
                               Fire_ID_Analysis == "High_Park_Fire_2012",
                             high_park_center[1, "X"], longitude)) %>%
  select(Study_ID, Fire_ID_Analysis, Fire_name, latitude, longitude,
         number_of_burn, n_pairs, analytes, coordinate_source) %>%
  arrange(Study_ID, Fire_ID_Analysis)
if (any(!is.finite(coords$latitude) | !is.finite(coords$longitude))) {
  stop("Current fires lacking map coordinates: ",
       paste(coords$Fire_ID_Analysis[!is.finite(coords$latitude) |
                                       !is.finite(coords$longitude)], collapse = ", "))
}

# Rhea and Writer sampled different watersheds in the same High Park fire.
# Preserve both study records, but show the wildfire only once on the map.
fire_locations <- coords %>%
  group_by(Fire_ID_Analysis) %>%
  summarise(Study_ID = paste(sort(unique(Study_ID)), collapse = "; "),
            Fire_name = first(Fire_name),
            latitude = if_else(first(Fire_ID_Analysis) == "High_Park_Fire_2012",
                               high_park_center[1, "Y"], first(latitude)),
            longitude = if_else(first(Fire_ID_Analysis) == "High_Park_Fire_2012",
                                high_park_center[1, "X"], first(longitude)),
            number_of_burn = sum(number_of_burn),
            n_pairs = sum(n_pairs),
            analytes = first(analytes),
            coordinate_source = if_else(
              first(Fire_ID_Analysis) == "High_Park_Fire_2012",
              "High Park boundary centroid", first(coordinate_source)),
            .groups = "drop") %>%
  arrange(Fire_ID_Analysis)
write_csv(coords, file.path(table_dir, "figure_1_study_fire_records.csv"))
write_csv(fire_locations, file.path(table_dir, "figure_1_map_locations.csv"))

# Original projection and map layers: white U.S. states and Canadian provinces.
laea_proj <- paste("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0",
                   "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
coordinates_sf <- st_as_sf(fire_locations, coords = c("longitude", "latitude"),
                           crs = 4269)
us_states <- st_read(here("inputs/map_shape_files/us_states/cb_2021_us_state_20m.shp"),
                     quiet = TRUE) %>%
  st_transform(laea_proj) %>%
  filter(!NAME %in% c("Hawaii", "Puerto Rico")) %>%
  select(geometry)
canadian_provinces <- st_read(
  file.path(root, "data/source/ne_50m_admin_1_states_provinces.geojson"),
  quiet = TRUE) %>%
  filter(admin == "Canada") %>%
  st_transform(laea_proj) %>%
  select(geometry)
us_ca <- bind_rows(us_states, canadian_provinces)

# This is the manual palette in the original Figure 1 code. Two names may
# share a color, as they did in that figure.
original_colors <- c("#E31A1C", "#A6CEE3", "#00AFBB", "#1B9E77",
                     "#7570B3", "#E7298A", "darkred", "black",
                     "#1F78B4", "#E31A1C", "#33A02C", "black",
                     "darkblue", "darkgreen", "blue", "yellow",
                     "red", "green", "purple")
fire_names <- sort(unique(fire_locations$Fire_name))
fire_colors <- setNames(original_colors[seq_along(fire_names)], fire_names)

# ggspatial is absent from the active workflow environment, so the original
# scale and north arrow annotations are drawn directly in projected meters.
bar_origin <- st_transform(st_sfc(st_point(c(-81, 25)), crs = 4269), laea_proj)
bar_xy <- st_coordinates(bar_origin)[1, ]
bar_height <- 45000
bar_polygons <- st_sfc(
  st_polygon(list(rbind(bar_xy, bar_xy + c(500000, 0),
                        bar_xy + c(500000, bar_height),
                        bar_xy + c(0, bar_height), bar_xy))),
  st_polygon(list(rbind(bar_xy + c(500000, 0), bar_xy + c(1000000, 0),
                        bar_xy + c(1000000, bar_height),
                        bar_xy + c(500000, bar_height),
                        bar_xy + c(500000, 0)))), crs = laea_proj)
scale_bar <- st_sf(fill = c("black", "white"), geometry = bar_polygons)
scale_labels <- st_sf(
  label = c("0", "500", "1,000 km"),
  geometry = st_sfc(lapply(c(0, 500000, 1000000), function(offset)
    st_point(bar_xy + c(offset, -90000))), crs = laea_proj))
north_origin <- st_transform(st_sfc(st_point(c(-66, 62)), crs = 4269), laea_proj)
north_xy <- st_coordinates(north_origin)[1, ]
north_shaft <- st_sfc(st_linestring(rbind(north_xy + c(0, -120000),
                                          north_xy + c(0, 190000))), crs = laea_proj)
north_tip <- st_sfc(st_polygon(list(rbind(north_xy + c(0, 290000),
                                           north_xy + c(-65000, 145000),
                                           north_xy + c(65000, 145000),
                                           north_xy + c(0, 290000)))), crs = laea_proj)
north_label <- st_sf(label = "N", geometry = st_sfc(
  st_point(north_xy + c(0, 405000)), crs = laea_proj))

# Original Figure 1 plot structure, updated with the current fires and counts.
figure <- ggplot() +
  geom_sf(data = us_ca, fill = "white", color = "black", linewidth = .22) +
  geom_sf(data = coordinates_sf,
          aes(size = number_of_burn, color = Fire_name)) +
  scale_color_manual(values = fire_colors,
                     guide = guide_legend(title = "Fire Name", ncol = 1,
                                          override.aes = list(size = 3), order = 1)) +
  scale_size_continuous(range = c(1.8, 6.5), breaks = 1:5,
                        name = "Number of Burned Watersheds",
                        guide = guide_legend(ncol = 1, order = 2)) +
  geom_sf(data = scale_bar, aes(fill = fill), color = "black",
          linewidth = .25, show.legend = FALSE) +
  scale_fill_identity() +
  geom_sf_text(data = scale_labels, aes(label = label), size = 2.6) +
  geom_sf(data = north_shaft, color = "black", linewidth = .55) +
  geom_sf(data = north_tip, fill = "black", color = "black") +
  geom_sf_text(data = north_label, aes(label = label), size = 3.4,
               fontface = "bold") +
  coord_sf(crs = st_crs(laea_proj),
           xlim = c(-4700000, 3300000), ylim = c(-2800000, 4300000),
           expand = FALSE) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 8))

ggsave(file.path(figure_dir, "figure_1_maps.png"), figure,
       width = 14, height = 9, dpi = 300, bg = "white")
message("Saved Figure 1 for ", n_distinct(model_table$Study_ID), " studies and ",
        nrow(fire_locations), " unique fires.")
