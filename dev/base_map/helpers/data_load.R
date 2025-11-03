##### helpers/data_load.R #####

# --- 埼玉県全域ポリゴンの読み込み ---
saitama_all <- st_read("data/admin_all.gpkg", quiet = TRUE) %>% st_transform(4326)

get_saitama_all <- function() {
  saitama_all
}

# --- その他のポリゴンの読み込み ---
admin <- st_read("data/admin.gpkg", quiet = TRUE) %>% st_transform(4326)
land_use_mesh <- st_read("data/land_use_mesh_en.gpkg", quiet = TRUE) %>% st_transform(4326)
stream <- st_read("data/stream.gpkg", quiet = TRUE) %>% st_transform(4326)
watershed <- st_read("data/watershed.gpkg", quiet = TRUE) %>% st_transform(4326)
plant_dist <- st_read("data/plant_sample_dwc.gpkg", quiet = TRUE)

# # --- ポリゴンの表示確認() ---
# ggplot() +
#   geom_sf(data = saitama_all, colour = "grey") +
#   geom_sf(data = admin, colour = "red", linewidth = 0.2) +
#   geom_sf(data = land_use_mesh, colour = "red4", fill = "transparent", linewidth = 0.1) +
#   geom_sf(data = watershed, colour = "brown", fill = "transparent", linewidth = 0.1) +
#  geom_sf(data = stream, colour = "blue", linewidth = 0.2)


plant_dist <- plant_dist %>%
  mutate(
    year = lubridate::year(eventDate),
    decade = case_when(
      year < 1990 ~ "1980s以前",
      year < 2000 ~ "1990s",
      year < 2010 ~ "2000s",
      year < 2020 ~ "2010s",
      TRUE ~ "2020s以降"
    )
  )
