### Calculations

# install package 'pacman'

pacman::p_load(
  tidyverse,
  here,
  readxl,
  viridis,
  knitr,
  networkD3,
  sf,
  edc,
  excel.link,
  future,
  future.apply,
  data.table
)

# Pland energy production

prod_eo <- 173.1 #GWh/year
prod_fv <- 95.7 #GWh/year

source( # loads 'depreciation' functions (make sure the file is stored in the same folder or adjust accordingly)
  here(
    "script",
    "paper_landscape_valuation",
    "depreciation_functions_24.R")
)

# Loading Italian - English correspondence table

table_path <- 
  here( # change the path if needed
    "script",
    "paper_landscape_valuation",
    "corr_table_it_en.xlsx"
  )

table_list <- 
  excel_sheets(
    table_path
  )

corr_tables_l <- 
  map(
    table_list,
    \(x) read_excel(
      table_path,
      sheet = x
    )
  ) |> setNames(table_list)

# Loading landscape classes values

paes_class_translation_vct <- # creo vettore dei nomi delle classi in inglese
  setNames(
    corr_tables_l$classi_cu_ca$classe_en,
    corr_tables_l$classi_cu_ca$classe_it
  )

paes_tipo_translation_vct <- 
  setNames(
    corr_tables_l$tipi_paes$tipo_pae_en,
    corr_tables_l$tipi_paes$tipo_pae_it
  )

wtp <- read_xlsx( # loading landscape values / WTP
  here(
    "script",
    "paper_landscape_valuation",
    "landscape_values_24.xlsx"),
  sheet = "valori_paesaggi_storici" # Sheet with landscape values taking into account the value of historical landscapes

) |> 
 mutate(
   classe_cu = recode(classe_cu, !!!paes_class_translation_vct),
   classe_na = recode(classe_na, !!!paes_class_translation_vct)
 )

# Loading ISPRA map with landscape types

shape.data_paes <- sf::st_read(here(
  "script", 
  "paper_landscape_valuation",
  "carta_paesaggio", 
  "CN250_UdP.shp"),
                               quiet = T) |> 
  mutate(TIPO_PAE = recode(
    TIPO_PAE,
    !!!paes_tipo_translation_vct
  ))

# Loading ISPRA 'Nature' map

shape.data_cnat <- sf::st_read(here("script", 
                                    "paper_landscape_valuation",
                                    "carta_natura",
                                    "CartaNaturalisticoCulturale.shp"),
                               quiet = T) |> 
  mutate(
    TIPO_PAE = recode(
      TIPO_PAE,
      !!!paes_tipo_translation_vct
    ),
    across(contains("classe"), \(x) recode(
      x,
      !!!paes_class_translation_vct
    )),
    across(contains("classe"),
           \(x) factor(x, levels = corr_tables_l$classi_cu_ca$classe_en))
  )
    
# Loading kml file of historical rural landscapes

kml_files <- list.files(path = here("script", 
                                    "paper_landscape_valuation",
                                    "traditional_landscapes", "kml"), pattern = "\\.kml$", full.names = TRUE)

sf_objects <- lapply(kml_files, function(file) {
  sf_obj <- st_read(file)
  if (is.na(st_crs(sf_obj))) {
    st_crs(sf_obj) <- 4326 # WGS84 CRS
  }
  return(sf_obj)
})

sf_objects <- lapply(sf_objects, function(sf_obj) {
  st_transform(sf_obj, crs = st_crs(shape.data_cnat)) # Converts to WGS84
})

combined_sf <- do.call(rbind, sf_objects)
st_crs(combined_sf) <- st_crs(shape.data_cnat) 

# Filtering POLYGON or MULTIPOLYGON objects only
combined_sf_filtered <- combined_sf[st_geometry_type(combined_sf) %in% c("POLYGON", "MULTIPOLYGON"), ]
shape.paes.trad <- st_cast(combined_sf_filtered, "MULTIPOLYGON") |> 
  select(!Description)

# Intersection between historical rural landscapes map and ISPRA map with landscape types

intersection_paes_trad <- st_intersection(shape.data_cnat, shape.paes.trad) |> 
  mutate(classe_rs = "sì")

# gemetries that do not intersect

non_intersecting_polygons <- st_difference(shape.data_cnat, st_union(shape.paes.trad)) |> #geometrie che non intersecano
  mutate(classe_rs = "no")

shape.data_cnat.paes <- # shapefile intersection
  rbind(select(intersection_paes_trad, !Name), non_intersecting_polygons)

# Adding landscape values to the shapefile 

shape.data_cnat.paes <- 
  shape.data_cnat.paes |> 
  left_join(wtp) |> 
  mutate(area_ha = as.numeric(st_area(geometry)) / 10^4, # geometry area in hectares
         wtp_abs = as.numeric(wtp * area_ha) / 10^6) # landscape values (million euro)

# Loading PV visibility data

visibility_data_fv <- 
  read_delim(
    file = here::here("script", 
                      "paper_landscape_valuation",
                      "visibility_analysis", "fv_lato15km_100m.csv"),
    delim = ","
  )

visibility_data_shape_fv <- # creating a shapfile from visibility data
  visibility_data_fv |> 
  mutate(geometry = pmap(list(left, bottom, right, top), function(left, bottom, right, top) {
    st_polygon(list(matrix(c(left, bottom, right, bottom, right, top, left, top, left, bottom),
                           ncol = 2, byrow = TRUE)))
  })) |> 
  st_as_sf() 

# intersection between visibility data and the landscape value map
st_crs(visibility_data_shape_fv) <- st_crs(shape.data_cnat.paes)

intersection_fv <- st_intersection(shape.data_cnat.paes, visibility_data_shape_fv)

depr_fv <- 
  read_delim(
    here("script", 
         "paper_landscape_valuation",
         "visibility_analysis",
         "sens_analysis.csv"),
    delim = ","
  )

vertex_combinations <- expand_grid(
  id = depr_fv$id,
  vertex_a = 1:6,
  vertex_b = 1:6
)  |>  
  filter(vertex_a <= vertex_b)

fv_trial <- visibility_data_fv |> 
  left_join(depr_fv)

names(fv_trial) <- str_remove(names(fv_trial), "_m")

fv_trial <- fv_trial |> 
  pivot_longer(dist_1:vis_6,
               names_to = c("var", "vertex"),
               names_sep = "_",
               values_to = "value"
  ) |> 
  select(id, N_base, distance, var, vertex, value) |> 
  pivot_wider(
    names_from = var,
    values_from = value
  ) |> 
  mutate(vertex = as.integer(vertex))

fv_trial <- vertex_combinations %>% 
  left_join(fv_trial,
            by = c("id", "vertex_a" = "vertex")) %>% 
  left_join(fv_trial,
            by = c("id", "vertex_b" = "vertex"),
            suffix = c("_a", "_b")) %>% 
  mutate(delta_rad_a_b = (180 - abs(abs(dir_a - dir_b) - 180)) * pi / 180,
         segment = sqrt(dist_a ^ 2 + dist_b^2 - 
                          2 * dist_a * dist_b * cos(delta_rad_a_b)), # distanza tra ogni coppia di vertici
         median_dist = 0.5 * sqrt(2 * (dist_a^2 + dist_b^2) - segment^2)) # mediana

app_d_alt <- fv_trial %>% 
  mutate(app_d_alt =  2 * atan((segment/(2*median_dist))),
         app_d_alt = if_else(
           vis_a  == 1 & vis_b == 1,
           app_d_alt,
           0
         )) %>% 
  select(id, vertex_a, vertex_b, vis_a, vis_b, app_d_alt) %>% 
  filter(vertex_a != vertex_b) %>% 
  group_by(id) %>% 
  slice(which.max(app_d_alt)) %>% 
  ungroup() %>% 
  select(id, app_d_alt)

depr_fv <- 
  intersection_fv |> 
  left_join(
    app_d_alt, by = c("id" = "id")
  ) |>
  mutate(
    depr = pmap_dbl(
      list(
        distance_m,
        N_base,
        app_d_alt
      ),
      \(x, y, z) depr_fv_alt(distance = x, visible_vertices = y, app_diameter = z)
    ),
    area_ha = as.numeric(st_area(geometry)) / 10^4, # area geometrie in ha
    total_value = wtp * area_ha,
    depr_value = wtp * depr * area_ha,
    area_visibile_ha = if_else(
      depr == 0,
      0,
      area_ha
    )
  )

app_d_data <- depr_fv |>  # needed to make a map of apparent diameter
  mutate(
    app_d_ratio = pmap_dbl(
      list(
        distance_m,
        N_base,
        app_d_alt
      ),
      \(x, y, z) depr_fv_alt(distance = x, visible_vertices = y, app_diameter = z, output = "app_d_ratio")
    )
  )

depr_fv_detail <- 
  depr_fv |> 
  as_tibble() |> 
  group_by(
    NOMEUNI, TIPO_PAE, classe_na, classe_cu, classe_rs
  ) |> 
  summarise(
    total_value = sum(total_value),
    depr_value = sum(depr_value),
    area_visibile_ha = sum(area_visibile_ha))|>  
  ungroup()

### WIND

visibility_data_eo <- 
  read_delim(
    file = here::here("script", "paper_landscape_valuation", 
                      "visibility_analysis", "eo_lato58km_100m.csv"),
    delim = ","
  )


visibility_data_shape_eo <- 
  visibility_data_eo |> 
  mutate(geometry = pmap(list(left, bottom, right, top), function(left, bottom, right, top) {
    st_polygon(list(matrix(c(left, bottom, right, bottom, right, top, left, top, left, bottom),
                           ncol = 2, byrow = TRUE)))
  }))  |> 
  st_as_sf() |> 
  st_set_crs(32632)

# computing geometry areas

geometry_area <- # each geometry has an area of 1 ha. So total area is the sum of all geometries from which at least 1 turbine is visible 
  length(visibility_data_shape_eo[visibility_data_shape_eo$N_base != 0, ]$geometry)

area_squadre_58_km <- 58^2*100

area_circle_29km_radius <- 29^2 * pi * 100


# Now perform the intersection using the reprojected layer.
intersection_eo <- st_intersection(shape.data_cnat.paes, visibility_data_shape_eo) |>
  mutate(area_ha = as.numeric(st_area(geometry)) / 10^4) |> # geometry area in ha
  select(NOMEUNI, TIPO_PAE, classe_na, classe_cu, wtp, clc18, distance_m, n = N_base, area_ha) |>
  as_tibble()

# Depreciation due to turbine height

height_depr <- eo_conf |>
  select(h) |> 
  unique() |> 
  mutate(depr_h = map_dbl(
    h,
    ~ height_fn(.x)
  ))

# Depreciation due to turbine number

number_depr <- intersection_eo |>
  select(n) |> 
  unique() |> 
  mutate(depr_n = map_dbl(
    n,
    ~ turbine_fn(.x)
  ))

eo_depr <- 
  intersection_eo |> 
  mutate(h = eo_conf[eo_conf$case == "base", ]$h)

eo_depr <- purrr::reduce(list(eo_depr ,height_depr,number_depr), 
                         dplyr::left_join) |> 
  mutate(depr_d = map_dbl(distance_m, ~ distance_fn(.x)),
         depr = depr_d * depr_n * depr_h,
         total_value = wtp * area_ha,
         depr_value = depr * wtp * area_ha,
         area_visibile_ha = if_else(
           depr == 0,
           0,
           area_ha
         )) |> 
  group_by(
    NOMEUNI, TIPO_PAE, classe_na, classe_cu
  ) |> 
  summarise(
    total_value = sum(total_value),
    depr_value = sum(depr_value),
    area_visibile_ha = sum(area_visibile_ha)) |> 
  ungroup()


## Comparison PV - WIND

depr_fv_all <- depr_fv_detail |> 
  summarise(depr_value = sum(depr_value)) |> 
  mutate(type = "PV")

comparison_fv_eo <- eo_depr |> 
  summarise(depr_value = sum(depr_value)) |> 
  mutate(type = "Wind") |> 
  bind_rows(depr_fv_all)


comparison_fv_eo <- 
  comparison_fv_eo |> 
  mutate(depr_value_mwh = if_else(
    type == "Wind",
    depr_value / prod_eo * 10^-3,
    depr_value / prod_fv * 10^-3
  ))

results <- 
  list(
    eo = eo_depr,
    fv = depr_fv_detail,
    comparison = comparison_fv_eo
  )



