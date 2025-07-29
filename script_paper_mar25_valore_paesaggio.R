## Mappe e grafici per paper valore paesaggistico marzo 2025 ###

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

source( # carico le funzioni di deprezzamento
  here("script", "funzioni_deprezzamento_24.R")
)

# carico tabelle di corrispoendenza italiano-inglese

table_path <- 
  here(
    "data",
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

# Carico i valori delle classi di paesaggio

tipo_valori <- "valori_paesaggi_storici" # tipologia di valori da caricare

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

wtp <- read_xlsx(
  here("data", "valori_paesaggio_24.xlsx"),
  sheet = tipo_valori
) |> 
 mutate(
   classe_cu = recode(classe_cu, !!!paes_class_translation_vct),
   classe_na = recode(classe_na, !!!paes_class_translation_vct)
 )

# carico la mappa delle tipologie di paesaggio ISPRA

shape.data_paes <- sf::st_read(here("data", "carta_paesaggio", "CN250_UdP.shp"),
                               quiet = T) |> 
  mutate(TIPO_PAE = recode(
    TIPO_PAE,
    !!!paes_tipo_translation_vct
  ))

# Carico carta della natura ISPRA

shape.data_cnat <- sf::st_read(here("data", "carta_natura", "CartaNaturalisticoCulturale.shp"),
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
    
#Carico i file Kml dei paesaggi rurali

# Leggi i fileKML dei paesaggi rurali
kml_files <- list.files(path = here("data", "paesaggi_tradizionali", "kml"), pattern = "\\.kml$", full.names = TRUE)

sf_objects <- lapply(kml_files, function(file) {
  sf_obj <- st_read(file)
  if (is.na(st_crs(sf_obj))) {
    st_crs(sf_obj) <- 4326 # Imposta WGS84 se il CRS non è definito
  }
  return(sf_obj)
})

sf_objects <- lapply(sf_objects, function(sf_obj) {
  st_transform(sf_obj, crs = st_crs(shape.data_cnat)) # Trasforma in WGS84
})

combined_sf <- do.call(rbind, sf_objects)
st_crs(combined_sf) <- st_crs(shape.data_cnat) # Imposta nuovamente WGS84

# Filtra solo gli oggetti di tipo POLYGON o MULTIPOLYGON
combined_sf_filtered <- combined_sf[st_geometry_type(combined_sf) %in% c("POLYGON", "MULTIPOLYGON"), ]
shape.paes.trad <- st_cast(combined_sf_filtered, "MULTIPOLYGON") |> 
  select(!Description)

#Faccio l'intersezione tra paesaggi rurali e carta paesaggio

intersezione_paes_trad <- st_intersection(shape.data_cnat, shape.paes.trad) |> 
  mutate(classe_rs = "sì")

#Geometrie che non intersecano

non_intersecting_polygons <- st_difference(shape.data_cnat, st_union(shape.paes.trad)) |> #geometrie che non intersecano
  mutate(classe_rs = "no")

shape.data_cnat.paes <- # shapefile frutto dell'intersezione con i polighi delle aree rurali
  rbind(select(intersezione_paes_trad, !Name), non_intersecting_polygons)

# procedura alternativa per avere mappa con unità di paesaggio che contengono aree rurali tradizionali
# shape.data_cnat$has_rural_area <- lengths(st_intersects(shape.data_cnat, shape.paes.trad)) > 0

# aggiungo i valori di disponibilità a pagare

shape.data_cnat.paes <- 
  shape.data_cnat.paes |> 
  left_join(wtp) |> 
  mutate(area_ha = as.numeric(st_area(geometry)) / 10^4, # calcolo area geometrie in ha
         wtp_abs = as.numeric(wtp * area_ha) / 10^6) # calcolo valori assoluti paesaggio (milioni di euro)

# Carico i dati riguardanti le aree di visibilità

visibility_data_fv <- 
  read_delim(
    file = here::here("data", "visibility_analysis", "fv_lato15km_100m.csv"),
    delim = ","
  )

visibility_data_shape_fv <- 
  visibility_data_fv |> 
  mutate(geometry = pmap(list(left, bottom, right, top), function(left, bottom, right, top) {
    st_polygon(list(matrix(c(left, bottom, right, bottom, right, top, left, top, left, bottom),
                           ncol = 2, byrow = TRUE)))
  })) |> 
  st_as_sf() 

# Controllare se ci sono righe
st_crs(visibility_data_shape_fv) <- st_crs(shape.data_cnat.paes)

intersection_fv <- st_intersection(shape.data_cnat.paes, visibility_data_shape_fv)

deprezzamento_fv <- 
  intersection_fv |> 
  mutate(
    depr = map2_dbl(
      distance_m,
      N_base,
      \(x, y) depr_fv(distance = x, visible_vertices = y)
    ),
    depr_value = wtp * depr
  )



deprezzamento_fv_detail <- 
  deprezzamento_fv |> 
  as_tibble() |> 
  group_by(
    NOMEUNI, TIPO_PAE, classe_na, classe_cu, classe_rs
  ) |> 
  summarise(depr_value = sum(depr_value)) |> 
  ungroup()

## Calcolo il deprezzamento FV anche con la metodologia alternativa

sens_analysis_fv <- 
  read_delim(
    here("data", "visibility_analysis", "sens_analysis.csv"),
    delim = ","
  )

vertex_combinations <- expand_grid(
  id = sens_analysis_fv$id,
  vertex_a = 1:6,
  vertex_b = 1:6
)  |>  
  filter(vertex_a <= vertex_b)

fv_trial <- visibility_data_fv |> 
  left_join(sens_analysis_fv)

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

deprezzamento_fv_alt <- 
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

app_d_data <- deprezzamento_fv_alt |>  # questo serve per fare mappa diametro apparente
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

deprezzamento_fv_detail_alt <- 
  deprezzamento_fv_alt |> 
  as_tibble() |> 
  group_by(
    NOMEUNI, TIPO_PAE, classe_na, classe_cu, classe_rs
  ) |> 
  summarise(
    total_value = sum(total_value),
    depr_value = sum(depr_value),
    area_visibile_ha = sum(area_visibile_ha))|>  
  ungroup()

## RISULTATI

results <- 
  readRDS(here("output", "results.rds"))

results_eo <- 
  readRDS(here("output", "results.rds"))[["eo"]] |> 
  mutate(TIPO_PAE = recode(TIPO_PAE, !!!paes_tipo_translation_vct),
         across(contains("classe"), \(x) recode(x, !!!paes_class_translation_vct)))

results_fv <- 
  readRDS(here("output", "results.rds"))[["fv"]] |> 
  mutate(TIPO_PAE = recode(TIPO_PAE, !!!paes_tipo_translation_vct),
         across(contains("classe"), \(x) recode(x, !!!paes_class_translation_vct)))

results_comp <- 
  readRDS(here("output", "results.rds"))[["comparison"]] |> 
  mutate(type = ifelse(
    type == "Eolico",
    "Wind",
    "Solar PV"
  ))

# Produzione impianti

prod_eo <- 173.1 #GWh/anno
prod_fv <- 95.7 #GWh/anno

