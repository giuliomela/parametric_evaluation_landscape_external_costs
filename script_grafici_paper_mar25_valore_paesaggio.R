### GRAFICI VALORE PAESAGGIO

source(
  here::here(
    "script",
    "script_paper_mar25_valore_paesaggio.R"
  )
)

# mappa del mondo come contorno


shape.data_w <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") |> 
  st_transform(crs = 3035) # proiezione ETRS89 / ETRS-LAEA (EPSG:3035)

# Mappa dei tipi di paesaggio

# unisco le geometrie dei tipi di peaesaggio

shape.data_paes_type <- shape.data_paes |> 
  group_by(TIPO_PAE) |>
  summarise(geometry = sf::st_union(geometry)) |>
  ungroup() |> 
  st_transform(crs = 3035)

### MAPPA TIPI PAESAGGIO

# limiti della mappa in coorsinate
bbox_wgs84 <- st_bbox(c(
  xmin = 7,
  xmax = 18.5,
  ymin = 35.8,
  ymax = 47
), crs = st_crs(4326))  # WGS84

# Convertire il bounding box in EPSG:3035
bbox_3035 <- st_transform(st_as_sfc(bbox_wgs84), crs = 3035)
bbox_coords <- st_bbox(bbox_3035)  # Ottieni i limiti in metri

# Creating a base map

landscape_pal <- viridis(
  n = length(unique(shape.data_paes_type$TIPO_PAE)),
  option = "turbo"
)

names(landscape_pal) <- sort(unique(shape.data_paes_type$TIPO_PAE))


landscape_map <- sf::st_cast(shape.data_w, "MULTIPOLYGON") |> 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "grey") +
  geom_sf(data = sf::st_cast(shape.data_paes_type, "MULTIPOLYGON"), aes(fill = TIPO_PAE), color = NA) +
  theme_bw() +
  scale_fill_manual(values = landscape_pal,
                    labels = ~ stringr::str_wrap(.x, width = 20)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = '#e6f2ff')) +
  coord_sf(
    xlim = c(bbox_coords["xmin"], bbox_coords["xmax"]),
    ylim = c(bbox_coords["ymin"], bbox_coords["ymax"])
  ) +
  # coord_sf(xlim = c(7, 18.5), ylim = c(35.8, 47)) +
  theme(legend.position = "right",
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.5, 'cm')) +
  labs(fill = "Landscape types")

# Mappa dei paesaggi rurali tradizionali

shape.data_ita <- shape.data_w |> 
  filter(name_en == "Italy")

shape.paes.trad <- 
  st_transform(shape.paes.trad, crs = 3035)

rural_map <- 
  sf::st_cast(shape.data_w, "MULTIPOLYGON") |> 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "grey") +
  geom_sf(data = sf::st_cast(shape.data_ita, "MULTIPOLYGON"), fill = "ivory", color = NA) +
  geom_sf(data = sf::st_cast(shape.paes.trad, "MULTIPOLYGON"), fill = "tomato", color = NA) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = '#e6f2ff')) +
  coord_sf(
    xlim = c(bbox_coords["xmin"], bbox_coords["xmax"]),
    ylim = c(bbox_coords["ymin"], bbox_coords["ymax"])
  ) +
  # coord_sf(xlim = c(7, 18.5), ylim = c(35.8, 47)) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.5, 'cm')) +
  labs(fill = "")

# Mappa del valore culturale e naturale

shape.data_cnat <- 
  st_transform(shape.data_cnat, crs = 3035)

livelli_25_cat <- 
  wtp |> 
  mutate(classe_na_cu = paste0(classe_cu, " - ", classe_na)) |> 
  pull(classe_na_cu) |> 
  unique()

shape.data_cnat_25cat <- shape.data_cnat |> 
  mutate(classe_na_cu = paste0(classe_na, " - ", classe_cu),
         classe_na_cu = factor(classe_na_cu,
                               levels = livelli_25_cat)) |> 
  st_transform(crs = 3035)

pal_25cat <- 
  c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
    "#ffff33", "#a65628", "#f781bf", "#999999", "#66c2a5",
    "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f",
    "#e5c494", "#b3b3b3", "#1b9e77", "#d95f02", "#7570b3",
    "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")

names(pal_25cat) <- livelli_25_cat

na_cu_map_25cat <- 
  sf::st_cast(shape.data_w, "MULTIPOLYGON") |> 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "grey") +
  geom_sf(data = sf::st_cast(shape.data_cnat_25cat, "MULTIPOLYGON"), 
          aes(fill = classe_na_cu), color = NA) +
  theme_bw() +
  scale_fill_manual(name = "Combinations\nnatural value - cultural value",
                    values = pal_25cat) +
  # scale_fill_viridis(name = "Combinazioni\nvalore culturale - valore naturale",
  #                    option = "viridis", discrete = T, direction = -1) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = '#e6f2ff')) +
  coord_sf(
    xlim = c(bbox_coords["xmin"], bbox_coords["xmax"]),
    ylim = c(bbox_coords["ymin"], bbox_coords["ymax"])
  ) +
  # coord_sf(xlim = c(7, 18.5), ylim = c(35.8, 47)) +
  theme(legend.position = "right",
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.5, 'cm')) +
  labs(fill = "")

# Mappa dei valori per ha

landscape_value_map <- sf::st_cast(shape.data_w, "MULTIPOLYGON") |> 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "grey") +
  geom_sf(data = sf::st_cast(shape.data_cnat.paes, "MULTIPOLYGON"), aes(fill = wtp), color = NA) +
  theme_bw() +
  scale_fill_viridis_c(option = "mako", name = "WTP (euro/ha/y)", direction = -1) +
  #scale_fill_distiller(palette = "Spectral", direction = 1) +
  # scale_fill_manual(values = landscape_pal,
  #                   labels = ~ stringr::str_wrap(.x, width = 20)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = '#e6f2ff')) +
  coord_sf(
    xlim = c(bbox_coords["xmin"], bbox_coords["xmax"]),
    ylim = c(bbox_coords["ymin"], bbox_coords["ymax"])
  ) +
  # coord_sf(xlim = c(7, 18.5), ylim = c(35.8, 47)) +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm')) +
  labs(fill = "WTP (euro/ha/year)")

## Mappa potenziale eolico

# Carico mappa potenziale eolico

shape_eolico <- 
  sf::st_read(here("data", "atlante_integrato", "potenziale_eolico", "Grid_nazionale_20240221.shp"),
              quiet = T)

#st_crs(shape_eolico) <- st_crs(shape.data_cnat.paes) # Imposta lo stesso sistema di coordinate

shape_eolico <- 
  st_transform(shape_eolico, crs = 3035)

shape.data_ita <- 
  shape.data_w |> 
  filter(name_en == "Italy")

shape_eolico_onshore <- 
  st_intersection(shape_eolico, shape.data_ita)

map_potenziale_eolico <- 
  sf::st_cast(shape.data_w, "MULTIPOLYGON") |> 
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill = "grey") +
  geom_sf(data = sf::st_cast(shape_eolico_onshore, "MULTIPOLYGON"),
          aes(fill = prod_100), color = NA) +
  theme_bw() +
  scale_fill_viridis(name = "Onshore wind potential\n100 m height \nMWh/MW",
                     direction = -1,
                     option = "magma", discrete = F) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = '#e6f2ff')) +
  coord_sf(
    xlim = c(bbox_coords["xmin"], bbox_coords["xmax"]),
    ylim = c(bbox_coords["ymin"], bbox_coords["ymax"])
  ) +
  theme(legend.position = "right",
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.5, 'cm'))


##GRAFICI risultati

res_eo_plot <- 
  results_eo |> 
  group_by(TIPO_PAE) |> 
  summarise(depr_value = sum(depr_value) / 1000) |> # risultati in migliaia di euro 
  ungroup() |> 
  mutate(TIPO_PAE = fct_reorder(str_wrap(TIPO_PAE, 20), depr_value)) |> 
  ggplot(aes(
    x = depr_value,
    y = TIPO_PAE,
    label = round(depr_value, 1)
  )) +
  geom_col(fill = "purple") +
  ggfittext::geom_bar_text(outside = T, min.size = 10) +
  ggthemes::theme_economist_white() +
  geom_vline(xintercept = 0) +
  labs(y = "", x = "Thousand euro/year", fill = "") +
  theme(legend.position = "bottom", plot.background=element_blank(),
        legend.background = element_blank()) 

res_fv_plot <- 
  results_fv |> 
  group_by(TIPO_PAE) |> 
  summarise(depr_value = sum(depr_value) / 1000) |> # risultati in migliaia di euro 
  ungroup() |> 
  mutate(TIPO_PAE = fct_reorder(str_wrap(TIPO_PAE, 20), depr_value)) |> 
  ggplot(aes(
    x = depr_value,
    y = TIPO_PAE,
    label = round(depr_value, 1)
  )) +
  geom_col(fill = "orange") +
  ggfittext::geom_bar_text(outside = T, min.size = 10) +
  ggthemes::theme_economist_white() +
  geom_vline(xintercept = 0) +
  labs(y = "", x = "Thousand euro/year", fill = "") +
  theme(legend.position = "bottom", plot.background=element_blank(),
        legend.background = element_blank())

res_comparison_plot <- 
  results_comp |> 
  pivot_longer(
    !type,
    names_to = "var",
    values_to = "value"
  ) |> 
  mutate(
    var = if_else(
      str_detect(var, "mwh"),
      "Loss of landscape value\nper MWh produced\n(euro/year)",
      "Absolute loss of landscape value\n(Thousand euro/year)"
    ),
    value = if_else(
      str_detect(var, "MWh"),
      value,
      value / 1000
    ),
    label = if_else(
      str_detect(var, "MWh"),
      round(value, 2),
      round(value, 1)
    )
  ) |> 
  ggplot(aes(
    x = value,
    y = type,
    fill = type, 
    label = label)
  ) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c(Wind = "purple", `Solar PV` = "orange")) +
  ggfittext::geom_bar_text(outside = T, min.size = 9, position = "dodge") +
  ggthemes::theme_economist_white() +
  facet_wrap(~ var, scales = "free_x") +
  geom_vline(xintercept = 0) +
  labs(y = "", x = "", fill = "") +
  theme(legend.position = "none", plot.background=element_blank(),
        legend.background = element_blank()) 


## Funzione deprezzamento distanza eolico

plot_depr_fun <- 
  ggplot() +
  xlim(.1, 29) +
  geom_function(fun = function(x) 1 - (30.94 * log(x) + 71.24)/
                  (30.94 * log(29) + 71.24),
                color = "purple",
                linewidth = 1) +
  theme_light() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "landscape value reduction",
       x = "Distance (km)")

## Grafico visibilità

# Mappa diametro apparente (in rapporto al campo visivo)

# carico sagoma impianto

area_poligono <- 
  st_read(
    here("data", "visibility_analysis", "poligono_fv", "area_poligono.shp")
  )

# Creazione di una nuova variabile colore
app_d_data$colore <- ifelse(app_d_data$app_d_ratio == 0, NA, app_d_data$app_d_ratio)

plot_d_app <- 
  ggplot() +
  geom_sf(data = app_d_data, aes(fill = colore), color = NA, size = 0.1) +
  geom_sf(data = area_poligono, color = "ivory", alpha = 0) +
  scale_fill_viridis(option = "rocket", direction = -1, na.value = "gray80",
                     name = "Share of the visual field\noccupied by the plant\n%") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank()) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.2, 
                              text_col = "black", 
                              unit_category = "metric", 
                              height = unit(0.2, "cm"), 
                              pad_x = unit(0.54, "cm"),
                              style = "ticks")

all_fig_en <- 
  list(
    landscape_map = landscape_map,
    rural_map= rural_map,
    na_cu_map_25cat= na_cu_map_25cat,
    landscape_value_map = landscape_value_map,
    map_potenziale_eolico = map_potenziale_eolico,
    res_eo_plot = res_eo_plot,
    res_fv_plot = res_fv_plot,
    res_comparison_plot = res_comparison_plot,
    plot_depr_fun = plot_depr_fun,
    plot_d_app = plot_d_app
  )
