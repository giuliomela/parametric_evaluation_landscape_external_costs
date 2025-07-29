### Funzioni di deprezzamento da usare nell'analisi

library(tidyverse)
library(here)

## Eolico

# configurazioni eolico

eo_conf <- tibble(
  case = c("base", "alt1", "alt2"),
  n = c(10, 10, 5), # numero turbine
  h = c(206, 140, 206), # altezza altezza metri
  mw = c(6.2, 2.2, 6.2) #potenza dei singoli aerogeneratori in MW
)

# distance
distance_fn <- function(distance_m, max_distance = 29){
  
  distance <- distance_m / 1000
  
  if (distance > 29) {
    
    0
    
  } else if (distance < 0.10001){
    
    1
    
  } else {
    
    y <- 1 - (30.94 * log(distance) + 71.24) / (30.94 * log(max_distance) + 71.24)
    
    y
    
  }
  
}

# turbine number

turbine_fn <- function(turbine_n, mean_turbine_n = 20){
  
  g <- turbine_n / mean_turbine_n
  
  g
}

#Turbine height

height_fn <- function(t_height, min_h = 70, max_h = 200){
  
  if (t_height < 15) {
    
    0
    
  } else {
    
    height <- ifelse(
      t_height > 200,
      200,
      t_height
    )
    
    h <- dplyr::case_when(
      height < 70 ~ 0,
      height >= 70 & height <= 100 ~ 861 * log(height) - 3.37 * height - 2281,
      height > 100 & height <= 200 ~ 1331 + 0.156 * height
    )
    
    prim_min <- - 2281.081 * min_h + 861 * (min_h * log(min_h) - min_h) - 1.685 * min_h^2
    
    prim_100_inf <- - 2281.081 * 100 + 861 * (100 * log(100) - 100) - (1.685 * 100^2)
    
    prim_100_sup <- 1331 * 100 + 0.156 / 2 * 100 ^2
    
    prim_max <- 1331 * max_h + 0.156 / 2 * max_h ^2
    
    medio_int <- 1 / (max_h - min_h) * (prim_100_inf - prim_min + prim_max - prim_100_sup)
    
    
    h / medio_int
    
    
  }
  
}

### Fotovoltaico

# metodo base

depr_fv <- # funzione per il calcolo della deprezzamento del paesaggio causato dagli impianti FV 
  function(
    plant_area = 1162388.867, # m2
    distance, # distanza in metri
    visual_field = 3.66, # radianti
    visible_vertices, # vertici visibili,
    n_vertex = 6 # numero vertici impianto
  ){
    diameter <- 2 * (plant_area / pi)^0.5 # metri
    
    app_diameter <- 2 * atan((diameter/(2*distance)))
    
    app_d_ratio = app_diameter / visual_field * 100
    
    depr  <- if_else(
      app_d_ratio <= 13.5,
      -0.004 * app_d_ratio^2 + 0.128 * app_d_ratio,
      1
    ) * visible_vertices / n_vertex
    
    return(depr)
  }

# metodo alternativo



depr_fv_alt <- # funzione per il calcolo della deprezzamento del paesaggio causato dagli impianti FV 
  function(
    plant_area = 1162388.867, # m2
    distance, # distanza in metri
    visual_field = 3.66, # radianti
    visible_vertices, # vertici visibili,
    n_vertex = 6, # numero vertici impianto
    app_diameter,
    output = "depr"
  ){

    app_d_ratio = app_diameter / visual_field * 100
    
    depr  <- if_else(
      app_d_ratio <= 13.5,
      -0.004 * app_d_ratio^2 + 0.128 * app_d_ratio,
      1
    ) * visible_vertices / n_vertex
    
    if (output == "depr") {
      
      return(depr)
      
    } else if (output == "app_d_ratio") {
      
      return(app_d_ratio)
      
    }
    
}



