### Functions used to compute the extent to which landscape value decreases with the presence of wind or solar farms

library(tidyverse)
library(here)

## Wind farms

# Defining wind farm characteristics of the 3 alternatives considered

eo_conf <- tibble(
  case = c("base", "alt1", "alt2"),
  n = c(10, 10, 5), # numero turbine
  h = c(206, 140, 206), # altezza altezza metri
  mw = c(6.2, 2.2, 6.2) #potenza dei singoli aerogeneratori in MW
)

# Depreciation due to distance from the wind farm

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

# Depreciation due to the turbine number

turbine_fn <- function(turbine_n, mean_turbine_n = 20){
  
  g <- turbine_n / mean_turbine_n
  
  g
}

# Depreciation due to Turbine height

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

### Solar plants


depr_fv_alt <- # function used to calculate to what extent landscape value decreases due to the presence of a PV plant
  function(
    plant_area = 1162388.867, # plant area in m2
    distance, # distance of the observer from the plant (m)
    visual_field = 3.66, # visual field in (rad)
    visible_vertices, # Number of visible vertices
    n_vertex = 6, # Number of vertices of the polygon approximating the plant
    app_diameter, #apparent diameter of the circle circumscribed tothe plant polygon
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



