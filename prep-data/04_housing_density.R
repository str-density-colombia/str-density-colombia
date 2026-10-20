#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   04_housing_density.R
#
#   Purpose   Map housing density (housing units per 1 km² grid cell) within
#             each municipality in the study, with insets locating the
#             municipality in Colombia.
#   Paper     Section 2.1, Figure 1.
#   Input     data/clean/100226_multilevel_final_dataset.xlsx
#             (from 03_prepare_input.R).
#             spatial-data/ShapeFiles/DANE_geodata/MGN_ANM_MPIOS.shp,
#             municipal boundaries.
#             spatial-data/dane_grilla/N6_1K/N6_1K.shp, 1 km² grid with
#             housing units from the 2018 census.
#             spatial-data/ShapeFiles/World/world.shp, country boundaries.
#   Output    Two maps per municipality, in spatial-data/map_housing_density/<municipality>/:
#             00_<municipality>_combined.png, density map with legend.
#             00_<municipality>_no_legend.png, density map with location
#             insets and no legend.
#   Packages  sf, ggplot2, ggspatial, cowplot, grid, dplyr, readxl
#   Note      Run the script from the root folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(grid)
library(dplyr)
library(readxl)

in_file    <- file.path("data", "clean", "100226_multilevel_final_dataset.xlsx")
mpio_file  <- file.path("spatial-data", "ShapeFiles", "DANE_geodata", "MGN_ANM_MPIOS.shp")
grid_file  <- file.path("spatial-data", "dane_grilla", "N6_1K", "N6_1K.shp")
world_file <- file.path("spatial-data", "ShapeFiles", "World", "world.shp")
out_dir    <- file.path("spatial-data", "map_housing_density")

# Projected coordinate system for Colombia (MAGNA-SIRGAS / Colombia Bogota
# zone, EPSG:3116).
crs_col <- 3116

# Margin around Colombia for the layer of neighbouring countries, as a share
# of the width and height of the country.
expand_factor <- 0.4

# Range of the colour scale (housing units per 1 km² cell), common to all
# municipalities. Cells above the upper limit take the colour of the limit.
global_limits <- c(0, 10000)



# 2 MUNICIPALITIES IN THE SAMPLE -----------------------------------------------

# Same restriction as in the estimation sample: listings with a positive price
# below 15 million COP/month and a positive floor area.
df2_c <- read_excel(in_file) %>%
  filter(precio > 0, precio < 15000000, area > 0)

col_shape <- st_read(mpio_file, quiet = TRUE)

# Five-digit DIVIPOLA code: department code + municipal code.
col_shape$cod.mun <- as.numeric(
  paste0(col_shape$DPTO_CCDGO, col_shape$MPIO_CCDGO)
)

# By assumption, the San Andrés archipelago (department 88) is left out of the
# maps so that the mainland fills the frame.
col_shape <- col_shape %>%
  filter(DPTO_CCDGO != "88")

# Boundaries of the municipalities in the sample.
col_shape_df <- merge(
  col_shape,
  df2_c %>% select(cod.mun, slug_ciudad, slug_depto) %>% distinct(),
  by = "cod.mun",
  all.y = TRUE
)



# 3 SPATIAL LAYERS -------------------------------------------------------------

## 3.1 Housing density grid and boundaries -----------------------------

grid_dane <- st_read(grid_file)

grid_dane    <- st_transform(grid_dane, crs_col)
col_shape    <- st_transform(col_shape, crs_col)
col_shape_df <- st_transform(col_shape_df, crs_col)

## 3.2 Neighbouring countries ------------------------------------------

latin_america <- st_read(world_file)
latin_america <- latin_america %>%
  filter(
    region_wb == "Latin America & Caribbean",
    admin != "Colombia"
  )

latin_america <- st_transform(latin_america, crs_col)

# The layer is cropped to a box around Colombia, expanded by expand_factor
# on each side.
bbox_col <- st_bbox(col_shape)

xrange <- bbox_col$xmax - bbox_col$xmin
yrange <- bbox_col$ymax - bbox_col$ymin

bbox_expand <- bbox_col
bbox_expand["xmin"] <- bbox_col["xmin"] - xrange * expand_factor
bbox_expand["xmax"] <- bbox_col["xmax"] + xrange * expand_factor
bbox_expand["ymin"] <- bbox_col["ymin"] - yrange * expand_factor
bbox_expand["ymax"] <- bbox_col["ymax"] + yrange * expand_factor

latin_america <- st_transform(latin_america, st_crs(col_shape))
latin_america <- st_crop(latin_america, bbox_expand)



# 4 HELPER FUNCTIONS -----------------------------------------------------------

# Adds a black frame around a map.
add_black_frame <- function(p){
  p + theme(
    panel.border = element_rect(colour = "black",
                                fill = NA, linewidth = 1),
    plot.margin = margin(0, 0, 0, 0)
  )
}

# Turns a bounding box into a square with the same centre, so that every
# municipality map has the same aspect ratio.
make_square_bbox <- function(bb){
  
  width  <- bb["xmax"] - bb["xmin"]
  height <- bb["ymax"] - bb["ymin"]
  
  side <- max(width, height)
  
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2
  
  bb["xmin"] <- cx - side/2
  bb["xmax"] <- cx + side/2
  bb["ymin"] <- cy - side/2
  bb["ymax"] <- cy + side/2
  
  bb
}

# Places two location insets on the right side of the municipality map: the
# municipality within Colombia (upper) and a closer view (lower).
embed_insets_nl <- function(p_main, p_inset1, p_inset2){
  alpha = 1.17
  ggdraw() +
    
    # Municipality map
    draw_plot(p_main, 0, 0, 1, 1) +
    
    # Upper inset
    draw_plot(
      p_inset1,
      x = 0.52*alpha,
      y = 0.52,
      width = 0.38,
      height = 0.38
    ) +
    
    # Lower inset
    draw_plot(
      p_inset2,
      x = 0.57*alpha,
      y = 0.075,
      width = 0.28,
      height = 0.28
    )
}



# 5 MAP OF ONE MUNICIPALITY ----------------------------------------------------

make_municipality_maps <- function(mpio_name){
  
  mpio_dir <- file.path(out_dir, gsub(" ", "_", mpio_name))
  dir.create(mpio_dir, recursive = TRUE, showWarnings = FALSE)
  
  mpio <- col_shape_df %>% filter(MPIO_CNMBR == mpio_name)
  mpio_grid <- st_intersection(grid_dane, mpio)
  
  ## 5.1 Map extent ----------------------------------------------------
  
  # The municipality's bounding box is expanded by half its size and made
  # square
  bb <- st_bbox(mpio)
  
  expand_factor <- 0.5
  bb_exp <- as.numeric(bb)
  names(bb_exp) <- names(bb)
  
  bb_exp["xmin"] <- bb_exp["xmin"] -
    (bb_exp["xmax"] - bb_exp["xmin"]) * expand_factor
  bb_exp["xmax"] <- bb_exp["xmax"] +
    (bb_exp["xmax"] - bb_exp["xmin"]) * expand_factor
  bb_exp["ymin"] <- bb_exp["ymin"] -
    (bb_exp["ymax"] - bb_exp["ymin"]) * expand_factor
  bb_exp["ymax"] <- bb_exp["ymax"] +
    (bb_exp["ymax"] - bb_exp["ymin"]) * expand_factor
  
  bb_exp <- make_square_bbox(bb_exp)
  
  ## 5.2 Municipality map ----------------------------------------------
  
  # Layers, from bottom to top: neighbouring countries, Colombia, housing
  # density within the municipality, and the municipal boundary.
  p_main <- ggplot() +
    geom_sf(data = latin_america,
            fill = "gray71", color = "black", linewidth = 0.08) +
    geom_sf(data = col_shape,
            fill = "antiquewhite", color = "black",
            linewidth = 0.08) +
    geom_sf(data = mpio_grid,
            aes(fill = viviendas), color = NA) +
    scale_fill_viridis_c(
      option = "plasma",
      limits = global_limits,
      breaks = pretty(global_limits, 10),
      oob = scales::squish,
      name = "Housing units per 1 km²"
    ) +
    geom_sf(data = mpio,
            fill = NA, color = "black", linewidth = 0.3) +
    coord_sf(
      xlim = c(bb_exp["xmin"], bb_exp["xmax"]),
      ylim = c(bb_exp["ymin"], bb_exp["ymax"])
    ) +
    labs(title = mpio_name) +
    annotation_scale(location = "bl", width_hint = 0.35, text_cex = 0.6) +
    annotation_north_arrow(location = "tl",
                           height = unit(0.7, "cm"),
                           width = unit(0.7, "cm")) +
    theme_void()
  
  main <- add_black_frame(p_main)
  
  ## 5.3 Upper inset: municipality within Colombia ---------------------
  
  # A dashed box around the municipality, buffered so that it remains
  # visible at the national scale.
  mpio_outline <- st_union(mpio)
  mpio_bbox <- st_as_sfc(st_bbox(mpio_outline))
  
  bb <- st_bbox(mpio_bbox)
  width  <- bb["xmax"] - bb["xmin"]
  height <- bb["ymax"] - bb["ymin"]
  
  mpio_bbox <- st_buffer(mpio_bbox,
                         dist = 1.4 * max(width, height))
  
  p_inset1 <- ggplot() +
    geom_sf(data = latin_america,
            fill = "gray71", color = "black", linewidth = 0.08) +
    geom_sf(data = col_shape,
            fill = "antiquewhite", color = "grey22", linewidth = 0.08) +
    geom_sf(data = col_shape_df %>%
              filter(MPIO_CNMBR == mpio_name),
            fill = "red", color = "red", linewidth = 1) +
    geom_sf(data = mpio_bbox,
            fill = NA, color = "red",
            linewidth = 0.8, linetype = 2) +
    coord_sf(expand = FALSE) +
    theme_void()
  
  p_inset1 <- add_black_frame(p_inset1) +
    theme(panel.grid.major = element_line(color = gray(.5),
                                          linetype = "dashed",
                                          size = 0.5),
          panel.background = element_rect(fill = "aliceblue")) +
    annotation_scale(location = "bl", width_hint = 0.35, text_cex = 0.6) +
    annotation_north_arrow(location = "tl",
                           height = unit(0.7, "cm"),
                           width = unit(0.7, "cm"))
  
  ## 5.4 Lower inset: closer view --------------------------------------
  
  # The extent moves from the whole country towards the municipality map
  # extent; expand_zoom is the share of the distance that is kept.
  bb_col <- st_bbox(col_shape)
  
  expand_zoom <- 0.08
  bb_zoom <- as.numeric(bb_col)
  names(bb_zoom) <- names(bb_col)
  
  bb_zoom["xmin"] <- bb_zoom["xmin"] +
    (bb_exp["xmin"] - bb_zoom["xmin"]) * (1-expand_zoom)
  bb_zoom["xmax"] <- bb_zoom["xmax"] -
    (bb_zoom["xmax"] - bb_exp["xmax"]) * (1-expand_zoom)
  bb_zoom["ymin"] <- bb_zoom["ymin"] +
    (bb_exp["ymin"] - bb_zoom["ymin"]) * (1-expand_zoom)
  bb_zoom["ymax"] <- bb_zoom["ymax"] -
    (bb_zoom["ymax"] - bb_exp["ymax"]) * (1-expand_zoom)
  
  bb_zoom <- make_square_bbox(bb_zoom)
  
  p_inset2 <- ggplot() +
    geom_sf(data = col_shape,
            fill = "antiquewhite", color = "black", linewidth = 0.08) +
    geom_sf(data = col_shape_df %>%
              filter(MPIO_CNMBR == mpio_name),
            fill = "red", color = "black", linewidth = 0.08) +
    geom_sf(data = latin_america,
            fill = "gray71", color = "black", linewidth = 0.08) +
    geom_sf(data = mpio_bbox,
            fill = NA, color = "red",
            linewidth = 0.8, linetype = 2) +
    coord_sf(
      xlim = c(bb_zoom["xmin"], bb_zoom["xmax"]),
      ylim = c(bb_zoom["ymin"], bb_zoom["ymax"]),
      expand = FALSE
    ) +
    annotation_scale(location = "bl", width_hint = 0.35, text_cex = 0.6) +
    annotation_north_arrow(location = "tl",
                           height = unit(0.7, "cm"),
                           width = unit(0.7, "cm")) +
    theme_void()
  
  p_inset2 <- add_black_frame(p_inset2) +
    theme(panel.grid.major = element_line(color = gray(.5),
                                          linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = "aliceblue"))
  
  ## 5.5 Final maps ----------------------------------------------------
  
  # Map with legend.
  p_final <- p_main +
    theme(panel.grid.major = element_line(color = gray(.5),
                                          linetype = "dashed",
                                          size = 0.5),
          panel.background = element_rect(fill = "aliceblue"),
          legend.position = "bottom",
          plot.title = element_text(
            size = 24,
            face = "bold",
            hjust = 0.5
          )) +
    guides(
      fill = guide_colorbar(
        barheight = unit(2, "cm"),
        barwidth  = unit(12, "cm")
      )
    )
  
  # Map with location insets and no legend.
  p_final_nl <- embed_insets_nl(main +
                                  theme(panel.grid.major = element_line(color = gray(.5),
                                                                        linetype = "dashed",
                                                                        size = 0.5),
                                        panel.background = element_rect(fill = "aliceblue"),
                                        plot.title = element_text(
                                          size = 24,
                                          face = "bold",
                                          hjust = 0.5
                                        )) + guides(fill = "none"),
                                p_inset1, p_inset2)
  
  ## 5.6 Save ----------------------------------------------------------
  
  ggsave(
    file.path(mpio_dir,
              paste0("00_", gsub(" ", "_", mpio_name), "_combined.png")),
    p_final,
    width = 9,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  ggsave(
    file.path(mpio_dir,
              paste0("00_", gsub(" ", "_", mpio_name), "_no_legend.png")),
    p_final_nl,
    width = 6,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
}



# 6 MAPS BY MUNICIPALITY -------------------------------------------------------

municipios <- unique(col_shape_df$MPIO_CNMBR)

for(i in seq_along(municipios)){
  print(municipios[i])
  make_municipality_maps(municipios[i])
}
