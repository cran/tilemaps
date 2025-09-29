## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(tilemaps)
library(sf)
library(dplyr)
library(ggplot2)

## ----fig.height=3,fig.width=4,fig.align='center'------------------------------
governors <- governors %>%
  mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE))

ggplot(governors) +
  geom_sf(aes(geometry = tile_map)) +
  geom_sf_text(aes(geometry = tile_map, label = abbreviation),
               fun.geometry = function(x) st_centroid(x)) +
  theme_void()

## ----fig.height=3,fig.width=4,fig.align='center'------------------------------
all_states <- governors %>%
  add_row(abbreviation = "AK", party = "Republican",
          tile_map = create_island(governors$tile_map, "lower left")) %>%
  add_row(abbreviation = "HI", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-12050000, 3008338)))

ggplot(all_states) +
  geom_sf(aes(geometry = tile_map)) +
  geom_sf_text(aes(geometry = tile_map, label = abbreviation),
               fun.geometry = function(x) st_centroid(x)) +
  theme_void()

## ----echo=FALSE---------------------------------------------------------------
all_states <- all_states %>%
  mutate(party = factor(party, c("Republican", "Democrat")))

## ----fig.height=4.5, fig.width=5.5, fig.align='center'------------------------
ggplot(all_states) +
  geom_sf(aes(geometry = tile_map, fill = party)) +
  geom_sf_text(aes(geometry = tile_map, label = abbreviation),
               fun.geometry = function(x) st_centroid(x)) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Party Affiliation of United States Governors (2020)") +
  theme_void()

## ----eval=FALSE---------------------------------------------------------------
# st_write(governors$tile_map, "us_tilemap.shp")

## ----fig.height=6, fig.width=7, fig.align='center'----------------------------
us_maps <- many_maps(governors$geometry, governors$abbreviation,
                     prop = c(0, 0.1), interpolate = c(0.5, 1),
                     smoothness = c(0, 20), shift = list(c(0,0), c(0,0.5)))

## ----fig.height=3, fig.width=4, fig.align='center'----------------------------
governors <- governors %>%
  mutate(square_map = us_maps$map[[1]])

ggplot(governors) +
  geom_sf(aes(geometry = square_map)) +
  geom_sf_text(aes(geometry = square_map, label = abbreviation)) +
  theme_void()

## ----eval=FALSE---------------------------------------------------------------
# plot_many_maps(us_maps$map, governors$abbreviation)

## ----echo=FALSE, fig.height=5, fig.width=6, fig.align='center'----------------
neighbors <- st_touches(governors$geometry)
crs <- st_crs(governors$geometry)
R <- length(governors$geometry)
A <- sum(st_area(governors$geometry))
s <- as.numeric(sqrt(A/R))

centroids <- tilemaps:::transform_centroids(governors$geometry, neighbors, crs, s, prop = 0.1)

interpolated_centroids <- tilemaps:::interpolate_centroids(centroids$noisy_centroids,
                                                           centroids$transformed_centroids,
                                                           crs, interpolate = 0.75)

centroids_df <- data.frame(st_coordinates(c(centroids$noisy_centroids,
                                            centroids$transformed_centroids,
                                            interpolated_centroids)))
centroids_df <- centroids_df %>%
  mutate(centroids = c(rep("noisy", nrow(governors)), 
                       rep("fully-transformed", nrow(governors)),
                       rep("interpolated", nrow(governors))),
         region = rep(governors$abbreviation, 3))

centroids_df$centroids <- factor(centroids_df$centroids, c("noisy", "interpolated",
                                                           "fully-transformed"))

ggplot(governors) +
  geom_sf(aes(geometry = geometry)) +
  geom_point(data = centroids_df, aes(X, Y, color = centroids)) +
  geom_line(data = centroids_df, aes(X,Y, group = region)) +
  scale_color_brewer(palette = "YlGnBu") +
  theme_void()

## ----echo=FALSE, fig.height=3, fig.width=6, fig.align='center'----------------
transformed_boundary <- tilemaps:::transform_boundary(governors$geometry, centroids$noisy_centroids,
                                                      interpolated_centroids)
smoothed_boundary <- smoothr::smooth(transformed_boundary, method = "ksmooth",
                                     smoothness = 20)

transformed_coords <- data.frame(st_coordinates(st_boundary(transformed_boundary)))
smoothed_coords <- data.frame(st_coordinates(st_boundary(smoothed_boundary)))
original_coords <- data.frame(st_coordinates(st_boundary(st_union(governors$geometry))))

legend_order <- c("original", "transformed", "smoothed")

ggplot() +
  geom_path(data = original_coords, aes(X,Y, group = L1, color = "original")) +
  geom_path(data = transformed_coords, aes(X,Y, group = L1, color = "transformed")) +
  geom_path(data = smoothed_coords, aes(X,Y, group = L1, color = "smoothed")) +
  theme_void() +
  scale_color_discrete(name = "boundary", breaks = legend_order)

## ----echo=FALSE---------------------------------------------------------------
tiles <- tilemaps:::fit_tiles(smoothed_boundary, nrow(governors),
                              s, shift = c(0.5,0.5))

## ----echo=FALSE, fig.height=3, fig.width=4, fig.align='center'----------------
permutation <- tilemaps:::assign_regions(interpolated_centroids, sf::st_centroid(tiles))
final_map <- tiles[order(permutation)]

governors <- governors %>%
  mutate(map = final_map)

ggplot(governors) +
  geom_sf(aes(geometry = map)) +
  geom_sf_text(aes(geometry = map, label = abbreviation)) +
  theme_void()

