library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(fmesher)
library(mgcv)

create_mesh <- function(bb, bb_buffer = 500, max.edge = 500) {
  bnd <- matrix(
    c(
      bb["xmin"] - bb_buffer, bb["ymin"] - bb_buffer,
      bb["xmax"] + bb_buffer, bb["ymin"] - bb_buffer,
      bb["xmax"] + bb_buffer, bb["ymax"] + bb_buffer,
      bb["xmin"] - bb_buffer, bb["ymax"] + bb_buffer,
      bb["xmin"] - bb_buffer, bb["ymin"] - bb_buffer
    ),
    ncol = 2,
    byrow = TRUE
  )
  fm_segm <- fm_segm(bnd)
  mesh <- fm_mesh_2d(interior = bnd, max.edge = max.edge)
  return(mesh)
}

get_grid <- function(mesh){

  # get the finite elements
  fem <- fm_fem(mesh, order = 1)

  # areas
  w <- fem$ta

  # get the centroids of the triangles
  mesh_locs <- mesh$loc[,1:2]
  idx <- mesh$graph$tv
  tri_loc <- as.data.frame(t(apply(idx, 1, function(ii){
    1/3*colSums(mesh_locs[ii,])
  })))

  dat <- cbind(tri_loc, w)
  names(dat) <- c("x", "y", "area")

  return(dat)
}


fit_lgcp <- function(dat, tri, kernel_size) {
  dat$area <- 1e-6

  # response data
  dat$presence <- 1
  tri$presence <- 0

  # bind these two together
  ppm_dat <- rbind(dat, tri)

  # need the log area
  ppm_dat$larea <- log(ppm_dat$area)

  # fit gam
  pp <- gam(presence ~ offset(larea) + s(x, y, bs="tp", k=kernel_size),
    data=ppm_dat, method="REML", family=poisson())

  return(pp)
}

fit_traffic_stop <- function(current_date, date_lags,
                             kernel_size = 40) {
  stops <- read_excel("./data/TRFC STOP DATA .xlsx") %>%
    mutate(date = mdy(`Call Time`)) %>%
    drop_na(Longitude, Latitude, date)

  stops_sf <- st_as_sf(
    stops,
    coords = c("Longitude", "Latitude"),
    crs = 4326
  ) %>%
    filter(
      date >= current_date - days(date_lags),
      date <= current_date
    )

  stops_sf_m <- st_transform(stops_sf, 26915) %>%
    filter(!st_is_empty(geometry))

  coords <- st_coordinates(stops_sf_m)
  dat <- data.frame(
    x = coords[, "X"],
    y = coords[, "Y"]
  )

  bb <- st_bbox(stops_sf_m)
  mesh <- create_mesh(bb, bb_buffer = 1000, max.edge = 2000)
  tri <- get_grid(mesh)
  pp <- fit_lgcp(dat, tri, kernel_size)
  return(list(pp = pp, dat = dat))
}

predict_lgcp <- function(pp, xmin, xmax, ymin,ymax,
                         npred = 100) {
  # predict
  pred <- expand.grid(
    x = seq(xmin, xmax, len=npred),
    y = seq(ymin, ymax, len=npred))
  # areas
  pred$larea <- log(diff(pred$x)[1] * diff(pred$y)[npred])

  # make the prediction
  pred$p <- exp(predict(pp, pred)) / exp(pred$larea)
  return(pred)
}
