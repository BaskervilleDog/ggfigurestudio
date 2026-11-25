map_examples <- function() {
  data.frame(
    id = c("counties", "county_classes", "stations", "routes", "raster", "tiles", "contours"),
    plot = c("County choropleth", "Classified county choropleth", "Station points and labels",
             "Survey transects", "Elevation raster", "Elevation tiles", "Elevation contours"),
    packages = c(rep("sf", 4), rep("terra", 3)),
    try_editing = c(
      "Click a county, then choose All areas in layer to apply a continuous fill palette.",
      "Click a county, then choose All areas in layer to apply a discrete fill palette.",
      "Click a station group to change shape or fill. Click a city label to style it.",
      "Click a transect and change its line type or width.",
      "Add a horizontal line at 450. Switch layer 1 from Raster to Tiles.",
      "Edit one cell's border, then choose All cells in layer to change the fill palette.",
      "Click a contour and change its colour or width. Add a vertical line at 300."
    ),
    stringsAsFactors = FALSE
  )
}

map_example <- function(id) {
  catalog <- map_examples()
  if (is.numeric(id) && length(id) == 1L && is.finite(id) &&
      id %in% seq_len(nrow(catalog))) id <- catalog$id[id]
  if (!is.character(id) || length(id) != 1L || !id %in% catalog$id)
    stop("Choose an example number or ID from map_examples().", call. = FALSE)
  package <- catalog$packages[match(id, catalog$id)]
  if (!requireNamespace(package, quietly = TRUE))
    stop("This example requires ", package, ".", call. = FALSE)
  switch(id,
    counties = map_county_plot(),
    county_classes = map_county_plot(discrete = TRUE),
    stations = map_station_plot(),
    routes = map_route_plot(),
    raster = map_elevation_base() + ggplot2::geom_raster() +
      ggplot2::labs(title = "Volcano elevation raster"),
    tiles = map_elevation_base() + ggplot2::geom_tile(colour = "#23363E", linewidth = 0.12) +
      ggplot2::labs(title = "Volcano elevation cells"),
    contours = map_contour_plot()
  )
}

map_counties <- function() {
  sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
}

map_county_plot <- function(discrete = FALSE) {
  counties <- map_counties()
  counties$births <- counties$BIR74
  if (discrete) {
    counties$births <- cut(counties$births, breaks = c(0, 1000, 3000, Inf),
                           labels = c("Up to 1,000", "1,001 to 3,000", "Above 3,000"))
  }
  scale <- if (discrete) ggplot2::scale_fill_brewer(palette = "Blues") else
    ggplot2::scale_fill_viridis_c(labels = function(values) {
      format(values, big.mark = ",", trim = TRUE, scientific = FALSE)
    })
  ggplot2::ggplot(counties) +
    ggplot2::geom_sf(ggplot2::aes(fill = births), colour = "white", linewidth = 0.2) +
    scale +
    ggplot2::coord_sf(default_crs = sf::st_crs(4326)) +
    ggplot2::labs(title = "North Carolina county map", fill = "Births",
         caption = "County data bundled with sf") +
    ggplot2::theme_minimal()
}

map_station_data <- function() {
  stations <- data.frame(
    city = c("Asheville", "Charlotte", "Winston-Salem", "Greensboro",
             "Raleigh", "Fayetteville", "Greenville", "Wilmington"),
    type = rep(c("Field station", "Weather station"), each = 4),
    lon = c(-82.55, -80.84, -80.24, -79.79, -78.64, -78.88, -77.37, -77.94),
    lat = c(35.60, 35.23, 36.10, 36.07, 35.78, 35.05, 35.61, 34.23)
  )
  locations <- sf::st_as_sf(stations, coords = c("lon", "lat"), crs = 4326)
  xy <- sf::st_coordinates(sf::st_transform(locations, 32119)) / 1000
  transform(stations, x = xy[, "X"], y = xy[, "Y"])
}

map_state_background <- function() {
  counties <- sf::st_transform(map_counties(), 32119)
  state <- sf::st_cast(sf::st_union(sf::st_geometry(counties)), "MULTIPOLYGON")
  xy <- as.data.frame(sf::st_coordinates(state))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = xy, ggplot2::aes(X / 1000, Y / 1000, group = L2, subgroup = L1),
                 fill = "#EDF2E7", colour = "#6B756B", linewidth = 0.35) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "Easting (km)", y = "Northing (km)",
         caption = "Illustrative network at city locations") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

map_station_plot <- function() {
  stations <- map_station_data()
  stations$label_y <- stations$y + c(14, 14, 14, -14, 14, -14, 14, -14)
  map_state_background() +
    ggplot2::geom_point(data = stations, ggplot2::aes(x, y, fill = type, shape = type),
               colour = "#253542", size = 3.7) +
    ggplot2::geom_text(data = stations, ggplot2::aes(x, label_y, label = city, group = city),
              size = 3) +
    ggplot2::scale_fill_manual(values = c("#27A08D", "#DF9353")) +
    ggplot2::scale_shape_manual(values = c(21, 24)) +
    ggplot2::labs(title = "Field and weather station network", fill = "Station", shape = "Station")
}

map_route_plot <- function() {
  stations <- map_station_data()
  routes <- transform(stations, route = rep(c("Western transect", "Eastern transect"), each = 4))
  map_state_background() +
    ggplot2::geom_path(data = routes, ggplot2::aes(x, y, colour = route, linetype = route), linewidth = 1.2) +
    ggplot2::geom_point(data = stations, ggplot2::aes(x, y), size = 2.1) +
    ggplot2::labs(title = "Illustrative survey transects", colour = "Transect", linetype = "Transect")
}

map_elevation_cells <- function() {
  # Use a small, regular local grid so all tiles remain easy to select.
  elevation <- terra::rast(datasets::volcano, crs = "")
  terra::ext(elevation) <- c(0, ncol(elevation) * 10, 0, nrow(elevation) * 10)
  elevation <- terra::aggregate(elevation, fact = 3, fun = "mean")
  names(elevation) <- "elevation"
  terra::as.data.frame(elevation, xy = TRUE)
}

map_elevation_base <- function() {
  ggplot2::ggplot(map_elevation_cells(), ggplot2::aes(x, y, fill = elevation)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_fixed() +
    ggplot2::labs(fill = "Elevation", x = "Local easting (m)", y = "Local northing (m)") +
    ggplot2::theme_minimal()
}

map_contour_plot <- function() {
  cells <- map_elevation_cells()
  x <- sort(unique(cells$x))
  y <- sort(unique(cells$y))
  z <- matrix(NA_real_, nrow = length(x), ncol = length(y))
  z[cbind(match(cells$x, x), match(cells$y, y))] <- cells$elevation
  lines <- grDevices::contourLines(x, y, z, levels = c(120, 150, 175))
  paths <- do.call(rbind, lapply(seq_along(lines), function(i) {
    line <- lines[[i]]
    data.frame(x = line$x, y = line$y, elevation = factor(line$level), path = i)
  }))
  ggplot2::ggplot(paths, ggplot2::aes(x, y, group = path, colour = elevation)) +
    ggplot2::geom_path(linewidth = 1.15) +
    ggplot2::scale_colour_manual(values = c("#237A6C", "#A17315", "#963F69")) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Volcano elevation contours", colour = "Contour level",
         x = "Local easting (m)", y = "Local northing (m)") +
    ggplot2::theme_minimal()
}
