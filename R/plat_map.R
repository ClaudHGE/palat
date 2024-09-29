#' Map points given the latitude, longitude and color
#'
#' @param df data frame with latitude, longitude, and color as values.
#' @param lat the column name of df that contains the latitude values. Default "lat".
#' @param lon the column name of df that contains the longitude values. Default "lon".
#' @param hex the column name of df that contains the color codes (or names). Default "HEX".
#' @param size default 3. Size of the points on the map
#' @param ... Other arguments passed to geom_point()
#'
#' @return a map with the points generated from the coordinates and the color determined in the arguments
#'
#' @import ggplot2
#' @import sf
#' @import rnaturalearth
#'
#'
#' @export
#'
#' @examples
#'
#' # Generate colors from coordinates
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' df <- data.frame(latitude, longitude)
#' RGB_df <- getRGB(df, lat = "latitude", lon = "longitude")
#' HEX_df <- getHEX(df = RGB_df)
#' plat_map(df = HEX_df, lat = "latitude", lon = "longitude", alpha = 0.5)
#'
#' colors_vector <- c("#00FFD7", "gold", "dodgerblue3", "firebrick3", "ivory")
#' lat = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' lon = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' df <- data.frame(lat, lon, colors_vector)
#' plat_map(df = df, hex = "colors_vector")
#'
plat_map <- function(df, lat = "lat", lon = "lon", hex = "HEX", size = 3, ...) {
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Extract latitude and longitude ranges
  lat_range <- range(df[[lat]], na.rm = TRUE)
  lon_range <- range(df[[lon]], na.rm = TRUE)

  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world) +
    ggplot2::geom_point(data = df, ggplot2::aes_string(x = lon, y = lat, color = hex), size = size, ...) +
    ggplot2::scale_color_identity() +  # Use the exact colors defined
    ggplot2::coord_sf(xlim = lon_range, ylim = lat_range, expand = TRUE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal()

  # Return the plot object
  return(p)
}




