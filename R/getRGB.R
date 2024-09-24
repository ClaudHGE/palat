#' Calculate the red (R), green (G), and blue (B) color code from two or more values
#'
#' @param lat a vector, or column name (df$colname) for latitude values. Must have at least two values
#' @param lon a vector, or column name (df$colname) for latitude values. Must have at least two values
#'
#' @return A numeric vector or matrix containing RGB values calculated from the input data.
#' The output includes the following components:
#' - R1, R2, R3, R4, R5: Red channel values.
#' - G1, G2, G3, G4, G5: Green channel values.
#' - B1, B2, B3, B4, B5: Blue channel values.
#' Each value represents an intensity level for the respective color channel.
#' @export
getRGB <- function(lat, lon) {
  lat_range <- c(min(lat), max(lat))
  lon_range <- c(min(lon), max(lon))
  # Normalize latitude and longitude
  norm_lat <- (lat - min(lat_range)) / (max(lat_range) - min(lat_range))
  norm_lon <- (lon - min(lon_range)) / (max(lon_range) - min(lon_range))

  # Calculate R, G, B components
  R <- (1 - norm_lon) * 255  # Decrease from east to west
  G <- norm_lon * 255  # Decrease from west to east
  B <- (1 - norm_lat) * 255  # Decrease from south to north

  return(c(R = R, G = G, B = B))
}

#'
#' @examples
#' ---using values--------
#' getRGB(lat = c(4.611, 6.251, 3.437, 10.391, 10.963),
#' lon = c(-74.083, -75.563, -76.522, -75.514, -74.796))
#' -----------------------
#'
#' ---using vectors-------
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' getRGB(lat = latitude, lon = longitude)
#' -----------------------
#'
#' ---with a data frame---
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' dataframe <- data.frame(latitude, longitude)
#' getRGB(lat = dataframe$latitude, lon = dataframe$longitude)
#' -----------------------
#'
#'
#'
