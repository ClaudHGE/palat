#' Calculate the red (R), green (G), and blue (B) color code from two or more values
#'
#' @param df data frame with latitude and longitude values. Must have at least two rows
#' @param lat the column name of df that contains the latitude values.
#' @param lon the column name of df that contains the longitude values.
#' @param bind logical. Whether the output data frame should be merged with the input data frame. Default TRUE
#'
#' @return A data frame containing RGB values calculated from the input data.
#' The output includes the following columns:
#' - R: Red channel values.
#' - G: Green channel values.
#' - B: Blue channel values.
#' Each row corresponds to a coordinate of latitude and longitude provided.
#' Each value represents an intensity level for the respective color channel.
#' @export
#' @examples
#' # Using a data frame
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' dataframe <- data.frame(latitude, longitude)
#' RGB_df <- getRGB(df = dataframe, lat = "latitude", lon = "longitude")


getRGB <- function(df, lat = "lat", lon = "lon", bind = TRUE) {
  # Extract latitude and longitude values from the specified columns
  lat <- df[[lat]]
  lon <- df[[lon]]

  n_coords <- length(lat)
  lat_range <- c(min(lat), max(lat))
  lon_range <- c(min(lon), max(lon))

  # Normalize latitude and longitude
  norm_lat <- (lat - min(lat_range)) / (max(lat_range) - min(lat_range))
  norm_lon <- (lon - min(lon_range)) / (max(lon_range) - min(lon_range))

  # Calculate R, G, B components
  R <- (1 - norm_lon) * 255  # Decrease from east to west
  G <- norm_lon * 255  # Decrease from west to east
  B <- (1 - norm_lat) * 255  # Decrease from south to north

  # Create data frame with the R, G and B columns
  rgb_df <- data.frame(R = R, G = G, B = B)
  rownames(rgb_df) <- seq(1, n_coords)

  if(bind == TRUE){
    bind_rgb_df <- cbind(df, rgb_df)
    return(bind_rgb_df)
  }
  return(rgb_df)
}


