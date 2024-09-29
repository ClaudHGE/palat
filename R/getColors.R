#' Add the R, G, B, and HEX columns to the input data frame. Optionally, map the results.
#'
#' @param df A data frame with latitude and longitude columns
#' @param lat Column name that contains the latitude values. Default "lat"
#' @param lon Column name that contains the longitude values. Default "lon"
#' @param map Logical. Whether a map is to be plotted. Default TRUE
#'
#' @return A data frame with the original columns and values with the added R, G, B and HEX columns
#' @export
#'
#'
#' @examples
#' # Sample data frame
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' dataframe <- data.frame(latitude, longitude)
#'
#' # Call the function without plotting the map
#' getColors(df = dataframe, lat = "latitude", lon = "longitude", map = FALSE)
#'
#' # Call the function and plot the map
#' getColors(df = dataframe, lat = "latitude", lon = "longitude")
#'
getColors <- function(df, lat = "lat", lon = "lon", map = TRUE) {
  rgb <- getRGB(df, lat, lon, bind = TRUE)
  rgb_hex <- getHEX(df = rgb, r = "R", g = "G", b = "B", bind = TRUE)
  print(head(rgb_hex))

  if (map == TRUE) {
    map <- plat_map(rgb_hex, lat, lon, hex = "hex_color", size = 3)
    print(map)
  }
  return(rgb_hex)
}
