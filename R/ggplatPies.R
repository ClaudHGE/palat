#' # Map pie charts given the latitude, longitude, color, proportions and groups
#'
#' @param df data frame with latitude, longitude, and color as values.
#' @param lat the column name of df that contains the latitude values. Default "lat".
#' @param lon the column name of df that contains the longitude values. Default "lon".
#' @param k column name with the names of the clusters (e.g., group, region).
#' # The factors or elements in k must be equal as the colnames in the df that contains the proportions.
#' @param hex Vector or column name that contains the color relative to the cluster. Default "HEX.K".
#' @param radius radius of the pie charts. Default 0.2.
#' @param land color of the land on the map. Default "gray".
#' @param coast color of the coast line (or country line). Default "white".
#' @param expand factor to expand the area of the map. Bigger pies require a bigger map area range.
#' @param pie_border_col color of the line that draws the pie chart. Default "black".
#' @param pie_line_size thickness of the line that draws the pie chart. Default 0.1.
#' @param pie_alpha transparency level of the pie chart
#' @param legend position of the legend. Default "none".
#' @param mapid map used by ggplot2::geom_map. Default "region". Options are "subregion", "id", "state".
#'
#' @return a ggplot object with pie charts showing the proportion of each category with the colors given the location.
#' @import ggplot2
#' @import sf
#' @import scatterpie
#'
#' @export
#'
#' @examples
#'
#' # Sample data frame without the colors
#' df1 <- data.frame(
#'   lati = c(4.611, 6.251, 3.437, 10.391, 10.963, 7.984722),
#'   lon = c(-74.083, -75.563, -76.522, -75.514, -74.796, -75.198056),
#'   # [6] doesn't entirely belong to "Andes" or "Coast" region
#'   region = c("Andes", "Andes", "Andes", "Coast", "Coast", NA),
#'   # the following are the proportions.
#'   # Please not that the column names match the names of the region (cluster)
#'   Andes = c(1, 1, 1, 0, 0, 0.5),
#'   Coast = c(0, 0, 0, 1, 1, 0.5)
#' )
#' # Get the colors per sample and bind them to df
#' df1 <- getColors(df1, map = FALSE, lat = "lati")
#'
#' # Get the average color per region
#' df1 <- colorCluster(df1, k = "region")
#' # the resultant df contains the necessary columns for ggplatPies.
#'
#' # Plot the pie charts on a map
#' plot <- ggplatPies(df = df1, lat = "lati", k = "region", radius = 0.4,
#'                    expand = 0.15, pie_alpha = 1, legend = "right")

ggplatPies <- function(df, lat = "lat", lon = "lon", k, hex = "HEX.K",
                       mapid = "region", radius = 0.2, land = "gray", coast = "white", expand = 0.05,
                    pie_border_col = "black", pie_line_size = 0.1, pie_alpha = 1,
                    legend = "none") {


  # Load world map data
  world <- ggplot2::map_data('world')

  # Calculate the ranges for latitude and longitude
  lat_range <- range(df$lat, na.rm = TRUE)
  lon_range <- range(df$lon, na.rm = TRUE)

  # Add extra padding to the ranges
  lat_padding <- diff(lat_range) * expand  # Add % padding on the latitude
  lon_padding <- diff(lon_range) * expand  # Add % padding on the longitude

  # Adjust the ranges
  lat_range <- c(lat_range[1] - lat_padding, lat_range[2] + lat_padding)
  lon_range <- c(lon_range[1] - lon_padding, lon_range[2] + lon_padding)

  #get palette
  pal <- getPalat(df, hex = hex, labels = k)

  # Adjust the radius of the pie
  df$radius <- rep(x = radius, nrow(df))

  # Create plot
  ## Create the base map
  p <- ggplot2::ggplot(world, aes(lon, lat)) +
    ggplot2::geom_map(map=world, aes(map_id = mapid), fill = land, color = coast) +
    ggplot2::coord_sf(xlim = lon_range, ylim = lat_range, expand = TRUE) +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal()

  ## Add the pies
  g <- p + scatterpie::geom_scatterpie(aes(x = lon, y = lat, group = k, r = radius),
                           data = df,
                           cols = names(pal), #palette
                           color = pie_border_col,
                           alpha = pie_alpha, size = pie_line_size) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme(legend.position = legend)

  # Return the plot object
  return(g)
}

