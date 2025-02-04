
#' Average RGB band color intensity within members of the same cluster
#'
#' @description
#' This function is useful when which to generaliz your map and get only one point per cluster.
#' However, it calculates the average coordinate of the points (not the centroid). Therefore,
#' should only be applied to generalize points that are not too far away from each other, so
#' the earth curvature does not have a meaningful effect. You must have the Red, Green and Blue values
#' for each row so the color is also computed. I suggest running getColors() first.
#'
#' @param df a data frame with numeric Red, Green and Blue values (three columns)
#' and a column that aggregates the points (e.g., Cluster)
#' @param k Column name with the information to aggregate/generalize the data
#' (e.g., cluster, population, group, region, ...). Default "Cluster".
#' @param lati the column name of df that contains the latitude values. Default "lat".
#' @param long the column name of df that contains the longitude values. Default "lon".
#' @param r red channel column name. From 0 to 255. Default "Red"
#' @param g green channel column name. From 0 to 255. Default "Green"
#' @param b blue channel column name. From 0 to 255. Default "Blue"
#' @param size size of the points to be plotted, passed through platMap()
#' @param label if labels are to be printed, column name in df with the labels. Default "FALSE"
#'
#' @return data frame with as many unique values as there are
#' in the cluster column.
#' The data frame contains six columns as follows:
#' - Cluster: the original name of the column is changed to Cluster,
#' - Red, Green and Blue: intensity of each color band respective to each
#' cluster.
#' - RGB: RGB color in decimal format respective to each cluster.
#' - HEX: RGB color in hexadecimal format respective to each cluster.
#' - lat: mean latitude respective to to each cluster.
#' - lon: mean longitude respective to to each cluster.
#'
#' @import stats
#' @export
#'
#' @examples
#' # Sample data frame with the color band values
#' df <- data.frame(
#' lat = c(4.611, 6.251, 3.437, 10.391, 10.963),
#' long = c(-74.083, -75.563, -76.522, -75.514, -74.796),
#' clusters = c("Andes", "Andes", "Andes", "Coast", "Coast"),
#' Red = c(255, 200, 100, 50, 0),
#' Green = c(100, 150, 200, 255, 50),
#' Blue = c(50, 75, 100, 125, 150)
#' )
#'
#' # Obtain a RGB colors per cluster only
#' colors <- pointCluster(df = df, k = "clusters", long = "long")
#' print(colors) # Only resultant values. One column per cluster.
#'
#'
#' # Workflow from latitude and longitude values
#' df <- data.frame(
#' lat = c(4.611, 6.251, 3.437, 10.391, 10.963),
#' long = c(-74.083, -75.563, -76.522, -75.514, -74.796),
#' clusters = c("Andes", "Andes", "Andes", "Coast", "Coast"))
#'
#' # Get the RGB bands. This works also with getRGB(..., bind = TRUE)
#' df1 <- getColors(df = df, lon = "long", map = FALSE)
#' df1 <- pointCluster(df = df1, k = "clusters", long = "long", label = "clusters")
#'
#'

pointCluster <- function(df, k, r = "Red", g = "Green", b = "Blue", lati = "lat",
                         long = "lon", size = 2, label = FALSE) {

  original_k <- k # Preserve the original name
  original_lat <- lati # Preserve the original name
  original_lon <- long # Preserve the original name
  colnames(df)[colnames(df) == k] <- "klu" # Standardize the colname for the function
  colnames(df)[colnames(df) == lati] <- "latte" # Standardize the colname for the function
  colnames(df)[colnames(df) == long] <- "longhi" # Standardize the colname for the function


  # Calculate average RGB values by cluster
  average_colors <-
    stats::aggregate(cbind(get(r), get(g), get(b), latte, longhi) ~ klu, data = df, FUN = mean)
  colnames(average_colors) <- c("klu", "Red", "Green", "Blue", "latte", "longhi")

  # Get the RBG triplet in decimal format
  average_colors$RGB <- with(average_colors,
                             paste(round(Red), round(Green), round(Blue),
                                   sep = ", "))
  # Convert RGB decimal to Hexadecimal
  average_colors$HEX <- with(average_colors,
                             sprintf("#%02X%02X%02X", round(Red), round(Green), round(Blue)))

  plot(platMap(df = average_colors, size = size, lon = "longhi", lat = "latte"))

  colnames(average_colors)[colnames(average_colors) == "klu"] <- original_k
  colnames(average_colors)[colnames(average_colors) == "latte"] <- original_lat
  colnames(average_colors)[colnames(average_colors) == "longhi"] <- original_lon

  return(average_colors)
  }

