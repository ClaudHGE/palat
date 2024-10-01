
#' Average RGB band color intensity within members of the same cluster
#'
#' @description
#' This function is useful when you have the Red, Green and Blue values for some
#' points at a lower level category (e.g., cities) and you want to obtain an
#' average color that define a group of your points (e.g., regions)
#' To map this output use platMap()
#'
#' @param df a data frame with numeric Red, Green and Blue values (three columns)
#' and a column that aggregates the points (e.g., Cluster)
#' @param k Column name with the information to aggregate/generalize the data
#' (e.g., cluster, group, region, country, ...). Default Cluster.
#' @param r red channel column name. From 0 to 255. Default "Red"
#' @param g green channel column name. From 0 to 255. Default "Green"
#' @param b blue channel column name. From 0 to 255. Default "Blue"
#' @param suffix default ".K". This is a suffix added to the new columns in the
#' output data frame so they can be distinguished from those in the input data frame.
#' @param bind Logical. Default TRUE. Whether the output data frame is to be
#' merged with the input data frame.
#' #'
#' @return if bind = FALSE: data frame with as many unique values as there are
#' in the cluster column.
#' The data frame contains six columns as follows:
#' - Cluster: the original name of the column is changed to Cluster,
#' - Red, Green and Blue: intensity of each color band respective to each
#' cluster. If bind = TRUE: the colnames have the added suffix. Default (e.g.) Red.K
#' - RGB: RGB color in decimal format respective to each cluster.
#' If bind = TRUE AND the column RGB was already in the df:
#' the colnames have the added suffix. Default (e.g.) RGB.K
#' - HEX.K: RGB color in hexadecimal format respective to each cluster. Suffix same as above
#' if bind = TRUE:
#' returns the original df with as many columns as the original df
#' plus those mentioned above.
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
#' colors <- colorCluster(df = df, k = "clusters", bind = FALSE)
#' print(colors) # Only resultant values. One column per cluster.
#'
#' # Obtain RGB colors per point and per cluster. The original number of rows,
#' # as well as the columns lat and long are preserved
#' colors <- colorCluster(df = df, k = "clusters", bind = TRUE)
#' print(colors) # Colors per point and per group.
#'
#' # Workflow from latitude and longitude values
#' df <- data.frame(
#' lat = c(4.611, 6.251, 3.437, 10.391, 10.963),
#' long = c(-74.083, -75.563, -76.522, -75.514, -74.796),
#' clusters = c("Andes", "Andes", "Andes", "Coast", "Coast"))
#'
#' # Get the RGB bands. This works also with getRGB(..., bind = TRUE)
#' df1 <- getColors(df = df, lon = "long", map = FALSE)
#' df1 <- colorCluster(df = df1, k = "clusters", bind = TRUE)
#'
#' # produce the map. Note that bind must be TRUE in the previous step
#' platMap(df1, lon = "long", hex = "HEX.K")
#'

colorCluster <- function(df, k = "Cluster", r = "Red", g = "Green", b = "Blue",
                         suffix = ".K", bind = TRUE) {

  original_k <- k # Preserve the original name
  colnames(df)[colnames(df) == k] <- "Cluster" # Standardize the colname for the function


  # Calculate average RGB values by cluster
  average_colors <-
    stats::aggregate(cbind(get(r), get(g), get(b)) ~ Cluster, data = df, FUN = mean)
  colnames(average_colors) <- c("Cluster", "Red", "Green", "Blue")

  # Get the RBG triplet in decimal format
  average_colors$RGB <- with(average_colors,
                               paste(round(Red), round(Green), round(Blue),
                                     sep = ", "))
  # Convert RGB decimal to Hexadecimal
  average_colors$HEX <- with(average_colors,
                               sprintf("#%02X%02X%02X", round(Red), round(Green), round(Blue)))

  if (bind == TRUE) {
    df_binded <- merge(df, average_colors, by = "Cluster", all.x = TRUE,
                       suffixes = c("", suffix))
    colnames(df_binded)[colnames(df_binded) == "Cluster"] <- original_k # Return the original colname
    return(df_binded)
  }
  colnames(average_colors)[colnames(average_colors) == "Cluster"] <- original_k
  return(average_colors)
}
