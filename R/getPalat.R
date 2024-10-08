#' Get the color palette
#'
#' @param df A data frame containing color information.
#' @param hex A string specifying the column name for hex color codes. Default "HEX". You also might use "HEX.K"
#' @param labels A string specifying the column name for labels. Default NULL.
#' @param plot Logical. Whether the palette is to be visualized. Default TRUE.
#' @param ... Other arguments passed to geom_tile() and geom_text()
#'
#' @import ggplot2
#' @return A named character vector of the color codes and labels if provided.
#' @export
#'
#' @examples
#' df <- data.frame(region = c("Andes", "Caribe"), HEX = c("#00FFD7", "#9A649F"))
#' getPalat(df, labels = "region")
#'
#' # Sample data frame
#' df <- data.frame(
#' region = c("Andes", "Andes", "Andes", "Caribe", "Caribe"),
#' city = c("Bogota", "Medellin", "Cali", "Barranquilla", "Santa Marta"),
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963),
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796))
#'
#' # Get the colors in the same df for each city
#' df <- getColors(df, lat = "latitude", lon = "longitude", map = FALSE)
#' palatXcity <- getPalat (df, labels = "city")
#' print(palatXcity)
#'
#' # Get the colors in the same df for each region
#' df <- colorCluster(df, k = "region")
#' palatXregion <- getPalat (df, labels = "region", hex = "HEX.K")
#' print(palatXregion)
#'
#' # Without labels nor hex codes
#' myColors <- data.frame(colors = c("gray36", "gray56"))
#' getPalat(myColors, hex = "colors")
#'



getPalat <- function(df, hex = "HEX", labels = NULL, plot = TRUE, ...) {
  # Extract unique rows for specified hex values
  unique_rows <- as.data.frame(unique(na.omit(df[, c(hex)])))
  colores <-  unique(na.omit(df[[hex]]))

  # Determine labels
  if (is.null(labels)) {
    nombres <- seq_along(colores)  # Use a sequence of numbers as labels if labels are NULL
    print("Don't you prefer setting labels? add the argument `labels`")
  } else {
    nombres <- unique(na.omit(df[[labels]]))
  }

  # Create a named vector for colors
  palat1 <- setNames(colores, nombres)

  if (plot == TRUE) {
    g <- ggplot2::ggplot(data = unique_rows, aes(x = 1, y = seq_along(nombres), fill = colores)) +
      ggplot2::geom_tile(...) +
      ggplot2::scale_fill_identity() +  # Use colors directly
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_text(aes(label = paste(nombres,colores, sep = ":  ")), color = "black", vjust = 1.5, ...)
    print(g)
  }
  return(palat1)
}
