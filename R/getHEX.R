

#' Convert RGB bands into a HEX color
#'
#' @param r red channel value(s). From 0 to 255.
#' @param g green channel value(s). From 0 to 255.
#' @param b blue channel value(s). From 0 to 255.
#'
#' @return a one-column dataframe with the HEX codes corresponding to each triad of RGB band codes
#' @export
#'
#' @examples
#' # Using values
#' getHEX(0.0000, 255.0000, 215.22190)
#'
#' # Using a data frame of with the red, green and blue bands
#' df <- data.frame(
#'   R = c(0.0000, 154.7355, 255.0000, 149.6125, 74.5449),
#'   G = c(255.0000, 100.2645, 0.0000, 105.3875, 180.4551),
#'   B = c(215.22190, 159.65453, 255.00000, 19.38081, 0.00000)
#'   )
#' getHEX(r = df$R, g = df$G, b = df$B)
#'
#' # Generate colors from coordinates
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' df <- data.frame(latitude, longitude)
#' RGB_df <- getRGB(lat = df$latitude, lon = df$longitude)
#' df_wRGB <- cbind(df, RGB_df) #optional
#' HEX_col <- getHEX(r = df_wRGB$R, g = df_wRGB$G, b = df_wRGB$B)
#' df_wRBGnHEX <- cbind(df_wRGB, HEX_col)


getHEX <- function(r, g, b) {
  n_colors <- length(r)
  hex_color <- sprintf("#%02X%02X%02X", as.integer(r), as.integer(g), as.integer(b))
  hex_df <- data.frame(hex_color)
  rownames(hex_df) <- seq(1, n_colors)

  return(hex_df)
  }
