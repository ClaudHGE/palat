

#' Convert RGB bands into a HEX color
#'
#' @param df data frame with the red, green, and blue channels.
#' @param r red channel column name. From 0 to 255. Default "Red"
#' @param g green channel column name. From 0 to 255. Default "Green"
#' @param b blue channel column name. From 0 to 255. Default "Blue"
#' @param bind logical. Whether the output data frame should be merged with the input data frame. Default TRUE
#'
#' @return a data frame with one column named HEX, which contains the HEX codes corresponding to each triad of RGB band codes.
#' OR the input data frame with this column added (Default)
#'
#' @export
#'
#' @examples
#' # Sample data frame with color intensity for the bands Red, Green and Blue.
#' df <- data.frame(
#'   R = c(0.0000, 154.7355, 255.0000, 149.6125, 74.5449),
#'   G = c(255.0000, 100.2645, 0.0000, 105.3875, 180.4551),
#'   Blue = c(215.22190, 159.65453, 255.00000, 19.38081, 0.00000)
#'   )
#' # Get the column with the hexadecimal (Hex) code not including the input data frame.
#' getHEX(df, r = "R", g = "G", bind = FALSE)
#'
#' # Generate colors from coordinates
#' # - Sample data frame
#' latitude <- c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude <- c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' df <- data.frame(latitude, longitude)
#' # - Get the Red, Green and Blue bands intensity
#' RGB_df <- getRGB(df, lat = "latitude", lon = "longitude")
#' # - Get the hexadecimal code
#' getHEX(df = RGB_df)
#' # The output includes the input data frame



getHEX <- function(df, r = "Red", g = "Green", b = "Blue", bind = TRUE) {
  # Extract RGB values from the specified columns
  r <- df[[r]]
  g <- df[[g]]
  b <- df[[b]]

  n_colors <- length(r)

  # Convert RGB to HEX
  HEX <- sprintf("#%02X%02X%02X", as.integer(r), as.integer(g), as.integer(b))

  # Create a data frame with hex colors
  hex_df <- data.frame(HEX)
  rownames(hex_df) <- seq(1, n_colors)

  if(bind == TRUE){
    bind_hex_df <- cbind(df, hex_df)
    return(bind_hex_df)
  }
  return(hex_df)
  }
