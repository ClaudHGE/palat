

#' Convert RGB bands into a HEX color
#'
#' @param df data frame with the red, green, and blue channels.
#' @param r red channel column name. From 0 to 255. Default "R"
#' @param g green channel column name. From 0 to 255. Default "G"
#' @param b blue channel column name. From 0 to 255. Default "B"
#' @param bind logical. Whether the output data frame should be merged with the input data frame. Default TRUE
#'
#' @return a one-column dataframe with the HEX codes corresponding to each triad of RGB band codes
#' @export
#'
#' @examples
#' # Get a data frame
#' df <- data.frame(
#'   R = c(0.0000, 154.7355, 255.0000, 149.6125, 74.5449),
#'   G = c(255.0000, 100.2645, 0.0000, 105.3875, 180.4551),
#'   Blue = c(215.22190, 159.65453, 255.00000, 19.38081, 0.00000)
#'   )
#' getHEX(df, b = "Blue")
#'
#' # Generate colors from coordinates
#' latitude = c(4.611, 6.251, 3.437, 10.391, 10.963)
#' longitude = c(-74.083, -75.563, -76.522, -75.514, -74.796)
#' df <- data.frame(latitude, longitude)
#' RGB_df <- getRGB(df, lat = "latitude", lon = "longitude")
#' getHEX(df = RGB_df)



getHEX <- function(df, r = "R", g = "G", b = "B", bind = TRUE) {
  # Extract RGB values from the specified columns
  r <- df[[r]]
  g <- df[[g]]
  b <- df[[b]]

  n_colors <- length(r)

  # Convert RGB to HEX
  hex_color <- sprintf("#%02X%02X%02X", as.integer(r), as.integer(g), as.integer(b))

  # Create a data frame with hex colors
  hex_df <- data.frame(hex_color)
  rownames(hex_df) <- seq(1, n_colors)

  if(bind == TRUE){
    bind_hex_df <- cbind(df, hex_df)
    return(bind_hex_df)
  }
  return(hex_df)
  }
