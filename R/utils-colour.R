

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert any R colour to a hex colour
#'
#' Return 'none' to indicate a transparent colour
#'
#' @param x any R colour
#'
#' @return hex colour string
#'
#' @importFrom grDevices col2rgb rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r2hex <- function(x) {
  if (is.null(x) || is.na(x) || tolower(x) %in% c('NA', 'none', 'null')) {
    return('none')
  }

  mat <- col2rgb(x)
  rgb(mat[1], mat[2], mat[3], maxColorValue = 255)
}

