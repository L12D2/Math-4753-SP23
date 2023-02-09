#' Make a vector of Z scores
#'
#' @param x A vector of numeric values
#'
#' @importFrom stats sd
#'
#' @return A vector of z values
#' @export
#'
#' @examples
#' my_z(1:4)
#'
my_z <- function(x){
  z <- (x - mean(x))/sd(x)
  list(z = z, x = x)
}

