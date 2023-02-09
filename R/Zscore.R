#' Creating Z scores
#'
#' @param x quantitative vector quantities
#'
#' @return a list containing z and x
#' @export
#'
#' @examples
#' z(1:4)
#'
z <- function(x){
  z <- (x - mean(x))/sd(x)
  list(z = z, x = x)
}
