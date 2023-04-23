#' My dnorm Curve
#'
#' @param mu #description
#' @param sigma # description
#' @param a #description
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a){

  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  x_curve1 <- seq(mu - 3*sigma, a, length = 1000)
  y_curve1 <- dnorm(x_curve1, mu, sigma)

  polygon(c(mu-3*sigma, x_curve1, a), c(0, y_curve1, 0), col = "Blue")
  area <- pnorm(a, mean = mu, sd = sigma)
  area <- round(area, 4)
  text(x=2, y = 0.03, paste("Area = ", area, sep=""))

}
