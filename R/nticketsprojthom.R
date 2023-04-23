#' Airline ticket problem
#'
#' @param N # tickets
#' @param gamma # gamma distribution
#' @param p # probability
#'
#' @return
#'
#'
utils::globalVariables(c("graphics", "abline", "barplot", "curve", "layout",
                         "polygon", "text", "title", "stats", "dnorm", "optimize", "pbinom", "pnorm", "qnorm", "x", "ntickets"))

#' @importFrom graphics
#' @importFrom abline
#' @importFrom barplot
#' @importFrom curve
#' @importFrom layout
#' @importFrom polygon
#' @importFrom text
#' @importFrom title
#' @importFrom stats
#' @importFrom dnorm
#' @importFrom optimize
#' @importFrom pbinom
#' @importFrom pnorm
#' @importFrom qnorm

#'
#' @examples
ntickets <- function(N, gamma, p) { #document
  n <- N:(N + N/12)
  # layout setup
  matrix <- matrix(1:2, nrow = 2, ncol = 1)

  layout(matrix)

  # Binomial graph
  bin_fun = 1 - gamma - pbinom(N, n, p)
  plot(n, bin_fun, type = 'b', pch = 4,
       col = "blue",
       cex = 0.6,
       xlab = 'n', ylab = 'objective',
       ylim = c(-0.15, 1.1))
  abline(h = 0, col = '#00FF00')
  # Optimize
  indep <- which.min(abs(bin_fun))
  tickets <- N + indep
  # Vertical line
  abline(v = tickets, col = '#CC00FF')
  # Title
  title(main = paste0('Objective vs. n and Optimal Tickets \n (', tickets, '). gamma = ', gamma, sep = " ", 'N = ', N, sep = " ", 'Discrete'),
        cex.main = 0.7)

  # Approximate to the normal graph

  x <- as.numeric(as.character(n))
  norm_fun <- function(n) {

    abs((N+1) - qnorm(1 - gamma, n*p, sqrt(n * p * (1 - p))))
  }

  curve(1 - gamma - pnorm(N + 0.5, mean = x * p, sd = (x * p * (1-p))^0.5), type = 'l',
        col = 'red',
        xlab = 'n', ylab = 'Objective',
        ylim = c(-0.15, 1.1),
        xlim = c(N, N + 30),
        cex = 0.6)
  abline(h = 0, col = '#00FF00')

  # Optimize
  indep_2 <- optimize(norm_fun, interval = c(N, 1000))
  tickets_norm <- indep_2$minimum

  # Vertical line added
  abline(v = tickets_norm, col = '#CC00FF')

  # Title
  title(main = paste0('Objective vs. n and Optimal Tickets \n (', tickets_norm, '). gamma = ', gamma, sep = " ", 'N = ', N, sep = " ", 'normal'),
        cex.main = 0.7)

  # Return a list of results in command line
  list(nd = tickets, nc = tickets_norm, N = N, gamma = gamma, p = p)

}
