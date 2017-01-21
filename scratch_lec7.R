sigma <- function(x)  1/(1 + exp(-x))

x <- seq(from = -5, to = 5, by = 0.1)
plot(x,sigma(x))

w_xh <- 0.5
w_hh <- -1.0
w_hy <- -0.7
h_bias <- -1.0
y_bias <- 0.0

x0 <- 9
x1 <- 4

h0 <- sigma(h_bias + x0 * w_xh)
h1 <- sigma(h_bias + x1 * w_xh + h0 * w_hh)
y1 <- y_bias + w_hy * h1

w_xh <- -0.1
w_hh <- 0.5
w_hy <- 0.25
h_bias <- 0.4
y_bias <- 0.0

x0 <- 18
x1 <- 9

h0 <- sigma(h_bias + x0 * w_xh)
h1 <- sigma(h_bias + x1 * w_xh + h0 * w_hh)
y1 <- y_bias + w_hy * h1
y0 <- y_bias + w_hy * h0
