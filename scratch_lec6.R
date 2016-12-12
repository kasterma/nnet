library(pryr)

# set up function

f1 <- function(x, y) x^2 + y^2

# compute gradient

library(numDeriv)
f1_h = f(x, f1(x[1], x[2]))
grad(func = f1_h, x = c(1,1))

# do a couple of steps of descent

eps <- 0.1
N <- 20
vals <- vector("list", length = N)
vals[[1]] <- c(1,2)
grad <- vector("list", length = N - 1)

for (idx in seq_len(N - 1)) {
  grad[[idx]] <- grad(func = f1_h, x = vals[[idx]])
  vals[[idx + 1]] <- vals[[idx]] - eps * grad[[idx]]
}

# plot the descent

library(ggplot2)

dat <- setNames(as.data.frame(do.call(rbind, vals)), c('x', 'y'))
ggplot(dat = dat, aes(x = x, y = y)) + geom_point()

library(dplyr)

dat_contour <-
  expand.grid(x = seq(0, 2, by = 0.2), y = seq(0, 2, by = 0.2)) %>%
  mutate(z = f1(x,y))

dat_steps <-
  cbind(dat[1:(N - 1),], rename(dat[2:N,], xend = x, yend = y))

ggplot(dat = dat, aes(x = x, y = y)) +
  geom_contour(dat = dat_contour, aes(x = x, y = y, z = z)) +
  geom_curve(dat = dat_steps, aes(x = x, y = y, xend = xend, yend= yend),
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = 0, color = "grey") +
  geom_point()
