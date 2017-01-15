# Observing a hidden state

create_system <- function(init_state, next_state, observe, name) {
  sys <- list(init_state = init_state, next_state = next_state, observe = observe, name = name)
  class(sys) <- "hidden_state_sys"
  sys
}

print.hidden_state_sys <- function(sys) {
  print(paste("hidden_state_sys:", sys$name))
}

sys1 <- create_system(function() 1, identity, function(x) x + rnorm(1), "stationary, rnorm")

generate_observations <- function(sys, n = 100) {
  obs <- numeric(n)
  state <- sys$init_state()
  for (idx in seq(n)) {
    obs[idx] <- sys$observe(state)
    state <- sys$next_state(state)
  }
  list(observations = obs, end_state = state)
}

obs1 <- generate_observations(sys1)
mean(obs1$observations)

N <- 1000
ests <- numeric(N)
for (idx in seq(N)) {
  o <- generate_observations(sys1)
  ests[idx] <- mean(o$observations)
}

library(ggplot2)
ggplot(data.frame(x = ests), aes(x = x)) + geom_density()

1/sqrt(1000)
sd(ests)
mad(ests)
