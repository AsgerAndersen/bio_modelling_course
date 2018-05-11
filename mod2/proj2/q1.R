library(tidyverse)

make_constant <- function(r,u,x0) {
  k <- x0 - u/r
  Vectorize(function(t) {
    k*(1+r)**t + u/r
  })
}

data_frame(t = 0:10) %>% 
  mutate(x = make_constant(0.015, 12000, 200000)(t)) %>% 
  ggplot(aes(t, x)) +
  geom_point()

make_linout <- function(r,u,alp,x0) {
  out <- function(tau) {
    u+alp*tau
  }
  summand <- function(tau) {
    (-out(tau))/((1+r)**tau)
  }
  Vectorize(function(t) {
    s <- sum(map_dbl(0:(t-1), summand))
    ((1+r)**(t-1))*(x0 + s)
  })
}

data_frame(t = 0:10) %>% 
  mutate(x = make_linout(0.015, 1000, 4000, 200000)(t)) %>% 
  ggplot(aes(t, x)) +
  geom_point()

uniroot(function(alp) {make_linout(0.015, 1000, alp, 200000)(10)},
        lower=4000, upper=5000)$root


make_decr_rent <- function(r0, r, u, alpha) {
  function(xt, t) {
    (1 + r0*(r**t))*xt - (u + alpha*t)
  }
}

decr_rent <- make_decr_rent(0.02,0.9,1000,4000)
xs <- vector(length = 11)
xs[1] <- 200000
for (i in 1:10) {
  xs[i+1] <- decr_rent(xs[i],i)
}

data_frame(t = 0:10, x = xs) %>% 
  ggplot(aes(t, x)) +
  geom_point()
