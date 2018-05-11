setwd('~/Desktop/Matematik og modeller/mod1/proj1')

library(magrittr)
library(tidyverse)
library(xtable)

sim_mod <- function(move, n, x1) {
  
  xs <- matrix(nrow = n+1, ncol = length(x1))
  xs[1,] <- x1
  for (t in 1:n) {
    xs[t+1,] <- move(xs[t,])
  }
  
  data_frame(t = 0:n, y0=xs[,1], y1=xs[,2], y2 = xs[,3])
  
}

M <- matrix(c(0.4,0.8,0,
              1.2,0,0.5,
              0.6,0,0),nrow=3)

move <- function(x) {
  M%*%x
}

n <- 50
x1 <- c(100,0,0)
sim <- sim_mod(move, n, x1)

sim[1:11,] %>% 
  gather(key, value, y0, y1, y2) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim[1:31,] %>% 
  gather(key, value, y0, y1, y2) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim %<>% 
  mutate(n = y0 + y1 + y2,
         y0_frac = y0/n,
         y1_frac = y1/n,
         y2_frac = y2/n,
         y0_growth_rate = y0/lag(y0),
         y1_growth_rate = y1/lag(y1),
         y2_growth_rate = y2/lag(y2))

sim[1:21,] %>% 
  gather(key, value, y0_frac, y1_frac, y2_frac) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim[1:21,] %>% 
  gather(key, value, y0_growth_rate, y1_growth_rate, y2_growth_rate) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

#b

gen_S <- function(M) {
  bs <- M[1,]
  l1 <- sum(M[2,])
  l2 <- l1*sum(M[3,])
  function(lambda) {
    bs[1]/lambda + 
      l1*bs[2]/(lambda**2) +
      l2*bs[3]/(lambda**3)
  }
}

S <- gen_S(M)
ggplot(data.frame(x=0), aes(x)) +
  stat_function(fun = S) +
  xlim(1,5) +
  xlab('lambda') + ylab('S')

uniroot(function(lambda) S(lambda) - 1,lower=1,upper=10)$root


#c
round(eigen(M)$values, 2)
round(eigen(M)$vectors, 2)
print(xtable(round(eigen(M)$vectors, 2)), include.rownames=F, include.colnames=F, file='qs.tex')
Q <- eigen(M)$vectors
Q

0.65*Q

round(solve(Q)%*%M%*%Q,2)

#e
make_M <- function(a) {
  matrix(c(0.4,0.8,0,
           a,0,0.5,
           0.6,0,0),nrow=3)
}

growth_rate <- function(a) {
  S <- gen_S(make_M(a))
  uniroot(function(lambda) S(lambda) - 1,lower=0.5,upper=2)$root
}

data_frame(a = seq(0.01, 1.5, 0.05)) %>% 
  mutate(lambda = Vectorize(growth_rate)(a)) %>% 
  ggplot(aes(a, lambda)) + 
  geom_hline(yintercept = 1, color='blue', linetype='dashed') +
  geom_vline(xintercept = 0.45, color='green', linetype='dashed') +
  geom_line()

stable <- uniroot(function(a) growth_rate(a) - 1, lower=0.1,upper=1.5)$root
stable

sim <- sim_mod(function(x) make_M(stable)%*%x, 30, c(100,0,0))

sim[1:20,] %>% 
  gather(key, value, y0, y1, y2) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim %<>% 
  mutate(n = y0 + y1 + y2,
         y0_frac = y0/n,
         y1_frac = y1/n,
         y2_frac = y2/n,
         y0_growth_rate = y0/lag(y0),
         y1_growth_rate = y1/lag(y1),
         y2_growth_rate = y2/lag(y2))

sim[1:20,] %>% 
  gather(key, value, y0_frac, y1_frac, y2_frac) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim[1:20,] %>% 
  gather(key, value, y0_growth_rate, y1_growth_rate, y2_growth_rate) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim <- sim_mod(function(x) make_M(stable-0.2)%*%x, 30, c(100,0,0))

sim %>% 
  gather(key, value, y0, y1, y2) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim %<>% 
  mutate(n = y0 + y1 + y2,
         y0_frac = y0/n,
         y1_frac = y1/n,
         y2_frac = y2/n,
         y0_growth_rate = y0/lag(y0),
         y1_growth_rate = y1/lag(y1),
         y2_growth_rate = y2/lag(y2))

sim[1:20,] %>% 
  gather(key, value, y0_frac, y1_frac, y2_frac) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())

sim[1:20,] %>% 
  gather(key, value, y0_growth_rate, y1_growth_rate, y2_growth_rate) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() +
  theme(legend.title = element_blank())
