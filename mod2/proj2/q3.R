library(tidyverse)

make_movement <- function(a,b,c,N) {
  function(now) {
    st <- now[1]
    it <- now[2]
    nexts <- (1-a)*st + b*st*(1 - (st+it)/N)
    nexti <- (1-c)*it + a*st
    c(nexts,nexti)
  }
}

evolve <- function(move, x0, n) {
  xs <- matrix(ncol = 2, nrow = n + 1)
  xs[1,] <- x0
  for (i in (2:(n+1))) {
    xs[i,] <- move(xs[i-1,])
  }
  xs <- data.frame(xs)
  colnames(xs) <- c('S', 'I')
  xs$t <- 0:n
  round(xs, 2)
}

N <- 100000
a <- 0.3
b <- 1.8
c <- 0.1

evolve(make_movement(a, b, c, N),
       c(1000,0),
       40) %>% 
  gather(key, value, S, I) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() + 
  theme(legend.position = 'none')

b <- 0.2

evolve(make_movement(a, b, c, N),
       c(1000,0),
       100) %>% 
  gather(key, value, S, I) %>% 
  ggplot(aes(t, value, color=key)) + 
  geom_line() + 
  theme(legend.position = 'none')

gamma <- function(a,b,c,N) {
  N*(b-a)/(b*(a+c))
}

b <- 1.8

Sstar <- c*gamma(a, b, c, N)
Istar <- a*gamma(a, b, c, N)

funcmat <- function(a, b, c, N, Sstar, Istar) {
 A11 <- 1-a+b-b*((2*Sstar + Istar)/N)
 A12 <- -b*Sstar/N
 A21 <- 1-c
 A22 <- a
 matrix(c(A11,A21,A12,A22), nrow=2)
}

A1 <- funcmat(a, b, c, N, Sstar, Istar)
A2 <- funcmat(a, b, c, N, 0, 0)
A1
A2
eigen(A1)
eigen(A2)
Mod(eigen(A1)$values)
