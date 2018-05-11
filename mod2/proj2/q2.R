library(tidyverse)

a <- 0.8
b <- 10000
c <- 3
c(-a*(c+1), 1+a*c)
A <- matrix(c(a, a, (a-1)*c, a*c), nrow=2, byrow = T)
add <- c(b, b*c)
  
move <- function(now) {
  A%*%now + add
}

evolve <- function(x0, n) {
  xs <- matrix(ncol = 2, nrow = n + 1)
  xs[1,] <- x0
  for (i in (2:(n+1))) {
    xs[i,] <- move(xs[i-1,])
  }
  xs <- data.frame(xs)
  colnames(xs) <- c('C', 'I')
  round(xs, 2)
}
evolve(c(1000000, -50000000000000), 10) %>% 
  ggplot(aes(C,I)) +
  geom_path() +
  geom_abline(intercept = 0, 
              slope = 3/2, 
              color = 'green',
              linetype='dashed')





