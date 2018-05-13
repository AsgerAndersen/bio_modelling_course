library(tidyverse)
library(gridExtra)

bkv <- 0.02
bgv <- 0.30
bgc <- 0.32
fv <- 0.25
fc <- 0.07
la <- 0.05
lb <- 0.025
h1 <- 0.5
h4 <- 0.4

aV <- h1*bkv + h4*bgv
bV <- (fv + la)
aBC <- h4*bgc
bBC <- lb

g1 <- function(x) 1 - exp(-0.5*x)

f <- function(now) {
  dV <- aV - bV*now[1]
  dBC <- aBC - bBC*now[2] - fc*g1(now[2]/now[1])*now[2]
  c(dV,dBC)
}

find_equib <- function (init) {
  list(init, 
       nlm(function(now) sum(f(now)**2), init)$estimate)
}
equibs <- map(map(1:10, function(x) rnorm(2, sd = 10)**2),
              find_equib)

equibs
equib <- equibs[[1]][[2]]
equib

-(fv + la)
-fc/2*((equib[1]/equib[2])**2)*exp(equib[1]/equib[2])

euler_auton <- function(diff, x0,
                        n, h, improv = T,
                        names) {
  xs <- matrix(nrow = n, ncol = 2)
  xs[1,] <- x0
  for (i in 2:n) {
    xstar <- xs[i-1,] + diff(xs[i-1,])*h
    if (improv) {
      xs[i,] <- xs[i-1,] + (diff(xs[i-1,]) + diff(xstar))*h/2
    }
    else {   
      xs[i,] <- xstar
    }
  }
  colnames(xs) <- names
  data.frame(xs)
}

sim1 <- euler_auton(f, c(0.2, 0.4), 
                    1500, 1/24, T, c('V', 'BC'))
sim2 <- euler_auton(f, c(2, 0.1), 
                    1500, 1/24, T, c('V', 'BC'))
grid.arrange(grobs = list(sim1 %>% 
                            ggplot(aes(V, BC)) + 
                            geom_path() +
                            geom_point(aes(x = equib[1], y = equib[2]), color='green', size=3),
                          sim2 %>% 
                            ggplot(aes(V, BC)) + 
                            geom_path() +
                            geom_point(aes(x = equib[1], y = equib[2]), color='green', size=3)),
             nrow=1)




