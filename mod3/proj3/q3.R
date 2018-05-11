library(magrittr)
library(tidyverse)
library(gridExtra)

r0 <- 0.08
u0 <- 80000
alpha <- 4000

xmin <- (r0*u0 + alpha)/r0**2

h <- Vectorize(function(t) {
  xmin + (alpha/r0)*t
})

x <- function(t, x0) {
  h(t) + (x0 - h(0))*exp(r0*t)
}
ts <- seq(0,100,by=0.2)
x0s <- xmin*seq(0.95,1.05,by=0.01)
xs <- map(map(x0s, function(x0) {function(t) x(t, x0)}),
    function(f) {f(ts)})
xs <- data.frame(xs)
colnames(xs) <- str_c('x', as.character(1:11))
plot_lines <- function(max_t) {
  xs %>% 
    mutate(t = ts) %>% 
    filter(t <= max_t) %>% 
    gather(key='key', value='value', -t) %>% 
    ggplot(aes(t, value, color=key)) +
    geom_line() + 
    theme(legend.position = 'none',
          axis.text.y = element_text(angle = 60),
          axis.title = element_blank())  
}
grid.arrange(grobs=map(c(10,30,50,100), plot_lines), nrow=2)

#############################################################################

r0 <- 0.08
u0 <- 80000
beta <- 0.1

xmin <- u0/(r0-beta)

h <- Vectorize(function(t) {
  xmin*exp(beta*t)
})

x <- function(t, x0) {
  h(t) + (x0 - h(0))*exp(r0*t)
}
ts <- seq(0,500,by=0.2)
x0s <- xmin*seq(0.95,1.05,by=0.01)
xs <- map(map(x0s, function(x0) {function(t) x(t, x0)}),
          function(f) {f(ts)})
xs <- data.frame(xs)
colnames(xs) <- str_c('x', as.character(1:11))
plot_lines <- function(tlims) {
  xs %>% 
    mutate(t = ts) %>% 
    filter(tlims[1] <= t , t <= tlims[2]) %>% 
    gather(key='key', value='value', -t) %>% 
    ggplot(aes(t, value, color=key)) +
    geom_line() + 
    theme(legend.position = 'none',
          axis.text.y = element_text(angle = 60),
          axis.title = element_blank())  
}
grid.arrange(grobs=map(list(c(0,5),c(0,10),c(0,25), c(0,50)), plot_lines), nrow=2)

  
  