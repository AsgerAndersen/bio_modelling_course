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

euler <- function(diff, 
                  x0, y0, x_end, 
                  h, improv = F) {
  xs <- seq(x0, x_end + x_end%%h, by = h)
  ys <- rep(NA, length(xs))
  ys[1] <- y0
  for (i in 2:length(xs)) {
    ystar <- ys[i-1] + diff(xs[i-1], ys[i-1])*h
    if (improv) {
      ys[i] <- ys[i-1] + (diff(xs[i-1], ys[i-1]) + diff(xs[i], ystar))*h/2
    }
    else {
      ys[i] <- ystar
    }
  }
  data_frame(t=xs, x_euler=ys)
}

approx <- function(h, t_end = 10, improv = F) {
  euler(function(t, x) r0*x - u0 - alpha*t,
        0, 1700000, t_end, 
        h, improv) %>% 
    mutate(x_true = map_dbl(t, function(t) x(t,1700000)),
           error = x_euler - x_true,
           error_percent = error / x_true)
}

approx_plot <- function(h, t_end, improv, min_t) {
  if (improv) {
    name <- 'Improved Euler'
  }
  else {
    name <- 'Ordinary Euler'
  }
  approx(h, t_end, improv) %>% 
    filter(min_t<t) %>% 
    select(-error, -error_percent) %>% 
    gather(key='key', value='value', -t) %>% 
    ggplot(aes(t, value, color=key)) +
    geom_line() + 
    theme(legend.position = 'none',
          axis.text.y = element_text(angle=60),
          axis.title = element_blank()) +
    ggtitle(str_c(name, ';  h = ', h, ';  t = ', min_t, ',...,', t_end))
}

error_plot <- function(h, t_end, improv, min_t, error_type) {
  if (improv) {
    name <- 'Improved Euler'
  }
  else {
    name <- 'Ordinary Euler'
  }
  approx(h, t_end, improv) %>% 
    filter(min_t<t) %>% 
    ggplot(aes_string('t', error_type)) +
    geom_line() + 
    theme(axis.text.y = element_text(angle=60),
          axis.title = element_blank()) +
    ggtitle(str_c(name, ';  h = ', h, ';  t = ', min_t, ',...,', t_end))
}

grid.arrange(grobs = list(approx_plot(1, 10, F, 5), 
                          approx_plot(1, 50, F, 30),
                          approx_plot(1, 10, T, 5), 
                          approx_plot(1, 50, T, 30)),
             nrow=2)

grid.arrange(grobs = list(error_plot(1, 10, F, 5, 'error'), 
                          error_plot(1, 50, F, 30, 'error'),
                          error_plot(1, 10, T, 5, 'error'), 
                          error_plot(1, 50, T, 30, 'error')),
             nrow=2)

grid.arrange(grobs = list(error_plot(1, 10, F, 5, 'error_percent'), 
                          error_plot(1, 50, F, 30, 'error_percent'),
                          error_plot(1, 10, T, 5, 'error_percent'), 
                          error_plot(1, 50, T, 30, 'error_percent')),
             nrow=2)

grid.arrange(grobs = list(error_plot(0.01, 10, F, 5, 'error'), 
                          error_plot(0.5, 10, T, 5, 'error')),
             nrow=1)

h_threshold_ord <- uniroot(function(h) approx(h) %>% .$error %>% tail(1) + 100, c(0.01, 1))$root
h_threshold_improv <- uniroot(function(h) approx(h,improv=T) %>% .$error %>% tail(1) + 100, c(0.5, 1))$root
h_threshold_ord
h_threshold_improv
approx(h_threshold_ord, improv=F) %>% tail(1)
approx(h_threshold_improv, improv=T) %>% tail(1)

#############################################################################

r0 <- 0.08
u0 <- 80000
beta <- 0.06
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

