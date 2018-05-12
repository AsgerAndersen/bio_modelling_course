library(tidyverse)
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


