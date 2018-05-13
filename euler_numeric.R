library(tidyverse)

#Put onedim into euler_auton, 
#which should now then output time,
#also if time has no effect on the 
#dynamics
euler_onedim <- function(diff, 
                         t0, x0, t_end, 
                         h, improv = T) {
  ts <- seq(t0, t_end + t_end%%h, by = h)
  xs <- rep(NA, length(ts))
  xs[1] <- x0
  for (i in 2:length(ts)) {
    xstar <- xs[i-1] + diff(ts[i-1], xs[i-1])*h
    if (improv) {
      xs[i] <- xs[i-1] + (diff(ts[i-1], xs[i-1]) + diff(ts[i], xstar))*h/2
    }
    else {
      xs[i] <- xstar
    }
  }
  data_frame(t=ts, x_euler=xs)
}

euler_auton <- function(diff, x0,
                        n, h, improv = T,
                        names) {
  xs <- matrix(nrow = n, ncol = length(x0))
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
  #xs$t <- seq(0, by = h, length.out = n)
  xs
}
