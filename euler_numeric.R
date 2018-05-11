library(tidyverse)
euler <- function(diff,x0,y0,n,h) {
  xs <- seq(x0, by = h, length.out = (n+1))
  ys <- rep(NA, (n+1))
  ys[1] <- y0
  for (i in 2:(n+1)) {
    ys[i] <- ys[i-1] + diff(xs[i-1], ys[i-1])*h
  }
  data_frame(x=xs, y=ys)
}

euler(function(x,y) y, 
      0, 0.01, 20, 0.05) %>% 
  ggplot(aes(x,y)) +
  geom_line()

