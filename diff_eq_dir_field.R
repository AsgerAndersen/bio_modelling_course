library(magrittr)
library(tidyverse)

line_seg <- Vectorize(function(point, slope, len) {
  v1 <- len/sqrt((1+slope**2))
  v2 <- slope*v1
  v <- c(v1, v2)
  c(-v/2, v/2) + point
})

dir_field <- function(diff, 
                      xlim = c(-2,2), 
                      ylim = c(-2,2), 
                      step_len = 0.2) {
  xs <- seq(xlim[1], xlim[2], step_len)
  ys <- seq(ylim[1], ylim[2], step_len)
  ps <- map(cross2(xs, ys), as.double)
  seg_len <- step_len*0.5
  slopes <- diff(ps)
  segs <- line_seg(ps, slopes, rep(seg_len, length(ps)))
  segs
  data_frame(x1 = segs[1,],
             y1 = segs[2,],
             x2 = segs[3,],
             y2 = segs[4,]) %>%
    ggplot() +
    geom_segment(aes(x = x1,
                     y = y1,
                     xend = x2,
                     yend = y2)) +
    coord_fixed() + xlab('t') + ylab('x')
}

#Test diff equation y' = y
dir_field(Vectorize(function(p) {p[2]}))

#Test diff equation y' = x
dir_field(Vectorize(function(p) {p[1]}))

#Test diff equation y' = x + y
dir_field(Vectorize(function(p) {p[1] + p[2]}))

#Test diff equation y' = x - y
dir_field(Vectorize(function(p) {p[1] - p[2]}))

#Test diff equation y' = x*y
dir_field(Vectorize(function(p) {p[1]*p[2]}))

#Test diff equation y' = x/y
dir_field(Vectorize(function(p) {p[1]/p[2]}))
