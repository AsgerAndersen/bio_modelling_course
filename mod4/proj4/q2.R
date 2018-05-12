b1 <- 3
b2 <- -2
x <- function(t) {
  b1*exp(-2*t)*cos(2*t) +
    b2*exp(-2*t)*sin(2*t)
}
y <- function(t) {
  b1*exp(-2*t)*2*sin(2*t) +
    b2*exp(-2*t)*-2*cos(2*t)
}
sim <- data_frame(t = seq(0,pi,by=0.05),
                  x = x(t),
                  y = y(t))
sim %>% ggplot(aes(x,y)) + geom_path()
sim %>% 
  gather(key = 'key', value = 'value', -t) %>% 
  ggplot(aes(t, value, color=key)) +
  geom_line() +
  theme(legend.title = element_blank())

solve(matrix(c(-4,-4,1,-4),nrow = 2), c(1,-2))
