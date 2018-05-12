library(tidyverse)
kg <- 2.2
kd <- 0.15
yg <- 0.8
p0 <- 10
equib <- c(p0*yg/kd*(1-yg), p0/kg*(1-yg))
A <- matrix(c(-kg,yg*kg,kd,-kd),nrow=2)
l1 <- eigen(A)$values[1]
l2 <- eigen(A)$values[2]
q1 <- eigen(A)$vectors[1,]
q2 <- eigen(A)$vectors[2,]
l1
l2
q1
q2
cs <- solve(matrix(c(q1,q2),nrow=2), c(2,10)-equib)
x <- function(t) cs[1]*exp(l1*t)*q1[1] + cs[2]*exp(l2*t)*q2[1] + equib[1]
y <- function(t) cs[1]*exp(l1*t)*q1[2] + cs[2]*exp(l2*t)*q2[2] + equib[2]
sim <- data_frame(t = seq(0,220,by=0.05),
                  WS = x(t),
                  WG = y(t))
sim %>% 
  gather(key = 'key', value = 'value', -t) %>% 
  ggplot(aes(t, value, color=key)) +
  geom_line() +
  geom_hline(yintercept = equib[1], color='green', linetype='dashed') +
  geom_hline(yintercept = equib[2], color='green', linetype='dashed') +
  theme(legend.title = element_blank())
