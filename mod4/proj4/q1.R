setwd('~/Desktop/bio_modelling_course/mod4/proj4/')

library(magrittr)
library(tidyverse)

data <- read_delim('anaestesi-data.txt',' ')

#f
fit <- nls(Koncentration~A1*exp(l1*Tid)+A2*exp(l2*Tid), 
           data=data,
           start=list(A1=1, A2=1, l1=-1, l2=-2))
pars <- fit$m$getPars()
Kb <- function(t) {
  unname(pars['A1']*exp(pars['l1']*t) + pars['A2']*exp(pars['l2']*t))
}
ggplot(data, aes(Tid, Koncentration)) +
  geom_point() +
  stat_function(fun=Kb, color='blue')

#g
Kb0 <- 0.846
a2_1 <- pars['A1']*(pars['l1']-pars['l2'])/Kb0 - pars['l1']
a2_2 <- pars['A1']*(pars['l2']-pars['l1'])/Kb0 - pars['l2']
a2 <- mean(c(a2_1,a2_2))
a2
a3 <- unname(pars['l1']*pars['l2']/a2)
a3
a1 <- unname(-pars['l1']-pars['l2']-a2-a3)
a1

#h
c1 <- unname(Kb0/(pars['l1']-pars['l2']))
c2 <- unname(Kb0/(pars['l2']-pars['l1']))
q1 <- unname(c(a2 + pars['l1'], a1))
q2 <- unname(c(a2 + pars['l2'], a1))
model <- function(t) unname(exp(pars['l1']*t)*c1*q1 + exp(pars['l2']*t)*c2*q2)
ts <- seq(0,5,0.01)
sim <- data.frame(t(as.matrix(data.frame(map(ts, model)))))
rownames(sim) <- NULL
colnames(sim) <- c('Blod', 'Væv')
sim %<>% mutate(t = ts) 
sim %>% 
  gather(key='key', value='value', -t) %>% 
  ggplot(aes(t, value, color=key)) +
  geom_line() +
  geom_hline(yintercept = 0.25, color='green', linetype='dashed') +
  theme(legend.title = element_blank())

uniroot(function(t) model(t)[2] - 0.25, c(0,0.75))$root
uniroot(function(t) model(t)[2] - 0.25, c(0.75,1.5))$root

#
d0 <- a3/a2*0.275
equib <- c(d0*a1/a3,d0*a2/a3)
b <- c(Kb0,0)-equib
A <- matrix(c(q1,q2),nrow = 2)
cs <- solve(A, b)
cs
model <- function(t) unname(exp(pars['l1']*t)*cs[1]*q1 + exp(pars['l2']*t)*cs[2]*q2 + equib)
ts <- seq(0,5,0.01)
sim <- data.frame(t(as.matrix(data.frame(map(ts, model)))))
rownames(sim) <- NULL
colnames(sim) <- c('Blod', 'Væv')
sim %<>% mutate(t = ts) 
sim %>% 
  gather(key='key', value='value', -t) %>% 
  ggplot(aes(t, value, color=key)) +
  geom_line() +
  geom_hline(yintercept = 0.25, color='green', linetype='dashed') +
  theme(legend.title = element_blank())

uniroot(function(t) model(t)[2] - 0.25, c(0,0.75))$root


