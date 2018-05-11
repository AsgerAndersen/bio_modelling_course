setwd('~/Desktop/Matematik og modeller/mod4/proj4/')

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

