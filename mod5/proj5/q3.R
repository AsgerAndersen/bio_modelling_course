setwd('~/Desktop/bio_modelling_course/mod5/proj5/')

library(tidyverse)
library(xtable)

a <- 2
b <- 0.001
c <- 0.1
d <- 0.01
e <- 0.2
f <- 0.7

Geq <- function(alpha, beta) {
  
  b_temp <- a*(1-b/beta)
  dis <- b_temp**2 + 4*a*alpha
  
  c(0, 
    1/alpha, 
    1/d,
    (b_temp + sqrt(dis))/(2*a*alpha),
    (b_temp - sqrt(dis))/(2*a*alpha))
}

Feq <- function(alpha, beta) {
  c(0, 
    0, 
    (d-alpha)/(d*b),
    1/beta,
    1/beta)
}

Ueq <- function(G4, G5) {
  c(0,
    0,
    0,
    (d*G4 - 1)/e,
    (d*G5 - 1)/e)
}

find_equibs <- function(alpha, beta) {
  Geqs <- Geq(alpha, beta)
  Feqs <- Feq(alpha, beta)
  Ueqs <- Ueq(Geqs[4], Geqs[5])
  eqs <- matrix(c(Geqs, Feqs, Ueqs),
                nrow = 5)
  colnames(eqs) <- c('G', 'F', 'U')
  rownames(eqs) <- c('L1', 'L2', 'L3', 'L4', 'L5')
  eqs
}

find_equibs(0.002, 0.005)
find_equibs(0.002, 0.0008)
find_equibs(0.02, 0.0008)
print(xtable(find_equibs(0.002, 0.005)), file='eq_t1.tex')
print(xtable(find_equibs(0.002, 0.0008)), file='eq_t2.tex')
print(xtable(find_equibs(0.02, 0.0008)), file='eq_t3.tex')

dF <- function(Geq,Feq,Ueq, alpha,beta) {
  matrix(c(a*(1-2*alpha*Geq-b*Feq),
           c*d*Feq,
           0,
           -a*b*Geq,
           c*(d*Geq-1-e*Ueq),
           -c*e*Feq,
           0,
           f*beta*Ueq,
           f*(beta*Feq - 1)),
         nrow=3
         )
}

L4 <- find_equibs(0.002, 0.005)[4,]
eigen(dF(L4[1], L4[2], L4[3], 0.002, 0.005))$values


