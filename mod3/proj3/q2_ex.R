library(magrittr)
library(tidyverse)
library(gridExtra)

setwd('~/Desktop/Matematik og modeller/mod3/proj3/')

sars<-read_table('SARS.txt') %>% 
  mutate_all(funs(as.integer))
colnames(sars) <- c('t', 'N')

plt <- sars %>% 
  ggplot(aes(t, N)) +
  geom_line()
plt

plt2 <- plt + 
  geom_hline(yintercept = 208, 
             color='green',
             linetype='dashed') +
  xlim(0, 75)
plt2  

make_logit <- function(r) {
  list(Vectorize(function(t) 208/(1 + 207*exp(-r*t))), r)
}

find_r_plots <- function(model_builder, search_spaces) {
  map(search_spaces,
      function (space) {
        grid.arrange(grobs=map(map(space, model_builder),
                           function(l) {
                             plt2 + 
                               stat_function(fun=l[[1]], color='blue') +
                               ggtitle(str_c('r = ', l[[2]])) +
                               theme(axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     axis.title = element_blank())
                           }),
                 nrow=2)
      }
  )
}

find_r_plots(make_logit, list(seq(0.12,0.22,by=0.02), seq(0.14,0.19,by=0.01)))

S <- function(model) {
  sars %>% 
    mutate(pred = model(t),
           res_sqrd = (N-pred)**2) %>% 
    summarise(S = sum(res_sqrd)) %>% 
    .$S
}

S(make_logit(0.17)[[1]])

eval_mods <- function(rs, model_builder) {
  data_frame(r=rs) %>% 
    mutate(S = map_dbl(map(rs, function(r) model_builder(r)[[1]]), S)) %>% 
    ggplot(aes(r,S)) +
    geom_point() + 
    scale_x_continuous(breaks=rs) 
}

eval_mods(seq(.12,.22,by=.01), make_logit)

make_mod_logit <- function(r) {
  list(Vectorize(function(t) 208/((1 + (208**5 - 1)*exp(-5*r*t)))**(1/5)), r)
}

find_r_plots(make_mod_logit, list(seq(0.12,0.22,by=0.02), seq(0.12,0.17,by=0.01)))

eval_mods(seq(.12,.22,by=.01), make_mod_logit)

S(make_mod_logit(0.14)[[1]])

make_mod_logit_2 <- function(r) {
  list(Vectorize(function(t) 208/((1 + 1.908*exp(-0.2*r*t)))**5), r)
}

find_r_plots(make_mod_logit_2, list(seq(0.38,0.48,by=0.02), seq(0.40,0.45,by=0.01)))

eval_mods(seq(0.38,0.48,by=0.01), make_mod_logit_2)

S(make_mod_logit_2(0.43)[[1]])

N1 <- function(t) 105/((1 + 10182*exp(-0.415*t)))**(0.504)
N2 <- function(t) 206/((1 + 146757*exp(-0.243*t)))**(0.154)

last_mod <- Vectorize(function(t) {
  if (t <= 31) N1(t)
  else N2(t)
})

plt2 + 
  stat_function(fun=last_mod, color='blue') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

c(N1(31), N2(31))

S(last_mod)


