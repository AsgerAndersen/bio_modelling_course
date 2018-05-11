library(ggplot2)
ggplot(data.frame(x=0), aes(x)) +
  stat_function(fun = function(x) {2.5/x}) +
  xlim(0.2,5) + xlab('lambda') + 
  ylab('S') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
