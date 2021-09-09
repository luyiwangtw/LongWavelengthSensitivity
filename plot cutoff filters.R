#plot cuttoff filters
library(dplyr)
library(pavo)
library(ggplot2)
library(tidyr)

raw.cutoff <- read.csv("data/cutoffs.csv",header=TRUE) %>% 
  as.rspec(lim = c(300,800)) %>% 
  procspec(opt = c("min", "max"))
cutoff <- gather(raw.cutoff, peak, value, -wl)

ggplot(cutoff,aes(x=wl, y=value, col=peak))+
  geom_line()+
  theme(legend.justification=c(0,1),
        legend.position = c(.05, .95),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")
        )+
  scale_color_manual(
    values = c( "orange1", "orange3","darkorange3", "orangered1", "red2"),
    labels=c("580 nm", "600 nm", "620 nm", "640 nm", "660 nm" ))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  xlab("Wavelength (nm)")+ 
  ylab("Normalised transmittance")
  
