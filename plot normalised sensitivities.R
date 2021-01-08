#plot normalised sensitivities

#1) filter-shifted 
library(ggplot2)
specsensbuprest.aim2 <- read.csv("data/data for plot/filter shift_aim2_for plot.csv",header=TRUE) %>%
  as.rspec()
wl<-specsensbuprest.aim2[,1]
peaks<-gather(specsensbuprest.aim2[,2:9], peak, value) %>% cbind(wl)

peak.order <- c("UVS.355.A1.", "SWS.445.A1.", "MWS.530.A1.", "LWS.570.A1..filter580.", "LWS.570.A1..filter600.", "LWS.570.A1..filter620.", "LWS.570.A1..filter640.", "LWS.570.A1..filter660.") #to order the peaks in the legend
ggplot(peaks,aes(x=wl, y=value, col=peak))+
  geom_line()+
  guides(color=guide_legend(title="peak sensitivity"))+
  scale_color_manual(
    values = c("darkorchid4", "dodgerblue3", "olivedrab4", "orange1", "orange3","darkorange3", "orangered1", "red2"),
    labels=c("355 nm", "445 nm","530 nm", "580 nm", "600 nm", "620 nm", "640 nm", "660 nm" ),
    breaks=peak.order)+
  xlab("Wavelength (nm)")+ 
  ylab("Relative spectral sensitivity")+
  theme_classic()

#2)opsin-shifted
specsensbuprest.aim2 <- read.csv("data/data for plot/opsin shift_aim2_for plot.csv",header=TRUE) %>%
  as.rspec()
wl<-specsensbuprest.aim2[,1]
peaks<-gather(specsensbuprest.aim2[,2:9], peak, value) %>% cbind(wl)

peak.order <- c("UVS.355.A1.", "SWS.445.A1.", "MWS.530.A1.", "LWS.580.A1.", "LWS.600.A1.", "LWS.620.A1.", "LWS.640.A1.", "LWS.660.A1.") #to order the peaks in the legend
ggplot(peaks,aes(x=wl, y=value, col=peak))+
  geom_line()+
  guides(color=guide_legend(title="peak sensitivity"))+
  scale_color_manual(
    values = c("darkorchid4", "dodgerblue3", "olivedrab4", "orange1", "orange3","darkorange3", "orangered1", "red2"),
    labels=c("355 nm", "445 nm","530 nm", "580 nm", "600 nm", "620 nm", "640 nm", "660 nm" ),
    breaks=peak.order)+
  xlab("Wavelength (nm)")+ 
  ylab("Relative spectral sensitivity")+
  theme_classic()

#3)even spacing 
even.space <- read.csv("data/data for plot/filtered_even space_for plot.csv",header=TRUE) %>%
  as.rspec()
specsensbuprest.model2 <- read.csv("data/data for plot/filter shift_aim2_for plot.csv",header=TRUE) %>%
  as.rspec()
system.list <- c("even space","uneven space")
system.type <- rep(system.list , c(1*501,1*501))
visevencurve<-even.space %>%  rename( UVS = 2, SWS = 3, MWS = 4, LWS = 5)
VS660curve<-specsensbuprest.model2[,c(1,2,3,4,9)] %>%  rename( UVS = 2, SWS = 3, MWS = 4, LWS = 5)
combine.curves <- visevencurve %>%  rbind(VS660curve) %>% cbind(system.type) %>%
  gather( peak, value, -wl, -system.type) 

ggplot(combine.curves,aes(x=wl, y=value, linetype=system.type, color=peak))+
  geom_line()+
  guides(color=guide_legend(title="peak sensitivity", reverse=TRUE),
         linetype=guide_legend(title="system type"))+
  scale_color_manual(
    values = c("red4","olivedrab4", "dodgerblue3", "darkorchid4"),
    labels = c("LWS", "MWS", "SWS", "UVS"))+
  xlab("Wavelength (nm)")+ 
  ylab("Relative spectral sensitivity")+
  theme_classic()
