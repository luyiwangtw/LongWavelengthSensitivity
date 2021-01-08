library(pavo)
library(ggplot2)
library(tidyr)
library(dplyr)

specsensbuprest.model2 <- read.csv("data/data for plot/filter shift_aim2_for plot.csv",header=TRUE) %>% as.rspec()
raw.dataset<-read.csv("data/refelectance spectra.csv",header=TRUE) %>% as.rspec()
dataset<- aggspec(raw.dataset, by = 3, FUN = mean) %>%  #average three measurements to a representative one
  procspec(opt = "smooth", span = 0.1, fixneg = "zero") #smooth the spectra and lift <0 to 0


#assign categoryies to each spectrum
category.list <- c("flower","leaf","beetle")
category <- rep(category.list , c(47*501,46*501,37*501))
wavelength<-rep(dataset[,1])

#transform the data for ploting purpose
dataset.transpose<-gather(dataset[,2:131], key = "species", value = "reflectance", na.rm = FALSE,
                          convert = FALSE, factor_key = FALSE)
dataset.plot.indivisually<-cbind(dataset.transpose, wavelength,category)

#split data by categories for future use
flower.spec<-dataset.plot.indivisually[1:23547,]
leaf.spec<-dataset.plot.indivisually[23548:46593,]
beetle.spec<-dataset.plot.indivisually[46594:65130,]

#merge the spectra before plot here
dataset.plot.indivisually$category2<-factor(dataset.plot.indivisually$category, levels=c("leaf","flower","beetle"))

#plot the spectra of the targets used in this model
wl<-specsensbuprest.model2[,1]
peaks<-gather(specsensbuprest.model2[,2:9], peak, value) %>% cbind(wl)

sens <- peaks %>% rename(
  "species" = "peak",
  "wavelength" = "wl"
)
sens$reflectance <- (sens$value)*100
sens$category2 <- sens$species

#set for facet
dataset.plot.indivisually$category3 <- dataset.plot.indivisually$category2


ggplot(dataset.plot.indivisually,aes(x=wavelength,y=reflectance, group=species, colour=category2))+
  geom_line(size=0.3, alpha=0.6)+
  geom_line(data=sens)+
  scale_color_manual( values = c( "cornflowerblue" , "palevioletred1","forestgreen","orange1", "orange3","darkorange3", "orangered1", "red2","olivedrab4","dodgerblue3","darkorchid4"))+
  facet_grid(category3 ~ .)+
  xlab("Wavelength (nm)")+ylab("Reflectance/Relative spectral sensitivity (%)")+
  scale_fill_discrete(name="sample type")+
  theme(legend.position = "none")+
  theme_classic()
