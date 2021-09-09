# Plot normalised sensitivity spectra

## Load libraries
library(ggplot2)
library(dplyr)
library(pavo)
library(tidyr)
library(tibble)
library(stringr)

## Load data
spec.filter <- read.csv("data/peak sensitivity_filter shift_aim2.csv",header=TRUE) %>%
  as.rspec()
spec.opsin <- read.csv("data/peak sensitivity_opsin shift_aim2.csv",header=TRUE) %>%
  as.rspec()
spec.even <- read.csv("data/peak sensitivity_evenly space.csv",header=TRUE) %>%
  as.rspec()


## Filter-shifted sensitivity spectra

filter.list <- list(spec.filter[, 1:2], spec.filter[, c(1, 3)], spec.filter[, c(1, 4)], spec.filter[, c(1, 5)], 
             spec.filter[, c(1, 6)], spec.filter[, c(1, 7)], spec.filter[, c(1, 8)], spec.filter[, c(1, 9)])

hold.filter <- tibble("355 nm" = NA, "445 nm" = NA, "530 nm" = NA, "580 nm" = NA, 
               "600 nm" = NA, "620 nm" = NA, "640 nm" = NA, "660 nm" = NA, wl = spec.filter[,1])

for(i in 1:length(filter.list)){
  
  temp <- filter.list[[i]] %>% procspec(opt = c("min", "max"))
  hold.filter[[i]] <- temp[, 2]
}

plotdat.filter <- gather(hold.filter[,1:8], peak, value) %>% cbind(hold.filter[,"wl"])

ggplot(plotdat.filter, aes(x = wl, y = value, color = peak))+
  geom_line()+
  guides(color = guide_legend(title = "peak sensitivity"))+
  scale_color_manual(
    values = c("darkorchid4", "dodgerblue3", "olivedrab4", "orange1", "orange3","darkorange3", "orangered1", "red2"))+
  xlab("Wavelength (nm)")+ 
  ylab("Relative spectral sensitivity")+
  theme_classic()

## Opsin shift sensitivity spectra

opsin.list <- list(spec.opsin[, 1:2], spec.opsin[, c(1, 3)], spec.opsin[, c(1, 4)], spec.opsin[, c(1, 5)], 
                    spec.opsin[, c(1, 6)], spec.opsin[, c(1, 7)], spec.opsin[, c(1, 8)], spec.opsin[, c(1, 9)])

hold.opsin <- tibble("355 nm" = NA, "445 nm" = NA, "530 nm" = NA, "580 nm" = NA, 
                      "600 nm" = NA, "620 nm" = NA, "640 nm" = NA, "660 nm" = NA, wl = spec.opsin[,1])

for(i in 1:length(opsin.list)){
  
  temp <- opsin.list[[i]] %>% procspec(opt = c("min", "max"))
  hold.opsin[[i]] <- temp[, 2]
}

plotdat.opsin <- gather(hold.opsin[,1:8], peak, value) %>% cbind(hold.opsin[,"wl"])

ggplot(plotdat.opsin, aes(x = wl, y = value, color = peak))+
  geom_line()+
  guides(color = guide_legend(title = "peak sensitivity"))+
  scale_color_manual(
    values = c("darkorchid4", "dodgerblue3", "olivedrab4", "orange1", "orange3","darkorange3", "orangered1", "red2"))+
  xlab("Wavelength (nm)")+ 
  ylab("Relative spectral sensitivity")+
  theme_classic()


## Evenly spaced sensitivity spectra

even.list <- list(spec.even[, 1:2], spec.even[, c(1, 3)], spec.even[, c(1, 4)], spec.even[, c(1, 5)])

hold.even <- tibble("UVS" = NA, "SWS" = NA, "MWS" = NA, "LWS" = NA, wl = spec.even[,1])

for(i in 1:length(even.list)){
  
  temp <- even.list[[i]] %>% procspec(opt = c("min", "max"))
  hold.even[[i]] <- temp[, 2]
}

pre.plotdat.even <- gather(hold.even[,1:4], peak, value) %>% cbind(hold.even[,"wl"]) %>% 
  add_column(system.type = strrep("evenely space", 1)) 
pre.plotdat.uneven <- plotdat.filter %>% filter(peak %in% c("355 nm", "445 nm", "530 nm", "660 nm")) %>% 
  add_column(system.type = strrep("unevenely space", 1)) %>% 
  mutate(peak = str_replace(peak, "355 nm", "UVS")) %>% 
  mutate(peak = str_replace(peak, "445 nm", "SWS")) %>% 
  mutate(peak = str_replace(peak, "530 nm", "MWS")) %>% 
  mutate(peak = str_replace(peak, "660 nm", "LWS"))

plotdat.even <- pre.plotdat.even %>% rbind(pre.plotdat.uneven)

ggplot(plotdat.even,
       aes(x = wl, y = value, 
           linetype = system.type, color = peak))+
  geom_line()+
  guides(color = guide_legend(title = "peak sensitivity", reverse = TRUE),
         linetype = guide_legend(title = "system type"))+
  scale_color_manual(
    values = c("red4","olivedrab4", "dodgerblue3", "darkorchid4"))+
  xlab("Wavelength (nm)")+ 
  ylab("Relative spectral sensitivity")+
  theme_classic()
