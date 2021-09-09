# Plot selected beetle and plant spectra in figure 1

## Load libraries
library(pavo)
library(dplyr)
library(ggplot2)

## Plot beetle spectra
beetle.spec.plot.dat <- read.csv("data/selected beetle spec for plotting.csv", header=TRUE) %>%
  as.rspec() %>%
  procspec(opt = "smooth") 

# Set up spp category for aggregating spectra when plotting with aggplot()
beetle.spp <- gsub("_Splice17_00\\d", "", names(beetle.spec.plot.dat))[-1]
table(beetle.spp)

beelte.spec.plot <- aggplot(beetle.spec.plot.dat, beetle.spp,
                            FUN.error = function(x) quantile(x, c(0.0275, 0.975)),
                            ylim = c(0,60),
                            alpha = 0.3, legend = TRUE
)


## Plot plant spectra
plant.spec.plot.dat <- read.csv("data/selected plant spec for plotting.csv",header=TRUE) %>%
  as.rspec() %>% 
  procspec(opt = "smooth") 

plant.spp <- gsub("_[0-9]", "", names(plant.spec.plot.dat))[-1]
table(plant.spp)

plant.spec.plot <- aggplot(plant.spec.plot.dat, plant.spp,
                           FUN.error = function(x) quantile(x, c(0.0275, 0.975)),
                           ylim = c(0,100),
                           alpha = 0.3, legend = TRUE
)
