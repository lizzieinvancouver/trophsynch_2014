### Started 23 January 2014 ###
### By Lizzie & Heather ###

## Hopefully this will be useful plotting f(x)s for the trophic synchrony work ##
## Right now, just for observations ##

## Be sure to set the wd and add folders for:
# input, output, within output put 'graphs'#
# within graphs put two folders: doybyyr, and phenodiff ##

options(stringsAsFactors=FALSE)
library(ggplot2)

setwd("/Users/Lizzie/Documents/git/R/trophsynch")

dater <- read.csv("input/spp_phenodata.csv", header=TRUE)
daterdiff <- read.csv("input/int_phenodata.csv", header=TRUE)

# basic idea here (showing all data, it's slow!)
ggplot(data=dater, aes(year, phenovalue,
       colour=factor(species))) +
       geom_point(shape=1) + facet_wrap(~intid) +
       geom_smooth(method=lm, se=FALSE, 
       aes(fill = factor(species)))

# make a f(x), which I adapted from one I found online
# and use lapply
doPlot <- function(sel_name) {
   subby <- dater[dater$studyid == sel_name,]
   ggobj <- ggplot(data=subby, aes(year, phenovalue,
       colour=factor(species))) +
       geom_point(shape=1) + facet_wrap(~intid) +
       geom_smooth(method=lm, se=FALSE, 
       aes(fill = factor(species)))
   print(ggobj)
   ggsave(sprintf("graphs/doybyyr/%s.pdf", sel_name))
}
   
lapply(unique(dater$studyid), doPlot)


doPlotphenodiff <- function(sel_name) {
   subby <- daterdiff[daterdiff$studyid == sel_name,]
   ggobj <- ggplot(data=subby, aes(year, phenodiff,
       colour=factor(intid))) +  geom_point(shape=1) +
       geom_smooth(method=lm, se=FALSE, 
       aes(fill = factor(intid)))
   print(ggobj)
   ggsave(sprintf("graphs/phenodiff/%s.pdf", sel_name))
}
   
lapply(unique(daterdiff$studyid), doPlotphenodiff)
