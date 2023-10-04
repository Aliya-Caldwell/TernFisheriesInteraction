
#### herring size distribution 2021 ####

rm(list=ls())
setwd("C:/Users/aec1075/Box/Furey Lab Shared Materials/TERNS")
setwd("/Users/aliyacaldwell/Box/Furey Lab Shared Materials/TERNS")

library(ggmap)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(libwgeom)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggsn)
library(lubridate)
library(palettetown)
library(tidyr)
library(tidyverse)
library(moveVis)
library(move)
library(raster)
library(lubridate)

fishsize<-read.csv("FishMeasurementData.csv")

herrsize<-subset(fishsize, Species == "Atlantic Herring")

ggplot(herrsize,aes(x=TL.mm., fill=DeploymentID))+
  geom_density()
  
  