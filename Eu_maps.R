setwd("~/1_umapr-master/data_for_maps")

# clear memory---------------------------------------------------------------------------------------------
gc()
rm(list=ls())

# libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(rgdal)
library(lemon)

# Data call
data <- read.csv("Eu.csv", header = TRUE, sep = ",")
head(data)
summary(data)

data <- data[!(data$LU1_Desc=="Artificial land" | data$LU1_Desc=="Bareland" | 
                 data$LU1_Desc=="Shrubland" | data$LU1_Desc=="Water" | data$LU1_Desc=="Wetlands"),]
head(data)
str(data)

# Land cover plus coordinate data columns
b <- data[5:7]
head(b)

# Min-Max Normalization for a common field plain for the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data <- as.data.frame(lapply(data[1:4], normalize))
head(data)

# New data
data <- cbind(data, b)
head(data)
str(data)

# Call polygon shapefile
shp = readOGR(dsn = ".", layer = "Europe")
plot(shp)
df = fortify(shp)
#View(df)

# Combined map plots CZ
a <- ggplot() +
  geom_polygon(data = df, aes(x = long, y = lat, group = group), fill = "whitesmoke", color = "black", lwd = 1) +
  geom_point(data = data, aes(x = long, y = lat, color = OC), size = 0.1) +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) +
  coord_quickmap() + theme_void() + 
  theme(legend.text = element_text(size=rel(1.2)))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("OC") +
  theme(plot.title = element_text(hjust = 0.5))
a

b <- ggplot() +
  geom_polygon(data = df, aes(x = long, y = lat, group = group), fill = "whitesmoke", color = "black", lwd = 1) +
  geom_point(data = data, aes(x = long, y = lat, color = P), size = 0.1) +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) +
  coord_quickmap() + theme_void() + 
  theme(legend.text = element_text(size=rel(1.2)))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("P") +
  theme(plot.title = element_text(hjust = 0.5))
b

c <- ggplot() +
  geom_polygon(data = df, aes(x = long, y = lat, group = group), fill = "whitesmoke", color = "black", lwd = 1) +
  geom_point(data = data, aes(x = long, y = lat, color = N), size = 0.1) +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) +
  coord_quickmap() + theme_void() + 
  theme(legend.text = element_text(size=rel(1.2)))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("N") +
  theme(plot.title = element_text(hjust = 0.5))
c

d <- ggplot() +
  geom_polygon(data = df, aes(x = long, y = lat, group = group), fill = "whitesmoke", color = "black", lwd = 1) +
  geom_point(data = data, aes(x = long, y = lat, color = K), size = 0.1) +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) +
  coord_quickmap() + theme_void() + 
  theme(legend.text = element_text(size=rel(1.2)))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("K") +
  theme(plot.title = element_text(hjust = 0.5))
d

# Combine plots
eu_combined_plot <- grid_arrange_shared_legend(a, b, c, d, ncol = 4, position = c("right"))

ggsave(plot = eu_combined_plot,
       filename = "eu_combined_plot.tiff",
       device = "tiff", 
       path = "./",
       width = 16, 
       height = 4,  
       units = "in",
       dpi = 320)



  
