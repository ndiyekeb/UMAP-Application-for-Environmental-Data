setwd("C:/Users/Ndiye/Desktop/umapr-master")

# clear memory---------------------------------------------------------------------------------------------
gc()
rm(list=ls())

# load packages--------------------------------------------------------------------------------------------
library(umapr)
library(tidyverse)
library(gridExtra)
library(ggprism)
library(lemon)

# load datasets--------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
# Czechia dataset------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

data1 <- read.csv('Czechia.csv', header = TRUE, sep = ',')
head(data1)
str(data1)
summary(data1)

data1 <- data1[!(data1$LU1_Desc=="Artificial land" | data1$LU1_Desc=="Bareland" | 
           data1$LU1_Desc=="Shrubland" | data1$LU1_Desc=="Water" | data1$LU1_Desc=="Wetlands"),]
head(data1)
str(data1)

# Land cover column
b1 <- data1[5]
head(b1)

# Min-Max Normalization for a common field plain for the data1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data1 <- as.data.frame(lapply(data1[1:4], normalize))
head(data1)

# New data1
data1 <- cbind(data1, b1)
head(data1)

#----------------------------------------------------------------------------------------------------------
# Europe dataset-------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------

data <- read.csv('Europe.csv', header = TRUE, sep = ',')
head(data)
str(data)
summary(data)

data <- data[!(data$LU1_Desc=="Artificial land" | data$LU1_Desc=="Bareland" | 
                 data$LU1_Desc=="Shrubland" | data$LU1_Desc=="Water" | data$LU1_Desc=="Wetlands"),]
head(data)
str(data)

# Land cover column
b <- data[5]
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

#----------------------------------------------------------------------------------------------------------
## Function parameters-------------------------------------------------------------------------------------
## There are a few important parameters. These are fully described in the UMAP Python documentation.

# The n_neighbor argument can range from 2 to n-1 where n is the number of rows in the datasets.
neighbors <- c(2, 4, 8, 16, 32, 64, 128, 256, 430)

set.seed(1234)
cz <- neighbors %>% 
  map_df(~umap(as.matrix(data1[,1:4]), n_neighbors = .x) %>% 
           mutate(LU1_Desc = data1$LU1_Desc, Neighbor = .x)) %>% 
  mutate(Neighbor = as.integer(Neighbor)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = LU1_Desc)) + 
  geom_point(size = 0.5) + 
  theme_prism() +
  facet_wrap(~ Neighbor, scales = "free") +
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2)))
cz

set.seed(1234)
eu <- neighbors %>% 
  map_df(~umap(as.matrix(data[,1:4]), n_neighbors = .x) %>% 
           mutate(LU1_Desc = data$LU1_Desc, Neighbor = .x)) %>% 
  mutate(Neighbor = as.integer(Neighbor)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = LU1_Desc)) + 
  geom_point(size = 0.05) + 
  theme_prism() +
  facet_wrap(~ Neighbor, scales = "free") +
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2)))
eu

# Combine plots
cz_eu <- grid_arrange_shared_legend(cz, eu, ncol = 2, position = c("bottom"))

ggsave(plot = cz_eu,
       filename = "cz_eu.tiff",
       device = "tiff", 
       path = "./",
       width = 16, 
       height = 9, 
       units = "in",
       dpi = 320)

# The min_dist argument can range from 0 to 1.-------------------------------------------------------------
dists <- c(0.001, 0.005, 0.010, 0.025, 0.050, 0.100, 0.500, 0.750, 0.990)

set.seed(1234)
cz1 <- dists %>% 
  map_df(~umap(as.matrix(data1[,1:4]), min_dist = .x) %>% 
           mutate(LU1_Desc = data1$LU1_Desc, Distance = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = LU1_Desc)) + 
  geom_point(size = 0.5) + 
  theme_prism() +
  facet_wrap(~ Distance, scales = "free") +
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2)))

cz1

set.seed(1234)
eu1 <- dists %>% 
  map_df(~umap(as.matrix(data[,1:4]), min_dist = .x) %>% 
           mutate(LU1_Desc = data$LU1_Desc, Distance = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = LU1_Desc)) + 
  geom_point(size = 0.05) + 
  theme_prism() +
  facet_wrap(~ Distance, scales = "free") +
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2)))

eu1

# Combine plots
cz1_eu1 <- grid_arrange_shared_legend(cz1, eu1, ncol = 2, position = c("bottom"))

ggsave(plot = cz1_eu1,
       filename = "cz1_eu1.tiff",
       device = "tiff", 
       path = "./",
       width = 16, 
       height = 9, 
       units = "in",
       dpi = 320)

# The distance argument can be many different distance functions.------------------------------------------
dists <- c("euclidean", "cosine", "hamming", "correlation", "minkowski", "chebyshev", "jaccard", "canberra", "dice")

set.seed(1234)
cz2 <- dists %>% 
  map_df(~umap(as.matrix(data1[,1:4]), metric = .x) %>% 
           mutate(LU1_Desc = data1$LU1_Desc, Metric = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = LU1_Desc)) + 
  geom_point(size = 0.5) +
  theme_prism() +
  facet_wrap(~ Metric, scales = "free") +
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2)))

cz2

set.seed(1234)
eu2 <- dists %>% 
  map_df(~umap(as.matrix(data[,1:4]), metric = .x) %>% 
           mutate(LU1_Desc = data$LU1_Desc, Metric = .x)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = LU1_Desc)) + 
  geom_point(size = 0.05) +
  theme_prism() +
  facet_wrap(~ Metric, scales = "free") +
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2)))

eu2

# Combine plots
cz2_eu2 <- grid_arrange_shared_legend(cz2, eu2, ncol = 2, position = c("bottom"))

ggsave(plot = cz2_eu2,
       filename = "cz2_eu2.tiff",
       device = "tiff", 
       path = "./",
       width = 16, 
       height = 9,  
       units = "in",
       dpi = 320)

# clear memory---------------------------------------------------------------------------------------------
gc()
rm(list=ls())
#----------------------------------------------------------------------------------------------------------




