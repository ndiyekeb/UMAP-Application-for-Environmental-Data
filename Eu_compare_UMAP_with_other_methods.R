setwd("C:/Users/Ndiye/Desktop/umapr-master")

gc()
rm(list=ls())

library(umapr)
library(Rtsne)
library(tidyverse)
library(bench)
library(ggplot2)
library(ggthemes)
library(ggprism)
library(gridExtra)
library(ggpubr)

# stuff to compare algorithms -------------------------------------------------
set.seed(1234)
embed <- function(labels, d) {
  times <- mark(
    um <- umap(d, min_dist = 0.1, n_neighbors = 16, metric = "cosine"),
    ts <- Rtsne(d)$Y,
    ts_no_pca <- Rtsne(d, pca = FALSE)$Y,
    check = FALSE)
  
  pca <- prcomp(d)$x[,1:2]
  
  times$expression <- c("UMAP", "PCA + t-SNE", "t-SNE")
  
  combo <- function(embedding, name) {
    colnames(embedding) <- c("V1", "V2")
    embedding %>%
      as.data.frame() %>%
      mutate(Algorithm = name, Class = labels)
  }
  
  list(times = times,
       results = bind_rows(
         combo(pca, "PCA"),
         mutate(um, Algorithm = "UMAP", Class = labels, V1 = UMAP1, V2 = UMAP2),
         combo(ts, "PCA + t-SNE"),
         combo(ts_no_pca, "t-SNE")))
}

plot_embeddings <- function(embeddings, dataset) {
  ggplot(embeddings, aes(V1, V2, color = Class)) +
    geom_point(size = 1) + 
    facet_wrap(~ Algorithm, scales = "free") +
    ggtitle(dataset) + 
    theme_prism()
}

# Europe data --------------------------------------------------------------
d <- read.csv("Europe.csv", header = TRUE, sep = ',')
head(d)
str(d)
summary(d)

d <- d[!(d$LU1_Desc=="Artificial land" | d$LU1_Desc=="Bareland" | 
           d$LU1_Desc=="Shrubland" | d$LU1_Desc=="Water" | d$LU1_Desc=="Wetlands"),]
head(d)
str(d)

# Land cover column
b <- d[5]
head(b)

# Min-Max Normalization for a common field plain for the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

d <- as.data.frame(lapply(d[1:4], normalize))
head(d)

# New data
d <- cbind(d, b)
head(d)

set.seed(1234)
d <- d[!duplicated(d), ]
with_labels <- d
d <- as.matrix(d[ , 1:4])

set.seed(1234)
data_result <- embed(with_labels$LU1_Desc, d)

# display results ----------------------------------------------------------
a <- plot_embeddings(data_result$results, "Europe")

fig <- a + 
  scale_colour_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
  guides(color = guide_legend(override.aes = list(size = 5), ncol = 1, title = "Land use type")) +
  theme(legend.text = element_text(size=rel(1.2))) + ggtitle("Europe")
fig

# mapping each nutrient based on levels ------------------------------------
a1 <- a$data
head(a1)

fig1 <- a1 %>% 
  mutate(SOC = a1$OC) %>%
  ggplot(aes(UMAP1, UMAP2, color = SOC)) +
  geom_point(size = 1) + 
  theme_prism() +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F",
                                         "#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4",
                                         "#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) + 
  ggtitle("OC") + theme(legend.text=element_text(size=rel(1.2)))

fig2 <- a1 %>% 
  mutate(Phosphorus = a1$P) %>%
  ggplot(aes(UMAP1, UMAP2, color = Phosphorus)) +
  geom_point(size = 1) + 
  theme_prism() +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F",
                                         "#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4",
                                         "#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) + 
  ggtitle("P") + theme(legend.text=element_text(size=rel(1.2)))

fig3 <- a1 %>% 
  mutate(Nitrogen = a1$N) %>%
  ggplot(aes(UMAP1, UMAP2, color = Nitrogen)) +
  geom_point(size = 1) + 
  theme_prism() +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F",
                                         "#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4",
                                         "#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) + 
  ggtitle("N") + theme(legend.text=element_text(size=rel(1.2)))

fig4 <- a1 %>% 
  mutate(Potassium = a1$K) %>%
  ggplot(aes(UMAP1, UMAP2, color = Potassium)) +
  geom_point(size = 1) + 
  theme_prism() +
  scale_colour_gradientn("", colours = c("#470E15","#9E0142","#D53E4F",
                                         "#F46D43","#FDAE61","#FEE08B",
                                         "#FFFFBF","#E6F598","#ABDDA4",
                                         "#66C2A5","#3288BD","#5E4FA2",
                                         "#171923","#000000")) + 
  ggtitle("K") + theme(legend.text=element_text(size=rel(1.2))) 

figs <- grid.arrange(fig,                            
                     arrangeGrob(fig1,fig2,fig3,fig4, 
                                 ncol = 2), 
                     nrow = 1)                      
figs

ggsave(plot = figs,
       filename = "Europe_method_comparison.tiff",
       device = "tiff", 
       path = "./",
       width = 18, 
       height = 8, 
       units = "in",
       dpi = 320)


