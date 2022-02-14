setwd("~/1_umapr-master")

gc()
rm(list=ls())

library(ggplot2)
library(gridExtra)
library(ggprism)
library(ggsignif)
library(ggpubr)
library(pastecs)

# Data call
data <- read.csv("Czechia.csv", header = TRUE, sep = ",")
head(data)
str(data)
summary(data)

data <- data[!(data$LU1_Desc=="Artificial land" | data$LU1_Desc=="Bareland" | 
           data$LU1_Desc=="Shrubland" | data$LU1_Desc=="Water" | data$LU1_Desc=="Wetlands"),]
head(data)
str(data)

# Descriptive statistics of the data
format(stat.desc(data), scientific = FALSE)

# Colour palette
rgb.palette <- colorRampPalette(c("#470E15","#9E0142","#D53E4F",
                                  "#F46D43","#FDAE61","#FEE08B",
                                  "#FFFFBF","#E6F598","#ABDDA4",
                                  "#66C2A5","#3288BD","#5E4FA2",
                                  "#171923","#000000"), space = "rgb")

# Statistical comparison
cmpr <- list(c("Woodland","Cropland"), c("Cropland","Grassland"))
cmpr1 <- list(c("Grassland","Woodland"))

a1 <- ggplot(data, aes(y=OC, x=LU1_Desc)) +
  geom_point(aes(colour=OC),size = I(1.5), alpha = 0.5,
             position=position_jitter(width=0.05, height=0.05)) +
  geom_boxplot(fill=NA, outlier.colour=NA) +
  labs(title="")+
  theme_prism() +
  coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_text(size=12,vjust = 0.5, hjust=0.5, colour='black'),
        axis.text.x = element_text(size=12))+
  scale_colour_gradientn(name="OC (g/kg)", colours =rgb.palette(10))+
  theme(legend.text = element_text(size = 10),legend.title = element_text(size = 12))+
  labs(y="OC", x = "")+ 
  ggtitle("OC | land cover classes (n = 432)")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.text=element_text(size=rel(1.2)))+
  stat_compare_means(comparisons = cmpr, tip.length=0, label.y = c(230, 250), size = 3, 
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))+
  stat_compare_means(comparisons = cmpr1, tip.length=0, label.y = 270, size = 3, 
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))

a1

a2 <- ggplot(data, aes(y=P, x=LU1_Desc)) +
  geom_point(aes(colour=P),size = I(1.5), alpha = 0.5,
             position=position_jitter(width=0.05, height=0.05)) +
  geom_boxplot(fill=NA, outlier.colour=NA) +
  labs(title="")+
  theme_prism() +
  coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_text(size=12,vjust = 0.5, hjust=0.5, colour='black'),
        axis.text.x = element_text(size=12))+
  scale_colour_gradientn(name="P (mg/kg)", colours =rgb.palette(10))+
  theme(legend.text = element_text(size = 10),legend.title = element_text(size = 12))+
  labs(y="P", x = "")+ 
  ggtitle("P | land cover classes (n = 432)")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.text=element_text(size=rel(1.2)))+
  stat_compare_means(comparisons = cmpr, tip.length=0, label.y = c(310, 340),
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))+
  stat_compare_means(comparisons = cmpr1, tip.length=0, label.y = 360,
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))

a2

a3 <- ggplot(data, aes(y=N, x=LU1_Desc)) +
  geom_point(aes(colour=N),size = I(1.5), alpha = 0.5,
             position=position_jitter(width=0.05, height=0.05)) +
  geom_boxplot(fill=NA, outlier.colour=NA) +
  labs(title="")+
  theme_prism() +
  coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_text(size=12,vjust = 0.5, hjust=0.5, colour='black'),
        axis.text.x = element_text(size=12))+
  scale_colour_gradientn(name="N (g/kg)", colours =rgb.palette(10))+
  theme(legend.text = element_text(size = 10),legend.title = element_text(size = 12))+
  labs(y="N", x = "")+ 
  ggtitle("N | land cover classes (n = 432)")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.text=element_text(size=rel(1.2)))+
  stat_compare_means(comparisons = cmpr, tip.length=0, label.y = c(15, 17),
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))+
  stat_compare_means(comparisons = cmpr1, tip.length=0, label.y = 19,
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))

a3

a4 <- ggplot(data, aes(y=K, x=LU1_Desc)) +
  geom_point(aes(colour=K),size = I(1.5), alpha = 0.5,
             position=position_jitter(width=0.05, height=0.05)) +
  geom_boxplot(fill=NA, outlier.colour=NA) +
  labs(title="")+
  theme_prism() +
  coord_flip()+
  theme(axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_text(size=12,vjust = 0.5, hjust=0.5, colour='black'),
        axis.text.x = element_text(size=12))+
  scale_colour_gradientn(name="K (mg/kg)", colours =rgb.palette(10))+
  theme(legend.text = element_text(size = 10),legend.title = element_text(size = 12))+
  labs(y="K", x = "")+ 
  ggtitle("K | land cover classes (n = 432)")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.text=element_text(size=rel(1.2)))+
  stat_compare_means(comparisons = cmpr, tip.length=0, label.y = c(1750, 1890),
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))+
  stat_compare_means(comparisons = cmpr1, tip.length=0, label.y = 1950,
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns")))

a4

fig <- grid.arrange(a1,a2,a3,a4, ncol = 2) # Multiplot

ggsave(plot = fig,
       filename = "Level_jitter_plot_Czechia.tiff",
       device = "tiff", 
       path = "./",
       width = 12, 
       height = 8, 
       units = "in",
       dpi = 320)
