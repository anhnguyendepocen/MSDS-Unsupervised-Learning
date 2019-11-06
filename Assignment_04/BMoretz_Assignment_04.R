library(data.table)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)
library(formattable)
library(readxl)
library(MASS)
library(GGally)
library(ggcorrplot)
library(ggdendro)
library(dendextend)

theme_set(theme_light())

# Theme Overrides
theme_update(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkgreen"),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             plot.subtitle = element_text(face = "bold", size = 8, colour = "darkred"),
             legend.title = element_text(size = 12, color = "darkred", face = "bold"),
             legend.position = "right", legend.title.align=0.5,
             panel.border = element_rect(linetype = "solid", 
                                         colour = "lightgray"), 
             plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "inches"))

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"

setwd(path.data)

eur.employment <- read.csv("EuropeanEmployment.csv")

print(eur.employment)

# ggpairs(eur.employment, cardinality_threshold = 30)

ggpairs(eur.employment[, -1], aes(col = Group), 
        cardinality_threshold = 30)

ggpairs(eur.employment, aes(col = Country), 
        cardinality_threshold = 30)

ggscatmat(eur.employment) +
  ggtitle("Scatterplot Matrix")

# A

ggplot(eur.employment, aes(FIN, SER, col = Country)) +
  geom_point(aes(size = 20)) +
  ggtitle("Financial vs Services") +
  guides(size = "none")

# B

ggplot(eur.employment, aes(SER, MAN, col = Country)) +
  geom_point(aes(size = 20)) +
  ggtitle("Manufacturing vs Services") +
  guides(size = "none")


ggplot(eur.employment, aes(MAN, FIN, col = Group)) +
  geom_point() +
  ggtitle("Manufactering vs Financials")

ggplot(eur.employment, aes(SPS, SER, col = Group)) +
  geom_point() +
  ggtitle("Social vs Service")

ggplot(eur.employment, aes(TC, FIN, col = Group)) +
  geom_point() +
  ggtitle("Transport vs Financials")

# Principal Component Analysis

employment.pca <- princomp( x = eur.employment[, -c(1,2)], cor=TRUE)

# See the output components returned by princomp();
names(employment.pca)

pc.1 <- employment.pca$loadings[,1];
pc.2 <- employment.pca$loadings[,2];

names(pc.1)

plot( -10,10, type='p', 
      xlim=c(-.6, .6), 
      ylim=c(-.6, .6),
      xlab='PC 1',ylab='PC 2')
text(pc.1, pc.2, labels=names(pc.1), cex=0.75)


pc <- data.table( Name = names(pc.1), X = pc.1, Y = pc.2)
pc$col <- brewer.pal(nrow(pc), "BrBG")

ggplot(pc, aes(X, Y)) +
  geom_text(aes(label = Name, col = col, size = 15)) +
  labs(x = "Component 1", y = "Component 2") +
  guides(col = "none", size = "none") +
  ggtitle("European Employment Principal Components")


employment.pca.std <- princomp( x = scale(eur.employment[, -c(1,2)]), cor = TRUE)

std.pc.1 <- employment.pca.std$loadings[,1];
std.pc.2 <- employment.pca.std$loadings[,2];

std.pc <- data.table( Name = names(pc.1), X = pc.1, Y = pc.2)
std.pc$col <- brewer.pal(nrow(pc), "BrBG")

ggplot(std.pc, aes(X, Y)) +
  geom_text(aes(label = Name, col = col, size = 15)) +
  labs(x = "Component 1", y = "Component 2") +
  guides(col = "none", size = "none") +
  ggtitle("European Employment Principal Components (Standardized)")

# (4)	Hierarchical Clustering Analysis

clusters <- hclust(dist(eur.employment[, -c(1,2)]))
plot(clusters)

ggdendrogram(clusters) +
  ggtitle("European Employment Dendrogram")

k3 <- cutree(clusters, k = 3)
rect.hclust(clusters, k=3, border="red")

k6 <- cutree(clusters, k = 6)
rect.hclust(clusters, k = 6, border="red")

dend <- as.dendrogram(clusters)

d3 = color_branches(dend, k=3)
plot(d3) +
  title("Dendrogram: K=3")

d6 = color_branches(dend, k=6)
plot(d6) + 
  title("Dendrogram: K=6")


pca.clusters <- hclust(dist(pc[, 2:3]))

ggdendrogram(pca.clusters) +
  ggtitle("European Employment Dendrogram - PCA")

cutree(pca.clusters, k = 3)

dend <- as.dendrogram(pca.clusters)

d3 = color_branches(dend, k=3)
plot(d3) +
  title("PCA Dendrogram: K=3")

d6 = color_branches(dend, k=6)
plot(d6) + 
  title("PCA Dendrogram: K=6")

