library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(ggthemes)
library(formattable)
library(scales)
library(reshape2)
library(skimr)
library(gridExtra)
library(lessR)
library(ggiraphExtra)
library(olsrr)
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(MASS)
library(Metrics)
library(stringr)
library(Rtsne)
library(plotly)
library(tidyverse)
library(psych)
library(ggcorrplot)
library(ggbiplot)
library(corrplot)
library(RColorBrewer)
library(ggdendro)
library(dendextend)

#####################################################################
######################### Unsupervised Learning #####################
#####################################################################

data.dir <- "D:/Projects/MSDS-Unsupervised-Learning/datasets/"
project.dir <- "D:/Projects/MSDS-Unsupervised-Learning/Project/"
  
setwd(project.dir)

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

# Data set

data.housing <- data.table(read.csv(paste0(data.dir, "ames_housing_data.csv")))

# Basic Data Structure

ncol(data.housing)
head(data.housing)
names(data.housing)

# New Features

data.housing[, TotalFloorSF := FirstFlrSF + SecondFlrSF]
data.housing[, HouseAge := YrSold - YearBuilt]
data.housing[, QualityIndex := OverallQual * OverallCond]
data.housing[, logSalePrice := log(SalePrice)]
data.housing[, Price_Sqft := SalePrice / TotalFloorSF]
data.housing[, TotalBath := FullBath + HalfBath]

summary(data.housing)

#################
### PCA
#################

# Utility

plot.vv <- function( pca, threshold = .8 ) {
  
  vv <- cumsum(pca$sdev^2)/sum(pca$sdev^2)
  vv.y <- head(vv[vv > threshold], 1)
  vv.x <- which(vv == vv.y)
  
  ggplot(data.table(y = vv)[, x := .I], aes(x,y)) + 
    geom_path(lwd = .8) +
    geom_point(color = "red", lwd = 4, shape = 1) +
    geom_vline(xintercept = vv.x, color = "cornflowerblue", lwd = .8, alpha = .8) +
    geom_hline(yintercept = vv.y, color = "cornflowerblue", lwd = .8, alpha = .8) +
    labs(title = "Total Variance Explained", 
         x = "Number of Componenets", 
         y = "Total Variance Explained")
}

plot.pca.loadings <- function(pca) {
  pc.1 <- pca$loadings[,1]; pc.2 <- pca$loadings[,2]
  
  colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
  
  pc <- data.table( Name = names(pc.1), X = pc.1, Y = pc.2)
  pc$col <- colfunc(nrow(pc))
  
  ggplot(pc, aes(X, Y)) +
    geom_text(aes(label = Name, col = col, size = 15)) +
    labs(x = "Component 1", y = "Component 2") +
    guides(col = "none", size = "none") +
    ggtitle("AMES Housing Principal Components")
}

# Initial split by attribute type

orig.housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
orig.housing.numeric <- data.housing[, orig.housing.numeric.col, with = F]
orig.housing.label <- data.housing[, !orig.housing.numeric.col, with = F]

str(orig.housing.numeric)
skim(orig.housing.numeric)

data.housing.cor <- cor(orig.housing.numeric)

# Overall correlations, keep as reference.

ggcorrplot(data.housing.cor,
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Housing Variables")

# Variables that have a lot of overlap in explained variance.
housing.relationships <- orig.housing.numeric[, -c("ThreeSsnPorch", "FirstFlrSF", "GarageYrBlt", 
                                                   "BsmtUnfSF", "TotalBsmtSF", "logSalePrice", 
                                                   "LotArea", "KitchenAbvGr", "Fireplaces", "MasVnrArea", 
                                                   "BsmtFinSF1", "BedroomAbvGr", "SecondFlrSF", "LotFrontage",
                                                   "BsmtFullBath", "BsmtHalfBath", "BsmtFinSF2", "ScreenPorch", 
                                                   "WoodDeckSF", "PID", "MiscVal", "MoSold", "PoolArea", 
                                                   "LowQualFinSF", "SID", "EnclosedPorch", "OpenPorchSF", 
                                                   "GarageArea")]

housing.relationships <- orig.housing.numeric
housing.relationships.cor <- cor(housing.relationships)

str(housing.numeric)

# Iterate & Clean
ggcorrplot(housing.relationships.cor,
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Housing Variables")

str(housing.relationships)

# Post clean-up data

housing.complete <- cbind(housing.label, housing.relationships)
housing.complete <- housing.complete[complete.cases(housing.numeric)]

str(housing.complete)
dim(housing.complete)

housing.numeric.col <- unlist(lapply(housing.complete, is.numeric))
housing.numeric <- housing.complete[, housing.numeric.col, with = F]
housing.label <- housing.complete[, !housing.numeric.col, with = F]

# Variable Correlations

housing.complete.cor <- cor(housing.numeric)

corrplot(housing.complete.cor)

housing.num.dim <- dim(housing.complete.cor)
housing.matrix <- matrix(housing.complete.cor,
                         nrow = housing.num.dim[1], 
                         ncol = housing.num.dim[2], byrow = T)

assertthat::are_equal(isSymmetric(housing.matrix), T)

housing.eigen <- eigen(housing.matrix)

# PCA -> Cor

summary(housing.pca.cor <- princomp( x = housing.complete.cor, cor = T ))

ggbiplot(housing.pca.cor) +
  labs(title = "Variable Dimensions")

plot(housing.pca.cor) # std plot
plot.vv(housing.pca.cor, .8) # cust vv

plot.pca.loadings(housing.pca.cor)

# PCA -> Std 

summary(housing.pca <- prcomp(x = housing.numeric, scale = T, center = T))

str(housing.pca)

# Exploratory, throw away

ggbiplot(housing.pca, groups = housing.complete$Neighborhood)

ggbiplot(housing.pca, groups = housing.complete$HouseStyle)

ggbiplot(housing.pca, groups = housing.complete$GarageType)

ggbiplot(housing.pca, groups = housing.complete$BldgType)

ggbiplot(housing.pca, groups = housing.complete$YrSold)

# Maybe Useful

summary(housing.complete$OverallQual)

housing.complete$QualGroup <- cut(housing.complete$OverallQual, seq(0, 10, 2))

ggbiplot(housing.pca, ellipse = T, groups = housing.complete$QualGroup) +
  labs(title = "Quality Groups")

value.groups <- 6

housing.complete[, ValueGroup := cut(SalePrice, value.groups, dig.lab = 5)]
value.labs <- levels(cut(housing.complete$SalePrice, value.groups))
value.labels <- data.table(cbind(lower = dollar( as.numeric( sub("\\((.+),.*", "\\1", value.labs) ) ),
                                 upper = dollar( as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", value.labs)))))
value.labels[, label := paste(lower, "-", upper)]
levels(housing.complete$ValueGroup) <- value.labels$label

ggbiplot(housing.pca, ellipse = T, groups = housing.complete$ValueGroup) +
  guides(color=guide_legend("Value", labels = comma)) +
  labs(title = "Value Groups")

##############################
# t-SNE Analysis
##############################

plot_tsne_cor <- function(cor, perplexity = 1, learning = 20, iterations = 5000, title = NULL) {
  
  def.title = paste("t-SNE: P=", perplexity, " L=", learning, "Iterations=", iterations )
  
  tsne <- Rtsne(cor, dims = 2, perplexity=perplexity, verbose =TRUE, max_iter = iterations, learning = learning)
  tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], Attribute = row.names(cor))
  
  ggplot(tsne.results, aes(x, y, label = Attribute)) +
    geom_point(aes(col = Attribute), size = 3) +
    geom_text_repel(aes(label = Attribute),
                    size = 4, box.padding = 1.5,
                    segment.size  = 0.2, segment.color = "grey50") +
    labs(title = ifelse(is.null(title), def.title, title), x = "tSNE dimension 1", y = "tSNE dimension 2") +
    theme(legend.position = "none")
}

plot_tsne_cor(housing.complete.cor, perplexity = 6, learning = 50, iterations = 5000)
plot_tsne_cor(housing.complete.cor, perplexity = 3, learning = 25, iterations = 5000, "AMES Housing Features")
plot_tsne_cor(housing.complete.cor, perplexity = 2, learning = 35, iterations = 5000)

# Value/Quality groupings?

perplexity <- 2
learning <- 25
iterations <- 5000
tsne <- Rtsne(housing.numeric, dims = 2, perplexity=perplexity, verbose =TRUE, max_iter = iterations, learning = learning)

tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], Attribute = housing.complete$ValueGroup)

ggplot(tsne.results, aes(x, y, label = Attribute)) +
  geom_point(aes(col = Attribute), size = 3) +
  labs(title = paste("t-SNE: P=", perplexity, " L=", learning, "Iterations=", iterations ), x = "tSNE dimension 1", y = "tSNE dimension 2") +
  theme(legend.position = "bottom")

tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], Attribute = housing.complete$QualGroup)

ggplot(tsne.results, aes(x, y, label = Attribute)) +
  geom_point(aes(col = Attribute), size = 3) +
  labs(title = paste("t-SNE: P=", perplexity, " L=", learning, "Iterations=", iterations ), x = "tSNE dimension 1", y = "tSNE dimension 2") +
  theme(legend.position = "bottom")


##############################
# Hierarchical Clustering Analysis, PCA
##############################

housing.pc <- data.table( Name = colnames(housing.numeric), 
                          X = housing.pca.cor$loadings[, 1], 
                          Y = housing.pca.cor$loadings[, 2])

plot.hclust.pca <- function( data, method = "complete", k = 3 ) {

  # H-Clust
  
  pca.dist <- dist( data[, 2:3])
  
  pca.hclustmodel <- hclust(pca.dist, method = method)
  
  # Clean-up Dendro
  pca.hc <- dendro_data(pca.hclustmodel)
  dict <- setNames(housing.pc$Name, 1:nrow(housing.pc))
  pca.hclustmodel$labels <- sapply(pca.hc$labels$label, function(x) dict[[as.character(x)]])
  
  dend <- pca.hclustmodel %>%
    as.dendrogram %>%
    set("branches_k_color", k=k) %>% set("branches_lwd", 1.2) %>%
    set("labels_colors") %>% set("labels_cex", c(.6, .8)) %>% 
    set("leaves_pch", 19)
  
  plot(dend) +
    title(main = paste0("AMES Housing Attribute Clusters, k=", k))
}

?hclust

plot.hclust.pca(housing.pc, method = "ward.D", k = 3)
plot.hclust.pca(housing.pc, method = "complete", k = 4)
plot.hclust.pca(housing.pc, method = "average", k = 6)
plot.hclust.pca(housing.pc, method = "mcquitty", k = 6)
