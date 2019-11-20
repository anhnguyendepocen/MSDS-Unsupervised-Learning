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

# Initial split by attribute type

housing.numeric.col <- unlist(lapply(data.housing, is.numeric))
housing.numeric <- data.housing[, housing.numeric.col, with = F]
housing.label <- data.housing[, !housing.numeric.col, with = F]

str(housing.numeric)
skim(housing.numeric)

data.housing.cor <- cor(housing.numeric)

# Overall correlations, keep as reference.

ggcorrplot(data.housing.cor,
           type = "lower",
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           lab_size = 3,
           title = "Correlogram of Housing Variables")


# Variables that have a lot of overlap in explained variance.
housing.relationships <- housing.numeric[, -c("ThreeSsnPorch", "EnclosedPorch", "OpenPorchSF", 
                                              "ScreenPorch", "WoodDeckSF", "PID", "MiscVal", "MoSold", 
                                              "YrSold", "PoolArea", "LowQualFinSF", "SID",
                                              "BsmtFinSF2", "BsmtHalfBath", "BsmtFullBath", 
                                              "KitchenAbvGr", "BedroomAbvGr", "SecondFlrSF",
                                              "LotFrontage", "BsmtFinSF1", "MasVnrArea",
                                              "Fireplaces", "FirstFloorSF", "LotArea")]
housing.relationships.cor <- cor(housing.relationships)

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

ggbiplot(housing.pca.cor)

plot(housing.pca.cor) # std plot
plot.vv(housing.pca.cor, .75) # cust vv

# PCA -> Std 

summary(housing.pca <- prcomp(housing.numeric, scale = T, center = T))

str(housing.pca)

ggbiplot(housing.pca, groups = housing.complete$Neighborhood)

ggbiplot(housing.pca, groups = housing.complete$HouseStyle)


#################
### Factor Analysis
#################

housing.fa <- fa(housing.complete.cor, nfactors = 4, )
housing.fa$loadings

housing.fa$loadings

fa.parallel(housing.complete.cor,
            n.obs = nrow(housing.complete.cor), 
            fa="both", 
            n.iter=100,
            fm="ml",
            show.legend=TRUE, main="Scree plot with parallel analysis")
