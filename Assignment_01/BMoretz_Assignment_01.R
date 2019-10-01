library(data.table)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(RColorBrewer)

theme_set(theme_gdocs())

path.data <- "D:/Projects/MSDS-Unsupervised-Learning/datasets"
setwd(path.data)

# Load Stock Data

stock.data <- read_excel("stockdata.xlsx",
                          col_types = c("date", rep("numeric", 42)))

head(stock.data)
str(stock.data)

sorted.df <- stock.data[order(stock.data$Date),];
head(sorted.df)

AA <- log(sorted.df$AA[-1]/sorted.df$AA[-dim(sorted.df)[1]]);

# Manually check the first entry: log(9.45/9.23)
# Type cast the array as a data frame;

returns.df <- as.data.frame(AA);
returns.df$BAC <- log(sorted.df$BAC[-1]/sorted.df$BAC[-dim(sorted.df)[1]]);
returns.df$BHI <- log(sorted.df$BHI[-1]/sorted.df$BHI[-dim(sorted.df)[1]]);
returns.df$CVX <- log(sorted.df$CVX[-1]/sorted.df$CVX[-dim(sorted.df)[1]]);
returns.df$DD  <- log(sorted.df$DD[-1]/sorted.df$DD[-dim(sorted.df)[1]]);
returns.df$DOW <- log(sorted.df$DOW[-1]/sorted.df$DOW[-dim(sorted.df)[1]]);
returns.df$DPS <- log(sorted.df$DPS[-1]/sorted.df$DPS[-dim(sorted.df)[1]]);
returns.df$GS  <- log(sorted.df$GS[-1]/sorted.df$GS[-dim(sorted.df)[1]]);
returns.df$HAL <- log(sorted.df$HAL[-1]/sorted.df$HAL[-dim(sorted.df)[1]]);
returns.df$HES <- log(sorted.df$HES[-1]/sorted.df$HES[-dim(sorted.df)[1]]);
returns.df$HON <- log(sorted.df$HON[-1]/sorted.df$HON[-dim(sorted.df)[1]]);
returns.df$HUN <- log(sorted.df$HUN[-1]/sorted.df$HUN[-dim(sorted.df)[1]]);
returns.df$JPM <- log(sorted.df$JPM[-1]/sorted.df$JPM[-dim(sorted.df)[1]]);
returns.df$KO  <- log(sorted.df$KO[-1]/sorted.df$KO[-dim(sorted.df)[1]]);
returns.df$MMM <- log(sorted.df$MMM[-1]/sorted.df$MMM[-dim(sorted.df)[1]]);
returns.df$MPC <- log(sorted.df$MPC[-1]/sorted.df$MPC[-dim(sorted.df)[1]]);
returns.df$PEP <- log(sorted.df$PEP[-1]/sorted.df$PEP[-dim(sorted.df)[1]]);
returns.df$SLB <- log(sorted.df$SLB[-1]/sorted.df$SLB[-dim(sorted.df)[1]]);
returns.df$WFC <- log(sorted.df$WFC[-1]/sorted.df$WFC[-dim(sorted.df)[1]]);
returns.df$XOM <- log(sorted.df$XOM[-1]/sorted.df$XOM[-dim(sorted.df)[1]]);
returns.df$VV  <- log(sorted.df$VV[-1]/sorted.df$VV[-dim(sorted.df)[1]]);

# Compute correlation matrix for returns;
returns.cor <- cor(returns.df)
returns.cor[, c('VV')]

round(returns.cor, 2)

tail(sort(returns.cor[which(returns.cor < 1 )]), 5)

returns.cor[2:21, c('AA')]

# Barplot the last column to visualize magnitude of correlations;
barplot(returns.cor[1:20,c('VV')],las=2,ylim=c(0,1.0))
title('Correlations with VV')

# Make correlation plot for returns;
# If you need to install corrplot package;  Note how many dependencies this package has;
#install.packages('corrplot', dependencies=TRUE)

corrplot(returns.cor)

# load car package
require(car)

# Fit some model
model.1 <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=returns.df)
summary(model.1)
vif.1 <- vif(model.1)
vif.1[which(vif.1 > 2.5)]

# Fit the full model
model.2 <- lm(VV ~ BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM,data=returns.df)
summary(model.2)
vif.2 <- vif(model.2)

vif.2[which(vif.2 > 2.5)]

# Principal Component Analysis

returns.pca <- princomp(x=returns.df[,-21],cor=TRUE)
# See the output components returned by princomp();
names(returns.pca)

pc.1 <- returns.pca$loadings[,1];
pc.2 <- returns.pca$loadings[,2];
names(pc.1)

dev.off()
plot( -10,10, type='p', xlim=c(-0.27,-0.12), ylim=c(-0.27,0.6),xlab='PC 1',ylab='PC 2')
text(-pc.1, -pc.2, labels=names(pc.1), cex=0.75)

# Load meta data for symbols.

symbol.info <- data.table( Symbol = c("AA", "BAC", "BHI", "CVX", "DD", "DOW", "DPS", "GS", "HAL", "HES", "HON", "HUN", "JPM", "KO", "MMM", "MPC", "PEP", "SLB", "WFC", "XOM", "VV"),
                           Name = c('Alcoa Aluminum', 'Bank of America', 'Baker Hughes Incorprated', 'Chevron', 'Dupont', 'Dow Chemical', 'DrPepper Snapple', 'Goldman Sachs', 'Halliburton', 'Hess Energy', 'Honeywell International', 'Huntsman Corporation', 'JPMorgan Chase', 'The Coca-Cola Company', '3M Company', 'Marathon Petroleum Corp', 'Pepsi Company', 'Schlumberger', 'Wells Fargo', 'Exxon-Mobile', 'Vanguard Large Cap Index'),
                           Industry = c('Industrial - Metals', 'Banking', 'Oil Field Services', 'Oil Refining', 'Industrial - Chemical', 'Industrial - Chemical', 'Soft Drinks', 'Banking', 'Oil Field Services', 'Oil Refining', 'Manufacturing', 'Industrial - Chemical', 'Banking', 'Soft Drinks', 'Manufacturing', 'Oil Refining', 'Soft Drinks', 'Oil Field Services', 'Banking', 'Oil Refining', 'Market Index' ))

industries <- unique(symbol.info$Industry)
industry.color <- brewer.pal(length(industries), "BrBG")

col.table <- data.table( Industry = industries, Shade = industry.color)

symbol.info <- merge(symbol.info, col.table, on = c("Industry"))

pc.1.long <- data.table(melt(pc.1), keep.rownames = T)
colnames(pc.1.long) <- c("Symbol", "PC1")

pc.2.long <- data.table(melt(pc.2), keep.rownames = T)
colnames(pc.2.long) <- c("Symbol", "PC2")

symbol.info <- merge(symbol.info, pc.1.long, by = c("Symbol"))
symbol.info <- merge(symbol.info, pc.2.long, by = c("Symbol"))

# Plot PCA by Symbol, colored by Industry
ggplot(symbol.info, aes(x = -PC1, y = -PC2, label = Symbol, color = Industry)) +
  geom_text() +
  xlab("PC1") +
  ylab("PC2") +
  xlim(-0.27, -0.125) +
  ylim(-0.27, 0.6) +
  ggtitle("Principal Component Loadings") +
  theme_gdocs(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#F2F2F2"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "grey50"),
    plot.title = element_text(size = 25,
                              face = "bold",
                              color = "#1C93D1",
                              hjust = 0.5,
                              lineheight = 1.2), # title
    panel.border = element_rect(colour = "#d3d3d3"),
    plot.subtitle = element_text(size = 15,
                                 face = "bold",
                                 hjust = 0.5), # subtitle
    plot.caption = element_text(size = 15), # caption
    axis.title.x = element_text(vjust = 0,
                                size = 22,
                                color = "#1C93D1",
                                face = "bold"), # X axis title
    axis.title.y = element_text(size = 22,
                                color = "#1C93D1",
                                face = "bold"), # Y axis title
    axis.text.x = element_text(size = 12,
                               face = "bold",
                               vjust = 0), # X axis text
    axis.text.y = element_text(size = 12,
                               face = "bold"), # Y axis text
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  )

# Plot the default scree plot;
plot(returns.pca)

# Make Scree Plot
scree.values <- (returns.pca$sdev^2)/sum(returns.pca$sdev^2);

plot(scree.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(scree.values,lwd=2,cex=1.5)
title('Scree Plot')


# Make Proportion of Variance Explained
variance.values <- cumsum(returns.pca$sdev^2)/sum(returns.pca$sdev^2);

plot(variance.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(variance.values,lwd=2,cex=1.5)
abline(h=0.8,lwd=1.5,col='red')
abline(v=8,lwd=1.5,col='red')
text(13,0.5,'Keep 8 Principal Components',col='red')
title('Total Variance Explained Plot')



