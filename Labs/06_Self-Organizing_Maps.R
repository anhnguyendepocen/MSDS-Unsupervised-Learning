require(tidyverse)
require(kohonen)
require(ggplot2)
require(ggridges)

###########################
# Resources
###########################

# https://rpubs.com/erblast/SOM

###########################
# Helper functions
###########################

# find the graph node number by the coordinates
find_node_by_coordinates <- function(x, y, grid_width) {
  return(((y * grid_width) + x) - grid_width)
}

# return the number of observations represented by each node
get_node_counts <- function(x) {
  df <- data.frame(node = x)
  counts <- df %>%
    group_by(node) %>%
    summarize(observations = n())
}

# guideline for grid size = 5 * sqrt(N)
# where N is the number of observations in the data set
find_grid_size <- function(N) {
  return(floor(sqrt(sqrt(N) * 5)))
}

# Shane Lynn 14-01-2014 used to define the palette
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

###########################
# Data load and prep
###########################

# load data - select the Credit_Card_Applications.csv
myFile <- file.choose()
raw_data <- read.csv(myFile,header=TRUE)

# check for missing observations
summary(raw_data)
levels(raw_data$Class) <- c('Not Approved', 'Approved')

# review the distributions of normal and fraudulent applications
raw_data %>%
  select(-CustomerID) %>%
  gather(variable, value, -Class) %>%
  ggplot(aes(y = as.factor(variable),
             fill = Class,
             x = percent_rank(value))) +
  geom_density_ridges() +
  ggtitle('Class Distributions') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Value') +
  ylab('Variable')

# remove CustomerID and Class
data <- raw_data[, 2:15]

# find all columns having numeric data
numerics <- data %>%
  select_if(is.numeric) %>%
  names

# find all columns having factors
factors <- data %>%
  select_if(is.factor) %>%
  names

data_list = list()
distances = vector()

# create a layer for each factor
for (fac in factors){
  data_list[[fac]] = kohonen::classvec2classmat( data[[fac]] )
  distances = c(distances, 'tanimoto')
}

data_list[['numerics']] = scale(data[,numerics])
distances = c(distances, 'sumofsquares')

# review data_list
str(data_list)
names(data_list)

# review distance measures
distances

# call the find_grid_size function to calculate the grid size
map_dimension = find_grid_size(dim(raw_data)[1])

# set the number of times the model will cycle through the observations
epochs = 2000
set.seed(123)

# create a grid onto which the som will be mapped
som_grid = somgrid(xdim = map_dimension
                   ,ydim = map_dimension
                   ,topo = "rectangular")

# train the SOM
cc_som = supersom(data_list
                  ,grid = som_grid
                  ,rlen = epochs
                  ,alpha = c(0.1, 0.01)
                  ,whatmap = c(factors, 'numerics')
                  ,dist.fcts = distances
                  ,keep.data = TRUE
)

plot(cc_som, type = "changes")

cc_som$unit.classif
observations_by_node <- get_node_counts(cc_som$unit.classif)

plot(cc_som, type = "dist.neighbours", palette.name = coolBlueHotRed)

anomalies <- find_node_by_coordinates(11, 3, 11)
rows <- which(cc_som$unit.classif == anomalies)
raw_data[rows, ]

plot(cc_som, type = "codes", palette.name = coolBlueHotRed)

cc_som$codes[["A12"]]

plot(cc_som, type = "property", property = getCodes(cc_som)[["A4"]],
     main=colnames(getCodes(cc_som))[["A4"]], palette.name=coolBlueHotRed)


plot(cc_som, type = "quality", palette.name = coolBlueHotRed)

###########################
# Clustering
###########################

library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# fuse all layers into one dataframe
codes = tibble( layers = names(cc_som$codes)
                ,codes = cc_som$codes ) %>%
  mutate( codes = purrr::map(codes, as_tibble) ) %>%
  spread( key = layers, value = codes) %>%
  apply(1, bind_cols) %>%
  .[[1]] %>%
  as_tibble()

# generate distance matrix for codes
dist_m = dist(codes) %>%
  as.matrix()

# generate seperate distance matrix for map location
dist_on_map = kohonen::unit.distances(som_grid)

#exponentiate euclidean distance by distance on map
dist_adj = dist_m ^ dist_on_map

clust_adj = hclust(as.dist(dist_adj), 'ward.D2')

som_cluster_adj = cutree(clust_adj, 11)

plot(cc_som, type="codes", main = "Clusters", bgcol = col_vector[som_cluster_adj], pchs = NA)