# Autoencoder
###########################
# Resources
###########################
# https://blogs.rstudio.com/tensorflow/posts/2018-01-24-keras-fraud-autoencoder/ (Links to an external site.)

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(keras)
library(pROC)
library(caret)
library(reshape2)

###########################
# Helper functions
###########################

# normalize using the min-max method
normalize <- function(x) {
  denom <- ifelse(max(x) - min(x) == 0, 1, max(x) - min(x))
  return ((x - min(x)) / denom)
}

###########################
# Data load and prep
###########################

# load data - select the creditcard.csv
myfile <- file.choose()
cc_data <- read.csv(myfile, header = TRUE)

###########################
# Exploratory Data Analysis
# Data Prep
###########################

cc_data %>%
  gather(variable, value, -Class) %>%
  ggplot(aes(y = as.factor(variable),
             fill = as.factor(Class),
             x = percent_rank(value))) +
  geom_density_ridges() +
  labs(fill = 'Fraud', y = 'Variables', x = '')

# Review range of variables and ensure no N/As exist
summary(cc_data)

# Check to see if dataset is balanced
cc_data %>%
  group_by(Class) %>%
  summarize(Count = n())

normalized_data <- cc_data %>%
  select(-Time, -Class) %>%
  as_tibble()

# Normalize the Amount
normalized_data$Amount <- normalize(normalized_data$Amount)

###########################
# Split the dataset
###########################

## set the seed to make your partition reproducible
set.seed(123)
smp_size <- floor(0.80 * nrow(normalized_data))
train_ind <- sample(seq_len(nrow(normalized_data)), size = smp_size)

df_train <- normalized_data[train_ind, ]
df_test <- normalized_data[-train_ind, ]

y_train <- cc_data$Class[train_ind]
y_test <- cc_data$Class[-train_ind]

# Create the training matrix
x_train <- df_train %>%
  as.matrix()

# Create the testing matrix
x_test <- df_test %>%
  as.matrix()

###########################
# Construct and compile the autoencoder
###########################

# Create a sequential model
model <- keras_model_sequential()
model %>%
  # encoder
  layer_dense(units = 15, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  # decoder
  layer_dense(units = 15, activation = "tanh") %>%
  layer_dense(units = ncol(x_train)
  )

# Compile the model
model %>% compile(
  loss='mean_squared_error',
  optimizer='adam',
  metrics = c('accuracy')
)

# View the model summary
summary(model)

###########################
# Fit the model
# Save the best model
###########################

# Save the best model
checkpoint <- callback_model_checkpoint(
  filepath = "model.hdf5",
  save_best_only = TRUE,
  period = 1,
  verbose = 1
)

# Stop training if val_loss stops decreasing
early_stopping <- callback_early_stopping(monitor = 'val_loss', patience = 5)

# Train the model
model %>% fit(
  x = x_train[y_train == 0,],
  y = x_train[y_train == 0,],
  epochs = 100,
  batch_size = 32,
  validation_data = list(x_test[y_test == 0,], x_test[y_test == 0,]),
  callbacks = list(checkpoint, early_stopping)
)

# Check the loss
loss <- evaluate(model, x = x_test[y_test == 0,], y = x_test[y_test == 0,])
loss


###########################
# Load the saved model
# Calculate mse
###########################
# Load the model
model <- load_model_hdf5("model.hdf5", compile = FALSE)

# Reconstruct the training set and calculate the mse
pred_train <- predict(model, x_train, batch_size = 32)
mse_train <- apply((x_train - pred_train) ^2, 1, sum)

# Reconstruct the test set and calculate the mse
pred_test <- predict(model, x_test, batch_size = 32)
mse_test <- apply((x_test - pred_test) ^2, 1, sum)

###########################
# AUC and ROC plots
###########################

# Calculate area under the curve and plot the ROC
train_roc <- roc(y_train, mse_train)
plot(train_roc, col = "blue", main = paste0("ROC - Training Set: AUC - ", round(train_roc$auc, 4)))

# Calculate area under the curve and plot the ROC
test_roc <- roc(y_test, mse_test)
plot(test_roc, col = "blue", main = paste0("ROC - Test Set: AUC - ", round(test_roc$auc, 4)))

###########################
# Recall and precision
###########################
# Calculate the precision
possible_k <- seq(0, 5000, length.out = 100)
precision <- sapply(possible_k, function(k) {
  predicted_class <- as.numeric(mse_test > k)
  sum(predicted_class == 1 & y_test == 1)/sum(predicted_class)
})

# Calculate the recall
recall <- sapply(possible_k, function(k) {
  predicted_class <- as.numeric(mse_test > k)
  sum(predicted_class == 1 & y_test == 1)/sum(y_test)
})

# Create dataset for plotting
evaluation <- cbind(seq(1:100), precision, recall) %>%
  as_tibble()
colnames(evaluation) <- c('Threshold', 'Precision', 'Recall')
evaluation <- melt(evaluation, id = 'Threshold')

# Plot precision and recall
ggplot() +
  geom_line(data = evaluation, aes(x = Threshold, y = value, colour = variable)) +
  labs(title = 'Precision / Recall')

###########################
# Fraudulent transactions
###########################

frauds <- replicate(length(y_test), 0)
frauds[which(mse_test >= 60)] <- 1

# Create confusion matrix
confusionMatrix(factor(frauds), factor(y_test))