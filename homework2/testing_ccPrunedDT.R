# Tree --------------------------------------------------------------------
set.seed(1)
tree.train <- tree(progr ~ ., data = train_df)
summary(tree.train)

plot(tree.train)
text(tree.train, pretty = 0)

# Test
set.seed(1)
tree.pred <- predict(tree.train, newdata = test_df)
table(tree.pred, test_df$progr)
compute_mse(tree.pred, test_df$progr) #4519.935

# Prune
set.seed(1)
cv.train <- cv.tree(tree.train)
plot(cv.train$size, cv.train$dev, type="b")

set.seed(1)
tree.pruned <- prune.tree(tree.train, best=8)
plot(tree.pruned)
text(tree.pruned, pretty=0)

# Predictions
set.seed(1)
yhat <- predict(tree.pruned, newdata=test_df)
plot(yhat, test_df$progr)
abline(0, 1)
compute_mse(yhat, test_df$progr)#3981.871


# Cross validation on unpruned tree?
# Load necessary library
library(rsample)

# Define the number of folds
k <- 5
set.seed(1)

# Create k-folds
folds <- vfold_cv(train_df, v = k)

# Initialize a vector to store MSEs for each fold
cv_mse <- numeric(k)

# Perform k-fold cross-validation
for (i in seq_len(k)) {
  # Split the data into training and validation sets
  train_data <- analysis(folds$splits[[i]])
  validation_data <- assessment(folds$splits[[i]])
  
  # Train the tree on the training set
  tree.train <- tree(progr ~ ., data = train_data)
  
  # Perform cost-complexity pruning
  cv.train <- cv.tree(tree.train)
  best_size <- which.min(cv.train$dev)
  tree.pruned <- prune.tree(tree.train, best = best_size)
  
  # Predict on the validation set
  yhat <- predict(tree.pruned, newdata = validation_data)
  
  # Compute MSE for the current fold
  cv_mse[i] <- compute_mse(yhat, validation_data$progr)
}

# Compute the average MSE across all folds
mean_cv_mse <- mean(cv_mse)
print(mean_cv_mse)