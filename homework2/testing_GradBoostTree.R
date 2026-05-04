{
    set.seed(1)
    boosted <- gbm(progr ~ ., data=train_df, distribution="gaussian", n.trees=5000, interaction.depth=4)
    # Plots
    knitr::kable(summary(boosted))
    plot(boosted, i="TG")
    plot(boosted, i="BP")
    
    yhat.boost <- predict(boosted, newdata=test_df, n.trees=5000)
}


set.seed(1)
boosted <- gbm(progr ~ ., data=train_df, distribution="gaussian", n.trees=5000, interaction.depth=4)

knitr::kable(summary(boosted))
plot(boosted, i="BMI")
plot(boosted, i="TG")

yhat.boost <- predict(boosted, newdata=test_df, n.trees=5000)

mse <- compute_mse(yhat.boost, test_df$progr)
mse #4351.85

#6. Optimize Hyperparameters
#To optimize the model, adjust parameters like n.trees, interaction.depth, and shrinkage. 
#Use cross-validation to find the best combination.







# Define a grid of hyperparameters
{
n_trees_grid <- c(1000, 2000, 3000, 5000)
interaction_depth_grid <- c(2, 4, 6)
shrinkage_grid <- c(0.01, 0.1, 0.2)

# Initialize variables to store the best results
best_mse <- Inf
best_params <- list()

# Manual grid search
set.seed(1)
train_split <- initial_split(train_df, prop = 0.8)  # 80% training, 20% validation
train_data <- training(train_split)
validation_data <- testing(train_split)
for (n_trees in n_trees_grid) {
    for (interaction_depth in interaction_depth_grid) {
        for (shrinkage in shrinkage_grid) {
            set.seed(1)
            # Train the model with the current combination of hyperparameters
            boosted <- gbm(progr ~ ., data = train_data, distribution = "gaussian",
                           n.trees = n_trees, interaction.depth = interaction_depth,
                           shrinkage = shrinkage, verbose = FALSE)
            
            # Predict on the test set
            yhat <- predict(boosted, newdata = validation_data, n.trees = n_trees)
            
            # Compute the MSE
            mse <- compute_mse(yhat, validation_data$progr)
            
            # Update the best parameters if the current MSE is lower
            if (mse < best_mse) {
                best_mse <- mse
                best_params <- list(n_trees = n_trees, interaction_depth = interaction_depth, shrinkage = shrinkage)
            }
        }
    }
}

# Print the best hyperparameters and the corresponding MSE
print(best_params)
print(best_mse) #4277.435
}

set.seed(1)
boosted <- gbm(progr ~ ., data=train_df, distribution="gaussian", n.trees=best_params$n_trees,
                                                                  interaction.depth=best_params$interaction_depth,
                                                                  shrinkage=best_params$shrinkage)
yhat.boost2 <- predict(boosted, newdata=test_df, n.trees=1000)
mse <- compute_mse(yhat.boost2, test_df$progr)
mse #3146.236

