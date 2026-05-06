{
    set.seed(1)
    n_pred <- ncol(train_df) - 1
}

{
    set.seed(1)
    bag.train <- randomForest(progr ~ ., data=train_df, mtry=n_pred, importance=TRUE)
    bag.train
    yhat.bag <- predict(bag.train, newdata=test_df)
    plot(yhat.bag, test_df$progr)
    abline(0,1)
    compute_mse(yhat.bag,test_df$progr)#3243.689
}
# More trees

{
    set.seed(1)
    ntree_grid <- seq(50, 500, by = 50)
    mtry_grid <- seq(1, n_pred, by = 1) 
    best_mse <- Inf
    best_ntree <- NULL
    best_mtry <- NULL
    
    train_split <- initial_split(train_df, prop = 0.8)  # 80% training, 20% validation
    train_data <- training(train_split)
    validation_data <- testing(train_split)
    
    for (ntree in ntree_grid) {
        for (mtry in mtry_grid) {
            set.seed(1)
            bag.train <- randomForest(progr ~ ., data = train_data, mtry = mtry, ntree = ntree, importance = TRUE)
            bag.pred <- predict(bag.train, newdata = validation_data)
            mse <- compute_mse(bag.pred, validation_data$progr)
            if (mse < best_mse) {
                best_mse <- mse
                best_ntree <- ntree
                best_mtry <- mtry
            }
        }
    }
    print(best_mse) # 3851.084
    print(best_ntree) # 300
    print(best_mtry) # 2
}

set.seed(1)
bag.train <- randomForest(progr ~ ., data=train_df, mtry=best_mtry, importance=TRUE, ntree=best_ntree)
knitr::kable(importance(bag.train))
varImpPlot(bag.train)
set.seed(1)
bag.pred <- predict(bag.train, newdata = test_df)
mse <- compute_mse(bag.pred, test_df$progr)
mse #3385.157

#OOB error
{
    set.seed(1)
    rf <- randomForest(progr ~ ., data = train_df, mtry = best_mtry, ntree = best_ntree, importance = TRUE)
    oob <- tail(rf$mse, 1)
    oob #3522.543
    plot(rf$mse, type = "l")
}
