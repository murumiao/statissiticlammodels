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
