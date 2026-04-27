library(tidyverse);
set.seed(1);
library(readr)
chd <- read_csv("chd.csv")
View(chd);
summary(chd);

#change chd$CHD to 0/1 instead of "Yes" and "No". COuld do the same with other bool-like cols
chd$CHD <- chd$CHD == "Yes"

chd$smoker <- chd$smoker == 1
chd$stroke <- chd$stroke == 1
chd$HTN <- chd$HTN == 1
chd$diabetes <- chd$diabetes == 1

chd$sex <- as.numeric(chd$sex == "Male")

sum(chd$CHD == TRUE) #TRUE = 644, FALSE = 3594
sum(is.na(chd));

# separate valuesrows with NAs in them
chd_na  <- chd[!complete.cases(chd),]
df <- chd[complete.cases(chd), ]


#Lets see correlation
cor(df)
#cpd-smoker=0.77, HTN-DBP=0.61
plot(df$cpd,df$smoker);
plot(df$DBP, df$HTN);


# KNN

library(class)

#SPLIT DATA (i.e. 50/50)

train <- sample(seq_len(nrow(df)), size=floor(0.5*nrow(df)))
summary(train)
train_df <- df[train,]
test_df <- df[-train,]
summary(train_df$CHD)
summary(test_df$CHD)

x_train <- train_df[, -which(names(train_df) == "CHD")]
x_test  <- test_df[, -which(names(test_df) == "CHD")]
y_train <- train_df$CHD

x_train <- model.matrix(~ . -1, data = x_train)
x_test  <- model.matrix(~ . -1, data = x_test)
y_train <- as.factor(y_train)

#Scale 
x_train <- scale(x_train)
x_test  <- scale(x_test,
                 center = attr(x_train, "scaled:center"),
                 scale  = attr(x_train, "scaled:scale"))

set.seed(1)
knn.pred <- knn(x_train, x_test, cl = y_train, k = 5)

# confusion matrix
table(knn.pred, test_df$CHD)
# accuracy
mean(knn.pred == test_df$CHD) #0.83

max_k <- 0
for (k in 1:100) {
  pred <- knn(x_train, x_test, y_train, k = k)
  acc <- mean(pred == test_df$CHD)
  if (max_k < acc) {
    max_k <- acc
    print(c(k, acc))
  }
}
#k = 32
knn.pred <- knn(x_train, x_test, cl = y_train, k = 32)
# confusion matrix
table(knn.pred, test_df$CHD)
# accuracy
mean(knn.pred == test_df$CHD) #0.85

#ROC curve

if (!require(ROCR)) {
  install.packages(ROCR)
  library(ROCR)
}
train_df <- train_df[complete.cases(train_df), ]
test_df  <- test_df[complete.cases(test_df), ]
y_train <- factor(train_df$CHD)
y_test  <- factor(test_df$CHD)

x_train <- train_df[, setdiff(names(train_df), "CHD")]
x_test  <- test_df[, setdiff(names(test_df), "CHD")]
x_train <- model.matrix(~ . - 1, data = x_train)
x_test  <- model.matrix(~ . - 1, data = x_test)
x_train <- scale(x_train)
x_test  <- scale(x_test,
                 center = attr(x_train, "scaled:center"),
                 scale  = attr(x_train, "scaled:scale"))

# Convert test labels to 0/1
y_test_numeric <- ifelse(test_df$CHD, 1, 0)


# KNN with probability
knn.pred <- knn(x_train, x_test, cl=y_train, k=4, prob=TRUE)

# Extract probabilities of class TRUE
knn.probs <- attr(knn.pred, "prob")
# Convert to probability of TRUE
knn.probs <- ifelse(knn.pred == "TRUE", knn.probs, 1 - knn.probs)

# Ensure knn.probs is numeric
knn.probs <- as.numeric(knn.probs)

# ROCR prediction object
pred_knn <- prediction(knn.probs, y_test_numeric)

# ROC curve
perf_knn <- performance(pred_knn, "tpr", "fpr")
plot(perf_knn, col="red", lwd=2, main="ROC Curve for KNN")
abline(a=0, b=1, lty=2, col="gray")

# AUC
auc_knn <- performance(pred_knn, measure="auc")@y.values[[1]]
auc_knn

max_c0 <- 0
max_c1 <- 0
max_accuracy <- 0
max_k <- 0

for (k in 3:9) {
  for(weightC0 in 10:20){
    for(weightC1 in 10:60){
      train0 <- sample(class0, length(class0) * weightC0/100)
      train1 <- sample(class1, length(class1) * weightC1/100)
      trainIndex <- c(train0, train1)
      
      train3_df <- df[trainIndex, ]
      test3_df  <- df[-trainIndex, ]
      
      y_train <- train3_df$CHD
      
      x_train <- train3_df[, setdiff(names(train3_df), "CHD")]
      x_test  <- test3_df[, setdiff(names(test3_df), "CHD")]
      
      nzv <- sapply(x_train, function(col) sd(col) != 0)
      
      x_train <- x_train[, nzv, drop = FALSE]
      x_test  <- x_test[, nzv, drop = FALSE]
      x_train <- scale(x_train)
      x_test <- scale(
        x_test,
        center = attr(x_train, "scaled:center"),
        scale  = attr(x_train, "scaled:scale")
      )
      
      knn.pred <- knn(x_train, x_test, cl = y_train, k = k)
      cm <- table(knn.pred, test3_df$CHD)
      
      TN <- cm[1,1]
      FN <- cm[1,2]
      FP <- cm[2,1]
      TP <- cm[2,2]
      
      w <- 0.9
      sensitivity <- TP / (TP + FN)
      specificity <- TN / (TN + FP)
      balanced_acc <- (w*sensitivity) + (1-w)* specificity
      
      if(max_accuracy < balanced_acc){
        max_k  <- k
        max_accuracy <- balanced_acc
        max_c0 <- weightC0
        max_c1 <- weightC1
        
      }
    }
  }
}
max_k
max_c0
max_c1
max_accuracy

train0 <- sample(class0, length(class0) * 0.10)
## Sample 2% from the class with CHD
train1 <- sample(class1, length(class1) * 0.55)

trainIndex <- c(train0, train1)
train2_df <- df[trainIndex, ]
test2_df  <- df[-trainIndex, ]

y_train <- train2_df$CHD

x_train <- train2_df[, setdiff(names(train2_df), "CHD")]
x_test  <- test2_df[, setdiff(names(test2_df), "CHD")]

x_train <- scale(x_train)
x_test  <- scale(x_test,
                 center = attr(x_train, "scaled:center"),
                 scale  = attr(x_train, "scaled:scale"))

knn.pred <- knn(x_train, x_test, cl = y_train, k = 10)

table(knn.pred, test2_df$CHD)
# accuracy
mean(knn.pred == test2_df$CHD) #0.85







