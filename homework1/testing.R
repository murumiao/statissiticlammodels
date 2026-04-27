#Load data
library(readr)
chd <- read_csv("chd.csv")


#prepare data
#chd$sex      <- as.numeric(chd$sex == "Male")
#chd$CHD      <- as.factor(chd$CHD)
#chd$smoker   <- as.factor(chd$smoker)
#chd$stroke   <- as.factor(chd$stroke)
#chd$HTN      <- as.factor(chd$HTN)
#chd$diabetes <- as.factor(chd$diabetes)

chd$CHD <- chd$CHD == "Yes"
chd$sex <- as.numeric(chd$sex == "Male")

View(chd)
summary(chd)

# separate valuesrows with NAs in them
ignore_na <- c("cpd", "HTN")
cols_check <- setdiff(names(chd), ignore_na)
chd_na <- chd[!complete.cases(chd[, cols_check]), ]
# rows that are complete in all required columns
df <- chd[complete.cases(chd[, cols_check]), ]

View(df)
df$sexIsMale <- df$sex == "Male"
cor(df)

ggplot(df, aes(x = CHD)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Class Distribution", x = "CHD (0 = No, 1 = Yes)", y = "Count")


library(naniar)
gg_miss_var(chd) + 
  labs(title = "Missing Values by Variable", x = "Variables", y = "Number of Missing Values")

library('tidyr')
library('dplyr')
df %>%
  ggplot(aes(x = "chol", y = chol)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "", y = "")

summary(df$chol)


# ROC
#FOR logistic regression
library(pROC)

# Fit model
logit <- glm(CHD ~ ., data=df, family=binomial)

# Predict probabilities
prob <- predict(logit, type="response")

# ROC
roc_logit <- roc(df$CHD, prob)

# Plot
plot(roc_logit, col="blue")
auc(roc_logit)















logit <- glm(CHD ~ ., data = df, family = binomial)

# Predicted probabilities
prob <- predict(logit, type = "response")

# ROCR objects
pred <- prediction(prob, df$CHD)
perf <- performance(pred, "tpr", "fpr")

# Plot ROC curve
plot(perf, col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# AUC
auc <- performance(pred, "auc")@y.values[[1]]
auc




