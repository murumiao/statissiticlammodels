library(tidyverse);
set.seed(1);

View(chd);
summary(chd);

#change chd$CHD to 0/1 instead of "Yes" and "No". COuld do the same with other bool-like cols
chd$CHD <- chd$CHD == "Yes"

chd$smoker <- chd$smoker == 1
chd$stroke <- chd$stroke == 1
chd$HTN <- chd$HTN == 1
chd$diabetes <- chd$diabetes == 1

#change sex to being numerical
chd$sex <- as.numeric(chd$sex == "Male")

sum(chd$CHD == TRUE) #TRUE = 644, FALSE = 3594
sum(is.na(chd));
644/(3594+644)

# separate valuesrows with NAs in them
chd_na  <- chd[!complete.cases(chd),]
df <- chd[complete.cases(chd), ]


#Lets see correlation
cor(df)
#cpd-smoker=0.77, HTN-DBP=0.61
plot(df$cpd, df$smoker);
plot(df$DBP, df$HTN);


#SPLIT DATA (i.e. 50/50)

train <- sample(seq_len(nrow(df)), size=floor(0.5*nrow(df)))
summary(train)
train_df <- df[train,]
test_df <- df[-train,]
summary(train_df$CHD)
summary(test_df$CHD)

# LOGISTIC REGRESSION
#goal: predict CHD based on every other field

glm.fits <- glm(CHD ~ sex+age+education+smoker+cpd+stroke+HTN+diabetes+chol+DBP+BMI+HR,
                data=train_df, family = binomial)

summary(glm.fits)
plot(glm.fits)
coef(glm.fits)

#see predictions
glm.probs <- predict(glm.fits, test_df, type="response" ) #Get predicted probability of guess

threshold = 0.5
glm.pred  <- rep(FALSE, nrow(test_df))
glm.pred[glm.probs > threshold] <- TRUE
summary(glm.pred)

# confusion matrix
#| tidy: false
table(glm.pred, test_df$CHD)
#Accuracy: 85%
mean(glm.pred == test_df$CHD)
#Error: 15%
mean(glm.pred != test_df$CHD)

#if we change threshold to 0.2
threshold = 0.2
glm.pred  <- rep(FALSE, nrow(test_df))
glm.pred[glm.probs > threshold] <- TRUE# confusion matrix
#| tidy: false
table(glm.pred, test_df$CHD)
#Accuracy: 73%
mean(glm.pred == test_df$CHD)
#Error: 26%
mean(glm.pred != test_df$CHD)

# better than "majority-class" guesser? No.
mean(test_df$CHD == FALSE) # Accuracy = 85.2%
mean(test_df$CHD != FALSE) # Error = 14.7%
#Fix: better data split?
library(tidymodels)
#df$CHD <- ifelse(df$CHD, 1,0)
#split_chd = initial_split(df, prop=0.5, strata=df$CHD)
#train2_df = training(split_chd)
#test2_df = testing(split_chd)
#summary(train2_df$CHD)
#summary(test2_df$CHD)

# Split indices by class
class0 <- which(df$CHD == FALSE)
class1 <- which(df$CHD == TRUE)

# Sample 70% from each class
train0 <- sample(class0, length(class0) * 0.7)
train1 <- sample(class1, length(class1) * 0.7)

trainIndex <- c(train0, train1)

train2_df <- df[trainIndex, ]
test2_df  <- df[-trainIndex, ]


glm.fits <- glm(CHD ~ sex+age+education+smoker+cpd+stroke+HTN+diabetes+chol+DBP+BMI+HR,
                data=train2_df, family = binomial)
summary(glm.fits)


summary(glm.fits)
glm.probs <- predict(glm.fits, test2_df, type="response" )
glm.pred  <- rep(FALSE, nrow(test2_df))
glm.pred[glm.probs > 0.2] <- TRUE
summary(glm.pred)

table(glm.pred, test2_df$CHD)
#Accuracy: 73%
mean(glm.pred == test2_df$CHD)
#Error: 27%
mean(glm.pred != test2_df$CHD)



#What if we remove correlated predictors? (they will increase variance )
#correlations: cpd-smoker=0.77, HTN-DBP=0.61, so re remove one of the pairs

glm.fits <- glm(CHD ~ sex+age+education+smoker+stroke+diabetes+chol+BMI+HR+DBP,
                data=train2_df, family = binomial)
summary(glm.fits)
glm.probs <- predict(glm.fits, test2_df, type="response" )
glm.pred  <- rep(FALSE, nrow(test2_df))
glm.pred[glm.probs > 0.2] <- TRUE
#Accuracy: 73%
mean(glm.pred == test2_df$CHD)
#Error: 27%
mean(glm.pred != test2_df$CHD)


# sample more the "Yes" Class

# Sample 70% from each class
max_c0 <- 0
max_c1 <- 0
max_accuracy <- 0
for(weightC0 in 1:100){
  weightC0
  for(weightC1 in 1:100){
  train0 <- sample(class0, length(class0) * weightC0/100)
  train1 <- sample(class1, length(class1) * weightC1/100)
  trainIndex <- c(train0, train1)
  
  train3_df <- df[trainIndex, ]
  test3_df  <- df[-trainIndex, ]
  
  
  glm.fits <- glm(CHD ~ sex+age+education+smoker+cpd+stroke+HTN+diabetes+chol+DBP+BMI+HR,
                  data=train3_df, family = binomial)
  
  glm.probs <- predict(glm.fits, test3_df, type="response" )
  glm.pred  <- rep(FALSE, nrow(test3_df))
  glm.pred[glm.probs > 0.2] <- TRUE
  
  if(max_accuracy < mean(glm.pred == test3_df$CHD)){
      #Accuracy: 73%
      mean(glm.pred == test3_df$CHD)
      #Error: 27%
      #mean(glm.pred != test3_df$CHD)
      max_accuracy <- mean(glm.pred == test3_df$CHD)
      max_c0 <- weightC0
      max_c1 <- weightC1
      
    }
  }
}
max_accuracy
max_c0 # 8
max_c1 # 2
train0 <- sample(class0, length(class0) * 0.08)
train1 <- sample(class1, length(class1) * 0.02)

trainIndex <- c(train0, train1)

train4_df <- df[trainIndex, ]
test4_df  <- df[-trainIndex, ]

glm.fits <- glm(CHD ~ sex+age+education+smoker+cpd+stroke+HTN+diabetes+chol+DBP+BMI+HR,
                data=train4_df, family = binomial)
glm.probs <- predict(glm.fits, test4_df, type="response" )
glm.pred  <- rep(FALSE, nrow(test4_df))
glm.pred[glm.probs > 0.2] <- TRUE

table(glm.pred, test4_df$CHD)
#Accuracy: 83%
mean(glm.pred == test4_df$CHD)

#What if we remove correlated predictors? (they will increase variance )
#correlations: cpd-smoker=0.77, HTN-DBP=0.61, so re remove one of the pairs

glm.fits <- glm(CHD ~ sex+age+education+smoker+stroke+diabetes+chol+BMI+HR+DBP,
                data=train4_df, family = binomial)
glm.probs <- predict(glm.fits, test4_df, type="response" )
glm.pred  <- rep(FALSE, nrow(test4_df))
glm.pred[glm.probs > 0.2] <- TRUE
#Accuracy: 83%
mean(glm.pred == test4_df$CHD)
#Error: 17%
mean(glm.pred != test4_df$CHD)

