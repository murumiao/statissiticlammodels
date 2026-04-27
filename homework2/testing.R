# Load Dataset ------------------------------------------------------------
{
    library(tidyverse)
    library(ISLR2)
    library(glmnet)
    library(readr)
    library(tree)
}
{
    dataf <<- read_delim("db.txt",delim = "\t", escape_double = FALSE, trim_ws = TRUE )
}
#View(dataf)
head(dataf)


# Data Preprocessing ------------------------------------------------------


# Data Exploration --------------------------------------------------------

cor(dataf)

library('corrplot')
cor_matrix <- cor(dataf)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, mar = c(0, 0, 1, 0), tl.cex = 0.7)
# Nota: HDL (TCH)con tutto, CD-LDL


# Data Split --------------------------------------------------------------
{
    set.seed(1)
    
    x <- model.matrix(progr ~ ., data=dataf)[, -12] # trim off the last column (i.e., keep only the predictors)
    y <- dataf$progr
    train <- sample(1:nrow(x), nrow(x)/2)
    test <- -train # numerical indexes, so we complement with -
    y_test <- y[test]
    
    x_train <<- x[train, ]
    y_train <<- y[train]
    
    x_test <<- x[test, ]
}

