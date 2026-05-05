rm(list=ls()) 
# Load Dataset ------------------------------------------------------------
{
    library(ISLR2)
    library(glmnet)
    library(randomForest)
    library(gbm)
    library(tree)
}
{
    
}
{
    library(readr)
    dataf <- read_delim("db.txt",delim = "\t", escape_double = FALSE, trim_ws = TRUE)
}


# Data Preprocessing ------------------------------------------------------

# Data Exploration --------------------------------------------------------
colSums(is.na(dataf)) #0

summary(dataf)

sapply(dataf, sd, na.rm = TRUE)


# Correlation matrix
cor_matrix <- cor(dataf, use = "complete.obs")
cor_matrix
# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.7)
# Focus su variabili correlate con porogr (BMI BP, HDL, TG)


# Histogram for each numeric variable
library(tidyverse)
dataf %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = Value)) +
    geom_histogram(bins = 20, fill = "skyblue") +
    facet_wrap(~ Variable, scales = "free") +
    theme_minimal() +
    labs(title = "Distributions of Numeric Variables")


# Boxplots for numeric variables
dataf %>%
    select(where(is.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "lightblue") +
    theme_minimal() +
    labs(title = "Boxplots for Numeric Variables")

# Data Split --------------------------------------------------------------
{
    set.seed(1)
    
    train_idx <- sample(seq_len(nrow(dataf)), size = 0.5 * nrow(dataf))
    
    train_df <- dataf[train_idx, ]
    test_df  <- dataf[-train_idx, ]
    
    train_df$progr <- as.numeric(train_df$progr)
    test_df$progr <- as.numeric(test_df$progr)
}


# Functions ---------------------------------------------------------------
compute_mse <- function(preds, truth) {
    mean((preds - truth)^2)
}


