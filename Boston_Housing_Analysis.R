#=================================
#       Data Pre-processing
#=================================

# Load some packages for data manipulation: 
library(tidyverse)
library(magrittr)

# Clear workspace: 
rm(list = ls())

# Use GermanCredit: 
# install.packages('caret')
# options(repos='http://cran.rstudio.com/')

library(caret)
# data("GermanCredit") %>% view()
data <- fread('C:/Users/dell/Desktop/Boston.csv')
data$chas<-as.factor(data$chas)
set.seed(123)


id <- createDataPartition(y = data$chas, p = 0.7, list = FALSE)
data_train <- data[id, ]
data_test <- data[-id, ]


# Activate h2o package for using: 
library(h2o)
h2o.init(nthreads = 16, max_mem_size = "16g")

h2o.no_progress()

# Convert to h2o Frame and identify inputs and output: 
test <- as.h2o(data_test)
train <- as.h2o(data_train)

response <- "chas"
predictors <- setdiff(names(train), response)


#===================================
#   Pure, Lasso and Ridge Logistic
#===================================

# Train Logistic Model: 
pure_logistic <- h2o.glm(family= "binomial", 
                         x = predictors, 
                         y = response, 
                         lambda = 0, 
                         # remove_collinear_columns = TRUE,
                         training_frame = train)


# h2o.varimp(pure_logistic)
# Function Shows the coefficients table: 

show_coeffs <- function(model_selected) {
  model_selected@model$coefficients_table %>% 
    as.data.frame() %>% 
    mutate_if(is.numeric, function(x) {round(x, 3)}) %>% 
    filter(coefficients != 0) %>% 
    knitr::kable()
}


# Use this function: 
show_coeffs(pure_logistic)

# Lasso Logistic Model: 

lasso_logistic <- h2o.glm(family = "binomial", 
                          alpha = 1,
                          seed = 1988, 
                          x = predictors, 
                          y = response, 
                          training_frame = train)

show_coeffs(lasso_logistic)

# Ridge Logistic Model: 
ridge_logistic <- h2o.glm(family = "binomial", 
                          alpha = 0,
                          seed = 1988, 
                          x = predictors, 
                          y = response, 
                          training_frame = train)

show_coeffs(ridge_logistic)

# Function shows model performance on test data: 

my_cm <- function(model_selected) {
  pred <- h2o.predict(model_selected, test) %>% 
    as.data.frame() %>% 
    pull(1)
  confusionMatrix(pred, data_test$chas) %>% 
    return()
}

lapply(list(pure_logistic, lasso_logistic, ridge_logistic), my_cm)

