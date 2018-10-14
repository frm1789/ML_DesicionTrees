library(gmodels)
library(readr)
library(dplyr)
library(class)
library(C50)

# Step 1: Get the data
require(caret)
data(GermanCredit)
str(GermanCredit)

# Step 2: Exploring and preparing the data
summary(GermanCredit$Duration)
# Min.      1st Qu.     Median    Mean      3rd Qu.     Max. 
# 4.0       12.0        18.0      20.9      24.0        72.0 

# Step 3: Creating a new variable default
credit <- GermanCredit
credit$default <- sample(0:1, size=1000, replace= TRUE, prob=c(.70,.30))
credit$default <- as.factor(credit$default)
prop.table(table(credit$default))
  
# Creating Random test
set.seed(123)
train_sample <- sample(1000,900)
str(train_sample)

credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_test$default))
prop.table(table(credit_train$default))

# Step 3: Training a model on the data
credit_model <- C5.0(credit_train[,-63], credit_train$default, trials = 1, rules = FALSE)

summary(credit_model)


credit_model
# Call:
#   C5.0.default(x = credit_train[, -63], y = credit_train$default, trials = 1, rules = FALSE)
# 
# Classification Tree
# Number of samples: 900 
# Number of predictors: 62 
# 
# Tree size: 124 
# 
# Non-standard options: attempt to group attributes


# Step 4: Analyzing the model
credit_pred <- predict(credit_model,credit_test)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default','predict default'))

  #        Cell Contents
  # Total Observations in Table:  100 
  # 
  #                | predict default 
  # actual default |         0 |         1 | Row Total | 
  # ---------------|-----------|-----------|-----------|
#                0 |        52 |        18 |        70 | 
#                  |     0.030 |     0.330 |           | 
#   ---------------|-----------|-----------|-----------|
#                1 |        26 |         4 |        30 | 
#                  |     0.180 |     0.460 |           | 
#   ---------------|-----------|-----------|-----------|
#     Column Total |        78 |        22 |       100 | 
#   ---------------|-----------|-----------|-----------|
  
summary(credit_model)

#Step 5: Improving using boosters
credit_boost10 <- C5.0(x = credit_train[, -63], y = credit_train$default, trials = 10)

# Step 6: Analyzing the improved model
summary(credit_boost10)

# Evaluation on training data (900 cases):
#   
#   (a)   (b)    <-classified as
# ----    ----
#   627          (a): class 0
#         273    (b): class 1
# 
# De acuerdo a lo observado, en la tabla no existen errores por parte  del classificador

credit_boots_pred10 <- predict(credit_boost10,credit_test)

library(gmodels)
CrossTable(credit_test$default, credit_boots_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default','predict default'))

#   Cell Contents
#   Total Observations in Table:  100 
# 
#                  | predict       default 
#   actual default |         0 |         1 | Row Total | 
#   ---------------|-----------|-----------|-----------|
#                0 |        56 |        14 |        70 | 
#                  |     0.560 |     0.140 |           | 
#   ---------------|-----------|-----------|-----------|
#                1 |        24 |         6 |        30 | 
#                  |     0.240 |     0.060 |           | 
#   ---------------|-----------|-----------|-----------|
#     Column Total |        80 |        20 |       100 | 
#   ---------------|-----------|-----------|-----------|
  
#Step 7: Improving using matrix
matrix_dimensions <- list(c("0","1"),c("0","1"))
names(matrix_dimensions) <- c("predicted", "actual")
error_cost <- matrix(c(0,1,4,0),nrow =2, dimnames = matrix_dimensions)

credit_cost <- C5.0(credit_train[,-63], credit_train$default, costs = error_cost )

credit_cost_pred <- predict(credit_cost, credit_test)

# Step 8: Analyzing the improved model
CrossTable(credit_test$default, credit_cost_pred, prop.r = FALSE,
           dnn = c('actual default','predict default'))

#   Cell Contents
#   |-------------------------|
#   |                       N |
#   | Chi-square contribution |
#   |           N / Col Total |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  100 
# 
# 
#                  | predict default 
#   actual default |         0 |         1 | Row Total | 
#   ---------------|-----------|-----------|-----------|
#                0 |        22 |        48 |        70 | 
#                  |     0.004 |     0.002 |           | 
#                  |     0.710 |     0.696 |           | 
#                  |     0.220 |     0.480 |           | 
#   ---------------|-----------|-----------|-----------|
#                1 |         9 |        21 |        30 | 
#                  |     0.010 |     0.004 |           | 
#                  |     0.290 |     0.304 |           | 
#                  |     0.090 |     0.210 |           | 
#   ---------------|-----------|-----------|-----------|
#     Column Total |        31 |        69 |       100 | 
#                  |     0.310 |     0.690 |           | 
#   ---------------|-----------|-----------|-----------|
  
  



