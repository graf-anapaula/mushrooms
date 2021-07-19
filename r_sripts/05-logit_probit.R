# Librerías ====
library(tidyverse)
library(tidymodels)
library(lmtest)

# lectura de datos ====
mushrooms <- read_csv("data/mushrooms_dummy.csv")

# split data ====
set.seed(20210709)
split <- initial_split(mushrooms, split  = 0.8)
train <- training(split)
test <- testing(split)

probit <- glm(data = train,
              class ~ ., 
              family = binomial(link = "probit"))
summary(probit)

logit <- glm(data = train,
              class ~ ., 
              family = "binomial")
summary(logit)
class_test <- test %>% select(class) 
pred_test_probit <- predict(probit, new_data = test %>% select(-class))
pred_test_probit <- bind_cols(pred_test_probit, class_test)










