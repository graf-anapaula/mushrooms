# Librerias =====
library(tidymodels)
library(tidyverse)

# Lectura de datos =====
data <- read_csv("data/mushrooms_dummy.csv")

# Initial split ====
set.seed(20210709)
mush_split <- initial_split(data, prop = 0.8)
mush_train <- training(mush_split)
mush_test <- testing(mush_split)

# Training model =====
recipe <- recipe(class ~ ., data = mush_train) %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

prep <- recipe %>%
  prep(strings_as_factors = FALSE)

ridge <- linear_reg(penalty = .10, mixture = 0) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(recipe)

ridge_fit <- wf %>%
  add_model(ridge) %>%
  fit(data = mush_train)

ridge_fit %>%
  pull_workflow_fit() %>%
  tidy () %>% 
  filter(abs(estimate) > 0)
summary(ridge_fit$fit)



# Métricas ====
class_test <- mush_test %>% select(class) 
pred_test <- predict(ridge_fit, new_data = mush_test %>% select(-class))
pred_test <- bind_cols(pred_test, class_test)


ames_metrics <- metric_set(rmse, rsq, mae)
metrics <- ames_metrics(pred_test, truth = class, estimate = .pred) %>% 
  mutate(method = 'ridge')

write_csv(metrics, "data/ridge_report.csv")



