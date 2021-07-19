# Library ====
library(tidymodels)

# Lectura de datos ====
datos <- read_csv("data/mushrooms_dummy.csv")

# Initial split ====
set.seed(20210709)
mush_split <- initial_split(datos, prop = 0.8)
mush_train <- training(mush_split)
mush_test <- testing(mush_split)

# Training model =====
recipe <- recipe(class ~ ., data = mush_train) %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

prep <- recipe %>%
  prep(strings_as_factors = FALSE)

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(recipe)

lasso_fit <- wf %>%
  add_model(lasso_spec) %>%
  fit(data = mush_train)

lasso_fit %>%
  pull_workflow_fit() %>%
  tidy() 

significativas <- lasso_fit %>% tidy() %>% arrange(desc(abs(estimate))) %>% head(10) 


class_test <- mush_test %>% select(class) 
pred_test <- predict(lasso_fit, new_data = mush_test %>% select(-class))
pred_test <- bind_cols(pred_test, class_test)

# Métricas de desempeño ====

pred_test %>% ggplot(aes(class, .pred)) +
  geom_point() +
  geom_abline()

ames_metrics <- metric_set(rmse, rsq, mae)
metrics <- ames_metrics(pred_test, truth = class, estimate = .pred) 
importantes <- c('odor_none', 'stalk_root_club', 'stalk_surface_below_ring_scaly', 'spore_print_color_green', 'gill_spacing',
                 'bruises', 'ring_type_pendant', 'stalk_root_bulbous', 'cap_color_white', 'gill_size')


write_csv(metrics, "data/lasso_report.csv")















