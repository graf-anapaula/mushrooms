# Library =====
library(tidyverse)
library(tidymodels)

# Lectura de datos ====
data <- read_csv("data/mushrooms_dummy.csv") 

# Data partitioning ====
set.seed(20210709)
mush_split <- initial_split(data, prop = 0.80)
mush_train <- training(mush_split)
mush_test <- testing(mush_split)


lm_model <- linear_reg() %>% set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  fit(class ~ ., data = mush_train)

lm_form_fit %>% tidy()

importantes <- c('odor_none', 'stalk_root_club', 'stalk_surface_below_ring_scaly', 'spore_print_color_green', 'gill_spacing',
                 'bruises', 'ring_type_pendant', 'stalk_root_bulbous', 'cap_color_white', 'gill_size')

# Comprobar que las variables importantes según nuestro árbol son significativas de acuerdo a un modelo de regresión
tidy(lm_form_fit) %>% filter(term %in% importantes)

# Todas las variables con sus coeficientes
summary <- summary(lm_form_fit$fit)
variables_signif <- summary %>% tidy() %>% 
  filter(p.value <= 0.10)
# Variables significativas ====
# Menor p-value, mayor significancia en nuestro modelo
variables_signif %>% arrange(p.value) # %>% select(term) %>% pull()

# Métricas de desempeño =====
class_test <- mush_test %>% select(class) 
pred_test <- predict(lm_form_fit, new_data = mush_test %>% select(-class))
pred_test <- bind_cols(pred_test, class_test)

# Métricas de desempeño ====

pred_test %>% ggplot(aes(class, .pred)) +
  geom_point() +
  geom_abline()

ames_metrics <- metric_set(rmse, mae, rsq)
metrics_lm <- ames_metrics(pred_test, truth = class, estimate = .pred) %>% 
  mutate(method = 'lm')

# Métricas con lasso y lm ====
metrics_lasso <- read_csv("data/lasso_report.csv") %>% 
  mutate(method = 'lasso')

metrics_ridge <- read_csv("data/ridge_report.csv")

metrics_report <- full_join(metrics_lasso, metrics_lm)
metrics_report <- full_join(metrics_report, metrics_ridge)

write_csv(metrics_report, "data/metrics_report.csv")


metrics_report %>% arrange(.metric, .estimate) 

