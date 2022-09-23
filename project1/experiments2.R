library(tidyverse)
library(tidymodels)

ames_train <- read.csv("train_1.csv")
ames_test <- read.csv("test_1.csv")
ames_y <- read.csv("test_y_1.csv")

ames_train <- mutate(ames_train, Sale_Price = log10(Sale_Price))

ames_rec <-
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type, 
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~ Gr_Liv_Area:starts_with("Bldg_Type"))

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

ames_test_res <- predict(lm_fit, new_data = ames_test)

ames_test_res <-
  bind_cols(ames_test %>% select(PID), ames_test_res) %>%
  rename(Sale_Price = .pred) %>% mutate(Sale_Price = 10 ^ Sale_Price)


sqrt(mean((log(ames_y$Sale_Price) - log(ames_test_res$Sale_Price))^2))

