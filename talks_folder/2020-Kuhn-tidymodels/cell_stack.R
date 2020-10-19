# remotes::install_github("tidymodels/tune")
# remotes::install_github("tidymodels/stacks")
# remotes::install_github("tidymodels/rules")
# remotes::install_github("tidymodels/discrim")
library(tidymodels)
library(stacks)
library(rules)
library(discrim)
library(doMC)
registerDoMC(cores = 10)

## -----------------------------------------------------------------------------

data(cells)
cells <- cells %>% select(-case)

## -----------------------------------------------------------------------------

set.seed(4595)
data_split <- initial_split(cells, strata = "class")
cell_train <- training(data_split)
cell_test  <- testing(data_split)

set.seed(2453)
folds <- vfold_cv(cell_train) 

ctrl <- control_grid(save_workflow = TRUE, save_pred = TRUE)

perf_metrics <- metric_set(roc_auc)

## -----------------------------------------------------------------------------

glmnet_recipe <- 
  recipe(formula = class ~ ., data = cell_train) %>% 
  step_normalize(all_predictors(), -all_nominal()) 

glmnet_spec <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 

glmnet_workflow <- 
  workflow() %>% 
  add_recipe(glmnet_recipe) %>% 
  add_model(glmnet_spec) 

glmnet_grid <-
  tidyr::crossing(
    penalty = 10 ^ seq(-6,-1, length.out = 20),
    mixture = c(0.05, 0.2, 0.4, 0.6, 0.8, 1)
  ) 

glmnet_tune <-
  tune_grid(
    glmnet_workflow,
    resamples = folds,
    grid = glmnet_grid,
    metrics = perf_metrics,
    control = ctrl
  ) 

## -----------------------------------------------------------------------------

ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

set.seed(41063)
ranger_tune <-
  ranger_spec %>% 
  tune_grid(
    class ~ .,
    resamples = folds,
    grid = 25,
    metrics = perf_metrics,
    control = ctrl
  )

## -----------------------------------------------------------------------------

c5_spec <-
  C5_rules(trees = tune(), min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("C5.0")

c5_grid <-
  tidyr::crossing(trees = 1:50, min_n = c(1, 5, 10))

set.seed(3159)
c5_tune <-
  c5_spec %>% 
  tune_grid(
    class ~ .,
    resamples = folds,
    grid = c5_grid,
    metrics = perf_metrics,
    control = ctrl
  )

## -----------------------------------------------------------------------------

fda_spec <-
  discrim_flexible(
    num_terms = tune(),
    prod_degree = tune(),
    prune_method = "none"
  ) %>%
  set_engine("earth")

fda_grid <-
  tidyr::crossing(num_terms = 2:20, prod_degree = 1:2)

fda_tune <-
  fda_spec %>%
  tune_grid(
    class ~ .,
    resamples = folds,
    grid = fda_grid,
    metrics = perf_metrics,
    control = ctrl
  ) 


## -----------------------------------------------------------------------------

nb_recipe <-
  recipe(formula = class ~ ., data = cell_train) %>%
  step_YeoJohnson(all_predictors())

nb_spec <-
  naive_Bayes(smoothness = tune()) %>%
  set_mode("classification") %>%
  set_engine("klaR")

nb_workflow <-
  workflow() %>%
  add_recipe(nb_recipe) %>%
  add_model(nb_spec)

set.seed(21575)
nb_tune <-
  tune_grid(
    nb_workflow,
    resamples = folds,
    grid = 10,
    metrics = perf_metrics,
    control = control_grid(save_workflow = TRUE, save_pred = TRUE)
  )

## -----------------------------------------------------------------------------

svm_recipe <- 
  recipe(formula = class ~ ., data = cell_train) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  step_normalize(all_predictors(), -all_nominal()) 

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab") 

svm_workflow <- 
  workflow() %>% 
  add_recipe(svm_recipe) %>% 
  add_model(svm_spec) 

svm_tune <-
  tune_grid(
    svm_workflow,
    resamples = folds,
    grid = 25,
    metrics = perf_metrics,
    control = ctrl
  ) 

## -----------------------------------------------------------------------------

save(c5_tune, fda_tune, glmnet_tune, nb_tune, ranger_tune, svm_tune,
     file = "cell_models.RData", version = 2, compress = "xz")

## -----------------------------------------------------------------------------


mod_summary <- function(x) {
  perf <- collect_metrics(x)
  p <- nrow(perf)
  wflow <- .get_tune_workflow(x)
  mod <- pull_workflow_spec(wflow) %>% class() %>% pluck(1)
  perf %>% 
    arrange(desc(mean)) %>% 
    dplyr::slice(1) %>% 
    dplyr::rename(`ROC AUC` = mean) %>% 
    mutate(Model = mod, `Sub-Models` = p) %>% 
    dplyr::select(Model, `Largest ROC AUC`, `Sub-Models`)
}

bind_rows(
  mod_summary(c5_tune),
  mod_summary(glmnet_tune),
  mod_summary(ranger_tune),
  mod_summary(fda_tune),
  mod_summary(nb_tune),
  mod_summary(svm_tune)
) %>% 
  arrange(Model)

## -----------------------------------------------------------------------------

predictions <- 
  stacks() %>% 
  add_candidates(c5_tune) %>%
  add_candidates(glmnet_tune) %>%
  add_candidates(ranger_tune) %>%
  add_candidates(fda_tune) %>%
  add_candidates(nb_tune) %>% 
  add_candidates(svm_tune)

ens_model <- blend_predictions(predictions, verbose = TRUE)


autoplot(ens_model)


