# Installere pakker
pacman::p_load(tidyverse, tidymodels, forcats, vip)

# Arbejdsmappe
setwd("C:/Users/andre/OneDrive - EaDania/eksamen_sem2/2_semesters_eksamensopgave2025_kode")

# Indlæs det rensede datasæt
master_final <- read_csv("Bilag2_master_final_shinyapp.csv")

# Reorder churnet-factor, så modellen "vender" rigtig
master_final <- master_final %>%
  mutate(
    churnet = factor(churnet, levels = c("yes","no"))
  )
names(master_final)

# Split i trænings og test
set.seed(123)
data_split <- initial_split(master_final, prop = .75, strata = churnet)
train_data <- training(data_split)
test_data <- testing(data_split)

# 10-fold CV
cv_folds <- vfold_cv(train_data, v = 10, strata = churnet)

# Pre-processing recipe - fokuseret på logistisk regression
churn_rec <- recipe(churnet ~ ., data = train_data) %>%
  update_role(`CVR-nr.`, MedlemsRegistreringsDato, new_role = "ID") %>%
  step_rm(starts_with("Gratis_Medlem")) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.05, other = "other") %>%
  step_novel(all_nominal_predictors(), new_level = "novel") %>%
  step_dummy(all_nominal_predictors()) %>%           # kun d−1 dummies
  step_zv(all_predictors()) %>%                      # fjern 0-variance
  step_nzv(all_predictors()) %>%                     # fjern near-0-variance
  step_lincomb(all_predictors()) %>%   # fjern perfekt kombination
  step_normalize(all_numeric_predictors())

# Definer logistisk regressionsmodel med en lille ridge-straffe, som ændre en smule på koeffienterne, men tilgengæld lammer de problematiske ret uidentificerebare kombinatoriske retninger
log_mod <- logistic_reg(
  mode    = "classification",
  penalty = 1e-4,    # lille ridge-straf
  mixture = 0        # 0 = rent ridge, 1 = rent lasso, mellem = elastic net
) %>%
  set_engine("glmnet")

# Opret workflow
wf_log <- workflow() %>% 
  add_recipe(churn_rec) %>% 
  add_model(log_mod)

# Definer metrikker (hvis ikke defineret andetsteds)
my_metrics <- metric_set(accuracy, precision, recall, f_meas, roc_auc, sensitivity, specificity)

# Tilpas modellen med k-fold krydsvalidering
log_cv_results <- wf_log %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = my_metrics
  )

# Sidste fit på træning/test-split
log_fit <- last_fit(wf_log, data_split, metrics = my_metrics)

# Saml performance-metrikker
log_metrics <- log_fit %>% 
  collect_metrics() %>% 
  select(.metric, .estimate)
print(log_metrics)

# Træn endelig model på ALT træningsdata
log_final_model <- fit(wf_log, data = train_data)

# Forudsig på testdata
log_pred <- log_fit %>%
  collect_predictions()

# Beregn og visualisér confusion matrix
log_pred %>% 
  conf_mat(churnet, .pred_class) %>%
  autoplot(type = "heatmap") + 
  ggtitle("Logistic Regression Confusion Matrix")

# Visualisér variable importance
glm_engine_fit <- extract_fit_parsnip(log_final_model)$fit
vip(glm_engine_fit, num_features = 20, geom = "col") +
  ggtitle("Logistic Regression: Absolutte koefficienter") +
  ylab("|β|") +
  theme_minimal()

# ROC-kurve
log_pred %>%
  roc_curve(churnet, .pred_yes) %>%
  autoplot() +
  ggtitle("ROC-kurve for Logistisk Regression")

