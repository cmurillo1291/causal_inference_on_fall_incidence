# --------------------------------------------------
# Variable importance (permutation-based). 
# For each variable:
#
# Permutation importance algorithm
#
# Step 1: Obtain predictions from the original data.
#
# Step 2: Permute one variable while keeping all
#         other variables unchanged.
#
# Step 3: Generate predictions using the perturbed
#         dataset.
#
# Step 4: Measure importance as the mean absolute
#         change in predictions:
#         mean(abs(perturbed prediction -
#                  original prediction))
#
# Step 5: Repeat for all variables.
#
# Step 6: Repeat within each cross-fitting fold.
#
# Step 7: Average fold-specific importance values
#         and normalize them relative to the most
#         important variable.
#
# Interpretation:
# Larger values indicate that permuting the variable
# causes greater changes in model predictions,
# suggesting a larger contribution to predictive
# performance.
# 
# Perturbation function - shuffling
# --------------------------------------------------
perturb_variable <- function(data, var) {
  data_new <- data
  data_new[[var]] <- sample(data[[var]])
  data_new
}

# SuperLearner prediction function to get predictions
# --------------------------------------------------
sl_pred <- function(model, data) {
  predict(model, newdata = data)$pred
}

#  permutation importance function
# --------------------------------------------------
compute_importance <- function(model, data, vars, pred_fun) {
  #For each variable:
  base_pred <- pred_fun(model, data) #1.Get original predictions
  
  res <- data.frame(
    variable = vars,
    importance = NA_real_)
  
  # 11 variables so 11 times 
  for (v in vars) {
    df_perturbed <- perturb_variable(data, v) # 2. permute the variable
    pert_pred <- pred_fun(model, df_perturbed) # 3. Generate new predictions in perturbated data
    # 4.Importance = average absolute change in predictions caused by permuting variable v
    res$importance[res$variable == v] <-
      mean(abs(pert_pred - base_pred), na.rm = TRUE)
  } # 5. repeat for all variables
  res 
}

# Propensity score model importance
# --------------------------------------------------
# permutation importance for each cross-fitted propensity score SuperLearner
imp_list_g <- vector("list", n_folds) # importance table
# Repeat for all variables.
for (k in seq_len(n_folds)) {
  
  imp_list_g[[k]] <- compute_importance(
    model = g_models[[k]], # SuperLearner (ensemble) propensity score model fitted for fold k
    data = W[folds[[k]], , drop = FALSE], # Select the observations from fold k
    vars = names(W), # covariates whose importance is evaluated
    pred_fun = sl_pred) %>% #prediction function for SuperLearner
    mutate(fold = k)
}

# 7. averages the permutation importance of each variable across the cross-fitting folds 
# and then rescales all values so that the most important variable has importance = 1.
importance_ps <- bind_rows(imp_list_g) %>%
  group_by(variable) %>% summarise(
    importance = mean(importance),
    .groups = "drop") %>% 
  mutate(importance = importance / max(importance)) %>%
  arrange(desc(importance))

#print(importance_ps)

# Outcome model importance
# same application than for the propensity score model
# --------------------------------------------------
imp_list_Q <- vector("list", n_folds)

for (k in seq_len(n_folds)) {
  
  outcome_data <- data.frame(
    A = A[folds[[k]]],
    W[folds[[k]], , drop = FALSE])
  
  imp_list_Q[[k]] <- compute_importance(
    model = Q_models[[k]],
    data = outcome_data,
    vars = names(W),
    pred_fun = sl_pred) %>%
    mutate(fold = k)
}

importance_out <- bind_rows(imp_list_Q) %>%
  group_by(variable) %>%
  summarise(
    importance = mean(importance),
    .groups = "drop") %>%
  mutate(importance = importance / max(importance)) %>%
  arrange(desc(importance))

#print(importance_ps)

# combine
tab <- data.frame(
  variable = importance_out$variable,
  outcome_model = importance_out$importance,
  propensity_model = importance_ps$importance[
    match(
      importance_out$variable,
      importance_ps$variable
    )
  ]
)

tab <- tab %>% arrange(desc(outcome_model))
tab[, 2:3] <- round(tab[, 2:3], 3)

kable(tab,caption = "Permutation-based variable importance using cross-fitted SuperLearner predictions")

