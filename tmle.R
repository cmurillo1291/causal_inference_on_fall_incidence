# 2.11.	Statistical framework: Targeted Maximum Likelihood Estimation
# ---------------------------------------------------------------------------------
library(SuperLearner)
library(survey)
library(dplyr)
library(knitr)
library(tmle)
set.seed(123)

# Variable definition
Y <- his_subset$Y
A <- his_subset$A
confounders <- names(his_subset[,3:13])
W <- his_subset%>% select(confounders)

# Survey design features
province<-his_subset$province
household<-his_subset$household
weights<-his_subset$weight
# ---------------------------------------------------------------------------------
# Cross-fitting folds parameters
N <- nrow(his_subset)
n_folds <- 10 # ref: Practical considerations for specifying a superlearner
fold_id <- sample(rep(1:n_folds, length.out = N))
folds   <- split(seq_len(N), fold_id)

# learners library
SL.glmnet.lasso <- function(..., alpha = 1) {
  SL.glmnet(..., alpha = alpha)
}

SL.glmnet.ridge <- function(..., alpha = 0) {
  SL.glmnet(..., alpha = alpha)
}

SL.library <- c(
  "SL.glm",
  "SL.glmnet.lasso", 
  "SL.glmnet.ridge",
  "SL.ranger",      # random forest
  "SL.xgboost",     # gradient boosting
  "tmle.SL.dbarts.k.5" # BART: Bayesian additive regression trees       
)
# ---------------------------------------------------------------------------------
##  Step 1: Nuisance parameters estimation via Super Learner with cross-fitting
Q_models  <- vector("list", n_folds) # for variable importance
g_models  <- vector("list", n_folds) # for variable importance

Q1 <- numeric(N)
Q0 <- numeric(N)
pi_hat <- numeric(N)

for (k in 1:n_folds) {
  
  test  <- folds[[k]]
  train <- setdiff(seq_len(N), test)
  
  # 1. Outcome model Q(A, W)
  sl_Q <- SuperLearner(
    Y = Y[train],
    X = data.frame(A = A[train], W[train, , drop = FALSE]),
    SL.library = SL.library,
    family = gaussian(),
    method = "method.NNLS",
    cvControl = list(V = 10)
  )
  Q_models[[k]] <- sl_Q # variable importance
  
  Q1[test] <- predict(sl_Q,
                      newdata = data.frame(A = 1, W[test, , drop = FALSE])
  )$pred
  
  Q0[test] <- predict(sl_Q,
                      newdata = data.frame(A = 0, W[test, , drop = FALSE])
  )$pred
  
  # 2. Propensity score model g(W)
  sl_pi <- SuperLearner(
    Y = A[train],
    X = W[train, , drop = FALSE],
    SL.library = SL.library,
    family = binomial(),
    method = "method.NNLS",
    cvControl = list(V = 10)
  )
  g_models[[k]] <- sl_pi # variable importance
  
  pi_hat[test] <- predict(
    sl_pi,
    newdata = W[test, , drop = FALSE]
  )$pred
}

# ---------------------------------------------------------------------------------
# Cross-validated performance metrics
cv_table <- data.frame(
  Algorithm = names(sl_Q$cvRisk),
  MSE_Q = round(as.numeric(sl_Q$cvRisk) ,3),
  Neg_log_like_g = round(as.numeric(sl_pi$cvRisk[names(sl_Q$cvRisk)]), 3)
)
kable(cv_table, caption = "Cross-validated predictive performance")

cv_table2 <- data.frame(
  Algorithm = names(sl_Q$coef),
  Weight_Q = round(as.numeric(sl_Q$coef), 3),
  Weight_g = round(as.numeric(sl_pi$coef[names(sl_Q$coef)]), 3)
)

kable(cv_table2, caption = "weight candidate algorithms in the final stacked model")
# ---------------------------------------------------------------------------------
## Step 2: Targeting step
# clever covariate
H1 <- A / pi_hat
H0 <- (1 - A) / (1 - pi_hat)

df <- data.frame(
  Q1 = Q1,
  H1 = H1,
  Q0 = Q0,
  H0 = H0,
  Y = Y,
  province = province,
  weights = weights,
  household = household
)

# Survey design object
dsgn <- svydesign(
  strata = province,
  ids = ~household,
  weights = ~weights, 
  data = df 
)

# fluctuation models
# closed form as a ratio of survey-weighted moments 
# delta1 <- svymean(H1*(Y-Q1), dsgn)/svymean(H1^2, dsgn)
# delta0 <- svymean(H0*(Y-Q0), dsgn)/svymean(H0^2, dsgn)
# Equivalently:
model1 <- svyglm(
  Y ~ -1 + offset(Q1) + H1,
  design = dsgn,
  family = gaussian()
)
delta1 <- coef(model1)
model0 <- svyglm(
  Y ~ -1 + offset(Q0) + H0,
  design = dsgn,
  family = gaussian()
)
delta0 <- coef(model0)

# Targeted estimate: Update initial predictions 
Q1_updated <- Q1 + delta1 * H1
Q0_updated <- Q0 + delta0 * H0

## Estimation ATE
# Point estimate
ate_tmle <- svymean(~I(Q1_updated - Q0_updated), dsgn)
ate_tmle

# SE and Wald CIs
eif <- (Q1_updated - Q0_updated - ate_tmle) +
  A / pi_hat * (Y - Q1_updated) -
  ((1 - A) / (1 - pi_hat)) * (Y - Q0_updated)

df$eif <- eif
dsgn_ic <- svydesign(
  strata = province,
  ids = ~household,
  weights = ~weights,
  data = df
)

se_tmle <- SE(svymean(~eif, dsgn_ic))
ci_tmle <- ate_tmle + c(-1, 1) * 1.96 * as.numeric(se_tmle)

se_tmle
ci_tmle

## Comparison to Unadjusted TMLE Estimates (targeting step)
# fluctuation model
delta1_uw <- coef(glm(Y ~ -1 + offset(Q1) + H1, family = gaussian()))
delta0_uw  <- coef(glm(Y ~ -1 + offset(Q0) + H0, family = gaussian()))

# Update initial predictions 
Q1_updated_uw  <- Q1 + delta1_uw  * H1
Q0_updated_uw  <- Q0 + delta0_uw  * H0

# ATE
ate_tmle_uw <- mean(Q1_updated_uw - Q0_updated_uw)

eif_uw <- (Q1_updated_uw - Q0_updated - ate_tmle_uw) +
  A / pi_hat * (Y - Q1_updated_uw) -
  (1 - A) / (1 - pi_hat) * (Y - Q0_updated_uw)

# SE and Wald CIs
se_tmle_uw <- sd(eif_uw) / sqrt(N)
ci_tmle_uw <- ate_tmle_uw + c(-1, 1) * 1.96 * se_tmle_uw
