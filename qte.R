#install.packages("qte")
library(qte)
library(dplyr)
library(knitr)
set.seed(123)

# select variables
Y <- his_subset$cost1
A <- his_subset$exercise
W <- his_subset %>%
  select(cost0, age,gender,mobility_restrictions_elderly, home_safer, year,
         multimorbidity,overweight_obese,education_cat, fall_category, weight)

# normalization weights
weights_norm <- his_subset$weight / mean(his_subset$weight)
jt.cia <- ci.qte(
  formla  = Y ~ A,
  x       = W,               
  data    = data.frame(Y = Y, A = A, W),
  w       = weights_norm,         
  probs   = seq(0.05, 0.95, 0.05),
  se      = TRUE,
  iters   = 5000,             
  method  = "logit"          
)

summary(jt.cia)

tab <- data.frame(
  tau       = jt.cia$probs,
  QTE       = round(jt.cia$qte, 2),
  Std.Error = round(jt.cia$qte.se, 2),
  CI        = paste0(
    "(",
    round(jt.cia$qte.lower, 0),
    ", ",
    round(jt.cia$qte.upper, 0),
    ")"
  )
)
kable(tab)
ggqte(jt.cia)
