### Comparison with other approaches 
library(survey)
library(dplyr)
library(ggplot2)
library(knitr)
library(broom)

## definition survey design
dsgn <- svydesign(
  ids = ~household, 
  strata = ~province, 
  weights = ~weight,
  data = his_subset
)

## unadjusted model

model_unadj <- svyglm(Y ~ A, design = dsgn)
summary(model_unadj)
confint(model_unadj)
# ---------------------------------------------------------------------------
## forward stepwise regression
varsW <- names(his_subset[3:13])
selected <- c()
remaining <- varsW
threshold <- 0.05  

repeat {
  pvals <- sapply(remaining, function(v) {
    
    f <- as.formula(
      paste("Y ~ A +", paste(c(selected, v), collapse = " + "))
    )
    fit <- svyglm(f, design = dsgn)
    coef_summary <- summary(fit)$coefficients
    coef_summary[v, "Pr(>|t|)"]
  })
  
  best_var <- names(which.min(pvals))
  best_p <- min(pvals)
  
  if (best_p < threshold) {
    selected <- c(selected, best_var)
    remaining <- setdiff(remaining, best_var)
  } else {
    break
  }
  if (length(remaining) == 0) break
}
final_formula <- as.formula(
  paste("Y ~ A +", paste(selected, collapse = " + "))
)

model_step <- svyglm(final_formula, design = dsgn)
summary(model_step)
confint(model_step)
# ---------------------------------------------------------------------------
## Parametric g-computation
formula_gcomp <- as.formula(paste("Y ~ A +", paste(varsW, collapse = " + ")))
glm_Q <- svyglm(formula_gcomp, family = gaussian(), design = dsgn)

dat0 <- dsgn$variables[, varsW, drop = FALSE]
dat0$A <- 0
dat1 <- dsgn$variables[, varsW, drop = FALSE]
dat1$A <- 1

Q0_par <- predict(glm_Q, newdata = dat0, type = "response")
Q1_par <- predict(glm_Q, newdata = dat1, type = "response")

ate_gcomp <- svymean(~I(Q1_par - Q0_par), dsgn)
ate_gcomp

# Bootstrap to get SE
boot_adj <- function() {
  # unique household IDs
  hh_ids <- unique(his_subset$household)
  # sample households with replacement
  sampled_hh <- sample(
    hh_ids,
    size = length(hh_ids),
    replace = TRUE
  )
  
  his_ima_boot <- do.call(
    rbind,
    lapply(seq_along(sampled_hh), function(i) {
      dat <- subset(his_subset, household == sampled_hh[i])
      dat$boot_hh <- paste0(sampled_hh[i], "_", i)
      
      dat
    })
  )
  
  hisdesign_boot <- svydesign(
    id = ~boot_hh,
    strata = ~province,
    weights = ~weight,
    data = his_ima_boot
  )
  
  reg_boot <- svyglm(
    Y ~ A + cost0 + age + gender + weight +
      mobility_restrictions_elderly +
      home_safer + year + multimorbidity +
      overweight_obese + education_cat +
      fall_category,
    design = hisdesign_boot
  )
  
  W <- his_ima_boot[, !(names(his_ima_boot) %in%
                          c("Y", "A", "province", "household", "boot_hh"))]
  
  Q0 <- predict(reg_boot,newdata = transform(W, A = 0),
                type = "response")
  Q1 <- predict(reg_boot, newdata = transform(W, A = 1),
                type = "response")
  mean(Q1 - Q0)
}
set.seed(123)
ate_results <- replicate(5000, boot_adj())
ate_boot <- mean(ate_results)
ate_boot
se_boot <- sd(ate_results)
se_boot
ci_boot <- quantile(ate_results, c(0.025, 0.975))
ci_boot

# ---------------------------------------------------------------------------
## ploting
results <- bind_rows(
  tidy(model_unadj, conf.int = TRUE) %>%
    filter(term == "A") %>%
    mutate(model = "Unadjusted"),
  
  tidy(model_step, conf.int = TRUE) %>%
    filter(term == "A") %>%
    mutate(model = "Forward stepwise"),
  
  data.frame(
    term      = "A",
    estimate  = ate_gcomp,
    std.error = se_boot,
    conf.low  = as.numeric(ci_boot[1]),
    conf.high = as.numeric(ci_boot[2]),
    model     = "G-computation"
  ),
  
  data.frame(
    term      = "A",
    estimate  = ate_tmle_uw,
    std.error = ate_tmle_uw,
    conf.low  = ci_tmle_uw[1],
    conf.high = ci_tmle_uw[2],
    model     = "TMLE w/o survey"
  ),
  
  data.frame(
    term      = "A",
    estimate  = as.numeric(ate_tmle[1]),
    std.error = as.numeric(se_tmle),
    conf.low  = ci_tmle[1],
    conf.high = ci_tmle[2],
    model     = "TMLE"
  )
  
)

results$model <- factor(
  results$model,
  levels = c(
    "Unadjusted",
    "Forward stepwise",
    "G-computation",
    "TMLE w/o survey",
    "TMLE"
  )
)
results_table <- results %>%
  select(model,
         estimate,
         std.error,
         conf.low,
         conf.high) %>%
  mutate(
    across(where(is.numeric), round, 3)
  )

kable(
  results_table,
  caption = "ATE across models"
)

# forest plot
results <- results %>%
  mutate(
    highlight = case_when(
      model == "TMLE" ~ "TMLE",
      model == "TMLE w/o survey" ~ "TMLE w/o survey",
      TRUE ~ "Other"
    )
  )

ggplot(results,
       aes(x = estimate,
           y = model,
           color = highlight)) +
  
  geom_point(size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.2) +
  
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  
  scale_color_manual(
    values = c(
      "TMLE" = "blue",
      "TMLE w/o survey" = "red",
      "Other" = "black"
    )
  ) +
  
  labs(
    x = "Estimate (95% CI)",
    y = ""
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
