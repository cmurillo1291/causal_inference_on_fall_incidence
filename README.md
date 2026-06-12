## Causal Inference on Fall Incidents and Their Consequences Using Linked Belgian Health Interview Survey Data

### Abstract

**Background:**  
Population ageing and the high prevalence of falls among older adults are contributing to rising healthcare costs. Preventive interventions such as strengthening and balance exercise are considered effective in reducing recurrent falls and fall-related morbidity. However, evidence on their impact on healthcare expenditures remains limited. Large-scale survey and administrative datasets provide an opportunity to study these effects but introduce methodological challenges, particularly confounding and complex survey design.

**Aim:**  
This thesis estimates the average treatment effect (ATE) of self-reported exercise for fall prevention on accumulated healthcare costs among older adults who experienced a fall, compared to no exercise participation.

**Methods:**  
Healthcare costs over a two-year follow-up were obtained from administrative claims data from the InterMutualistic Agency. For participants who died during follow-up, costs were accumulated only up to the time of death. Exposure and confounders were derived from the 2013 and 2018 waves of the Belgian Health Interview Survey. Exercise was defined as self-reported strengthening and balance training in the year prior to the survey. Confounders included demographic factors, clinical vulnerability, fall history, home safety, baseline healthcare costs, province, and survey wave.

The ATE was estimated using Targeted Maximum Likelihood Estimation (TMLE) with Super Learner. Survey design features (weights, clustering, and stratification) were incorporated. Results were compared with unadjusted regression, forward stepwise regression, and parametric g-computation. Quantile Treatment Effects (QTE) were also estimated. Complete-case analysis was used throughout.

**Results:**  
The complete-case sample included 731 individuals (119 exercise; 612 no exercise), of whom 59 died during follow-up. The estimated ATE was €87 (SE = 2405; 95% CI: −4,626 to 4,800). Ignoring survey design underestimated uncertainty (SE ≈ 1,686). Unadjusted estimates suggested a large positive effect (€4,495), which decreased substantially after adjustment. QTE results suggested heterogeneous effects across the cost distribution, with larger positive estimates in upper quantiles but high uncertainty.

**Conclusions:**  
There is no evidence that self-reported strengthening and balance exercise after a fall reduces healthcare costs. The results highlight the importance of proper confounding adjustment and accounting for survey design when estimating causal effects using population-based survey data.

### Study Variables

#### Outcome (Y)
Accumulated healthcare costs over the two-year follow-up period.

#### Exposure (A)
Self-reported preventive exercise performance after experiencing a fall within the 12 months preceding the survey date.

#### Confounders (W)

- `cost0`: Baseline healthcare costs (preceeding year)
- `age`
- `gender`
- `mobility_restrictions_elderly`
- `home_safer`
- `year`: survey cohort
- `multimorbidity`
- `overweight_obese`
- `education_cat`
- `fall_category`
- `weight`: survey post-stratification weight

### Repository Structure

| File | Description |
|------|-------------|
| `tmle.R` | Main TMLE analysis |
| `positivity.R` | Positivity diagnostics |
| `variableimportance.R` | Variable importance analysis |
| `comparison_approaches.R` | Comparison with alternative causal inference approaches |

The scripts should be run in the following order:

1. `tmle.R`
2. `positivity.R`
3. `variableimportance.R`
4. `comparison_approaches.R`

**Note:** `tmle.R` generates objects required by the other scripts. This script also contains the procedure to obatain the TMLE Estimates without accounting for the survey sampling design.  

### Data Availability

The data used in this dissertation are derived from linked Belgian Health Interview Survey data and are not publicly available. Consequently, the repository contains code only and does not include the original data.

### Software

Analyses were conducted in R. Required packages are loaded within the individual scripts.

### Author

Carlos Murillo Ezcurra  
Master of Statistics (MaStat), Ghent University

### Supervisors: 
Prof Robby de Pauw and Prof Stijn Vansteelandt


