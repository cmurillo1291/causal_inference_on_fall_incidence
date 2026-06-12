## Causal Inference on Fall Incidents and Their Consequences Using Linked Belgian Health Interview Survey Data

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


