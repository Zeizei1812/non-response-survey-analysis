# Non-response bias correction in survey data

This project analyzes a graduate employment survey affected by partial and total non-response.
Since non-response is not random, naive estimates may be biased.

## Data
The dataset comes from a graduate employment survey (Licence Professionnelle, 18 months after graduation).
Due to confidentiality reasons, the raw data are not publicly available.

## Methodology
- Data cleaning and preprocessing
- Construction of a binary outcome variable (employment status)
- Logistic regression modeling of response probability
- Correction using inverse propensity weighting (IPW)
- Calibration on known population margins (gender) using the `icarus` package
- Comparison between naive and corrected estimates

## Results
The corrected estimates are consistent with the official published results,
highlighting the relevance of non-response bias correction methods.

## Tools
- R
- Packages: dplyr, ggplot2, survey, icarus
