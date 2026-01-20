library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringi)
library(lubridate)
library(boot)
library(icarus)

# STEP 1: DATA LOADING
myfile <- read_csv2("IP18mois_LicencePro_2022.csv")

# Cleaning accents for the variable of interest
myfile$`46. Emploi_18M_Lien_emploi_dom_formation` <- stri_trans_general(
  myfile$`46. Emploi_18M_Lien_emploi_dom_formation`, "Latin-ASCII"
)

# Harmonization of response categories
myfile <- myfile %>%
  mutate(`46. Emploi_18M_Lien_emploi_dom_formation` = case_when(
    `46. Emploi_18M_Lien_emploi_dom_formation` == "Plut�t oui" ~ "Plutot oui",
    `46. Emploi_18M_Lien_emploi_dom_formation` == "Plut�t non" ~ "Plutot non",
    `46. Emploi_18M_Lien_emploi_dom_formation` == "Tout � fait" ~ "Tout a fait",
    TRUE ~ `46. Emploi_18M_Lien_emploi_dom_formation`
  ))

# STEP 2: RESPONSE STATUS
myfile <- myfile %>%
  mutate(
    response_status = case_when(
      `6. Repondant` == "Non" ~ "Total non-response",
      `6. Repondant` == "Oui" & is.na(`46. Emploi_18M_Lien_emploi_dom_formation`) ~ "Partial non-response",
      `6. Repondant` == "Oui" & !is.na(`46. Emploi_18M_Lien_emploi_dom_formation`) ~ "Response"
    )
  )

# STEP 3: PREPROCESSING

myfile <- myfile %>%
  mutate(
    naissance_clean = trimws(`160. Naissance_Date`),
    naissance_date = dmy(naissance_clean),
    annee = year(naissance_date)
  )

myfile$`146. Domaine_formation` <- stri_trans_general(myfile$`146. Domaine_formation`, "Latin-ASCII")
myfile <- myfile %>%
  mutate(`146. Domaine_formation` = case_when(
    `146. Domaine_formation` == "Droit, �conomie, gestion" ~ "Droit, economie, gestion",
    `146. Domaine_formation` == "Sciences de la Sant�" ~ "Sciences de la Sante",
    TRUE ~ `146. Domaine_formation`
  ))

myfile$`151. Composante_localisation` <- stri_trans_general(myfile$`151. Composante_localisation`, "Latin-ASCII")

# STEP 4: BINARY Y VARIABLE
myfile <- myfile %>%
  mutate(y = case_when(
    `46. Emploi_18M_Lien_emploi_dom_formation` %in% c("Plutot oui", "Tout a fait") ~ 1,
    `46. Emploi_18M_Lien_emploi_dom_formation` %in% c("Plutot non", "Pas du tout") ~ 0,
    TRUE ~ NA_real_
  ))

# STEP 4: NON-RESPONSE INDICATOR FOR Y
myfile <- myfile %>%
  mutate(non_rep_y = if_else(is.na(y), 1, 0))

# STEP 5: LOGISTIC NON-RESPONSE MODEL
model_nr <- glm(non_rep_y ~ `152. Composante` + `158. Genre` + annee +
                  `159. Boursier` + `148. Parcours_Type` +
                  `161. Nationalite` + `156. Regime_inscription`,
                data = myfile,
                family = binomial())

summary(model_nr)

# STEP 6: SIGNIFICANT VARIABLES

# Extraction of p-values from the model
coeffs <- summary(model_nr)$coefficients
p_values <- coeffs[, 4]  # 4th column = Pr(>|z|)

# Filter significant variables at the 1% level
significatives <- coeffs[p_values < 0.01, , drop = FALSE]

# Display
cat("Significant variables (p < 0.05) in the non-response model:\n")
print(significatives)

# STEP 7: NON-RESPONSE ADJUSTMENT: INITIAL WEIGHTS

# 1. Computation of the weighting factor based on the non-response rate
n_oui <- sum(myfile$`6. Repondant` == "Oui", na.rm = TRUE)
n_non <- sum(myfile$`6. Repondant` == "Non", na.rm = TRUE)
facteur_correction <- (n_oui + n_non) / n_oui  # each respondent represents more individuals

# 2. Assignment of initial weights
myfile <- myfile %>%
  mutate(
    poidsini = if_else(`6. Repondant` == "Oui", facteur_correction, NA_real_)
  )

# STEP 8: CALIBRATION ON MARGINS

# Known population totals for the auxiliary variable
# Example: 466 women, 565 men
mar1 <- c("158. Genre", 2, 466, 565, 0)
marges <- rbind(mar1)

# Keep only valid respondents (y observed)
myfile_calage <- myfile %>%
  filter(!is.na(y))

# Calibration procedure
poids_calages <- calibration(
  data = myfile_calage,
  marginMatrix = marges,
  colWeights = "poidsini",
  method = "linear"
)

# Add calibrated weights to the dataset
myfile_calage$poids_calages <- poids_calages

# STEP 9: ESTIMATIONS

# Corrected estimate (with calibrated weights)
estimation_calee <- sum(myfile_calage$y * myfile_calage$poids_calages) /
  sum(myfile_calage$poids_calages)

# Naive estimate (without correction)
estimation_naive <- mean(myfile_calage$y)

# Display results
cat("Naive estimate:", round(estimation_naive * 100, 2), "%\n")
cat("Corrected (calibrated) estimate:", round(estimation_calee * 100, 2), "%\n")

# Here we obtain 82.8% for the naive estimate and 82.59% for the adjusted estimate,
# which matches the expected results published by the professor at the following link:
# https://sphinx-manage.univ-amu.fr/report/(T(pjs885b3km))/r.aspx

