# Portfolio adaptation of an academic causal-inference project
# ------------------------------------------------------------
# Data:
#   Simulated longitudinal data modelled on the
#   New Zealand Attitudes and Values Study (NZAVS).
#
# Analysis design:
#   Based on the EPIC Lab standard causal-inference workflow
#   implemented using the `margot` R ecosystem.
#
# This portfolio version highlights my study-specific configuration,
# analysis choices, execution, and interpretation. It does not claim
# authorship of the underlying EPIC Lab workflow or packages.


# 01_data_preparation.R
# ------------------------------------------------------------
# Purpose:
#   1. Define the three-wave study design
#   2. Define exposure, outcomes, and baseline covariates
#   3. Construct the binary Openness exposure
#   4. Reshape longitudinal data to wide format
#   5. Construct two-stage inverse-probability-of-censoring weights
#   6. Combine censoring and design weights
#   7. Save the final dataset used by the causal-forest analysis


# -------------------------------------------------------------------------
# Packages and reproducibility
# -------------------------------------------------------------------------

library(tidyverse)
library(here)
library(qs)
library(grf)
library(margot)

set.seed(123)


# -------------------------------------------------------------------------
# Project directories
# -------------------------------------------------------------------------

data_dir <- here("data")
derived_dir <- here("derived")

dir.create(derived_dir, showWarnings = FALSE)


# -------------------------------------------------------------------------
# Load simulated teaching data
# -------------------------------------------------------------------------

# The synthetic instructional dataset is not redistributed in this
# portfolio repository.
#
# Expected local object:
#   data/df_nz_long.qs

df_nz_long <- margot::here_read_qs(
  "df_nz_long",
  data_dir
)


# -------------------------------------------------------------------------
# Initial data preparation
# -------------------------------------------------------------------------

rural_labels <- c(
  "High Urban Accessibility",
  "Medium Urban Accessibility",
  "Low Urban Accessibility",
  "Remote",
  "Very Remote"
)

dat_prep <- df_nz_long |>
  arrange(id, wave) |>
  margot::remove_numeric_attributes() |>
  mutate(

    # Cap implausibly large alcohol-intensity values.
    alcohol_intensity = pmin(alcohol_intensity, 15),

    # Ordered rural / urban accessibility classification.
    rural_gch_2018_l = factor(
      rural_gch_2018_l,
      levels = 1:5,
      labels = rural_labels,
      ordered = TRUE
    )
  ) |>
  droplevels()


# -------------------------------------------------------------------------
# Study design
# -------------------------------------------------------------------------

# Three-wave temporal ordering:
#
# t0 = baseline covariates
# t1 = exposure
# t2 = outcomes

baseline_wave <- "2018"
exposure_waves <- "2019"
outcome_wave <- "2020"

all_waves <- c(
  baseline_wave,
  exposure_waves,
  outcome_wave
)


# -------------------------------------------------------------------------
# Exposure
# -------------------------------------------------------------------------

name_exposure <- "openness"

exposure_var_binary <- paste0(
  name_exposure,
  "_binary"
)

exposure_var <- c(
  name_exposure,
  exposure_var_binary
)

# Openness is measured on a 1–7 scale.
# The analysis contrasts:
#
#   1 = Openness > 5
#   0 = Openness <= 5

cut_points <- c(1, 5)
exposure_cut <- 5


# -------------------------------------------------------------------------
# Outcomes
# -------------------------------------------------------------------------

outcome_vars <- c(
  "warm_asians",
  "warm_chinese",
  "warm_immigrants",
  "warm_indians",
  "warm_maori",
  "warm_muslims",
  "warm_pacific",
  "warm_refugees"
)


# -------------------------------------------------------------------------
# Baseline covariates
# -------------------------------------------------------------------------

baseline_vars <- c(

  # Demographics
  "age",
  "born_nz_binary",
  "education_level_coarsen",
  "employed_binary",
  "eth_cat",
  "male_binary",
  "not_heterosexual_binary",
  "parent_binary",
  "partner_binary",
  "rural_gch_2018_l",
  "sample_frame_opt_in_binary",

  # Personality traits other than Openness
  "agreeableness",
  "conscientiousness",
  "neuroticism",

  # Health and lifestyle
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_disability_binary",
  "log_hours_children",
  "log_hours_commute",
  "log_hours_exercise",
  "log_hours_housework",
  "log_household_inc",
  "short_form_health",
  "smoker_binary",

  # Social and psychological characteristics
  "belong",
  "nz_dep2018",
  "nzsei_13_l",
  "political_conservative",
  "religion_identification_level",
  "warm_nz_euro"
)

baseline_vars <- sort(baseline_vars)
outcome_vars <- sort(outcome_vars)


# -------------------------------------------------------------------------
# Define baseline cohort
# -------------------------------------------------------------------------

# The coursework analysis required baseline measurement of the exposure
# and baseline measurements of the study outcomes.

ids_baseline <- dat_prep |>
  filter(
    wave == baseline_wave,
    !is.na(.data[[name_exposure]]),
    if_all(
      all_of(outcome_vars),
      ~ !is.na(.)
    )
  ) |>
  distinct(id) |>
  pull(id)

dat_long <- dat_prep |>
  filter(
    id %in% ids_baseline,
    wave %in% all_waves
  ) |>
  droplevels()

message(
  "Baseline cohort: ",
  length(ids_baseline),
  " participants"
)


# -------------------------------------------------------------------------
# Construct binary exposure
# -------------------------------------------------------------------------

# `create_ordered_variable()` is part of the EPIC Lab / margot workflow.
# With the selected cut-point, the substantive contrast is:
#
#   Openness > 5 versus Openness <= 5.

dat_long <- margot::create_ordered_variable(
  dat_long,
  var_name = name_exposure,
  custom_breaks = cut_points,
  cutpoint_inclusive = "upper"
)

# Convert binary variables into the format expected downstream.

dat_long <- margot::margot_process_binary_vars(
  dat_long
)


# -------------------------------------------------------------------------
# Transform skewed time-use and income variables
# -------------------------------------------------------------------------

dat_long_final <- margot::margot_log_transform_vars(
  dat_long,

  vars = c(
    starts_with("hours_"),
    "household_inc"
  ),

  prefix = "log_",
  keep_original = FALSE,

  # Preserve exposure variables in their original form.
  exceptions = exposure_var

) |>
  select(
    all_of(
      c(
        baseline_vars,
        exposure_var,
        outcome_vars,
        "id",
        "wave",
        "year_measured",
        "sample_weights"
      )
    )
  ) |>
  droplevels()


# -------------------------------------------------------------------------
# Preserve baseline survey-design weights
# -------------------------------------------------------------------------

dat_baseline <- dat_long_final |>
  filter(wave == baseline_wave)

t0_sample_weights <- dat_baseline$sample_weights


# -------------------------------------------------------------------------
# Long-to-wide transformation
# -------------------------------------------------------------------------

df_wide <- margot::margot_wide_machine(
  dat_long_final,

  id = "id",
  wave = "wave",

  baseline_vars = baseline_vars,
  exposure_var = exposure_var,
  outcome_vars = outcome_vars,

  confounder_vars = NULL,

  imputation_method = "none",

  include_exposure_var_baseline = TRUE,
  include_outcome_vars_baseline = TRUE,

  extend_baseline = FALSE,
  include_na_indicators = FALSE
)

# Restore the baseline survey-design weights.

df_wide$t0_sample_weights <- t0_sample_weights


# -------------------------------------------------------------------------
# Encode wide-format data for GRF
# -------------------------------------------------------------------------

ordinal_columns <- intersect(
  c(
    "t0_education_level_coarsen",
    "t0_eth_cat",
    "t0_rural_gch_2018_l",
    "t0_gen_cohort"
  ),
  names(df_wide)
)

continuous_columns_keep <- "t0_sample_weights"

df_wide_encoded <-
  margot::margot_process_longitudinal_data_wider(

    df_wide,

    ordinal_columns = ordinal_columns,

    continuous_columns_keep =
      continuous_columns_keep,

    not_lost_in_following_wave =
      "not_lost_following_wave",

    lost_in_following_wave =
      "lost_following_wave",

    remove_selected_columns = TRUE,

    exposure_var = exposure_var,

    scale_continuous = TRUE
  )


# -------------------------------------------------------------------------
# Ensure binary exposure is encoded as 0 / 1
# -------------------------------------------------------------------------

t0_exposure_binary <- paste0(
  "t0_",
  exposure_var_binary
)

t1_exposure_binary <- paste0(
  "t1_",
  exposure_var_binary
)


to_binary_numeric <- function(x) {

  if (is.factor(x)) {
    return(as.numeric(x) - 1)
  }

  as.numeric(x)
}


df_wide_encoded[[t0_exposure_binary]] <-
  to_binary_numeric(
    df_wide_encoded[[t0_exposure_binary]]
  )

df_wide_encoded[[t1_exposure_binary]] <-
  to_binary_numeric(
    df_wide_encoded[[t1_exposure_binary]]
  )


# Sanity checks.

stopifnot(
  all(
    df_wide_encoded[[t0_exposure_binary]][
      !is.na(df_wide_encoded[[t0_exposure_binary]])
    ] %in% 0:1
  ),

  all(
    df_wide_encoded[[t1_exposure_binary]][
      !is.na(df_wide_encoded[[t1_exposure_binary]])
    ] %in% 0:1
  )
)


# -------------------------------------------------------------------------
# Two-stage inverse-probability-of-censoring weighting
# -------------------------------------------------------------------------
#
# Stage 0:
#   baseline -> exposure wave
#
# Stage 1:
#   exposure wave -> outcome wave
#
# Final weight:
#
#   survey design weight × IPCW(stage 0) × IPCW(stage 1)
#
# The upper 1% of the resulting weights is trimmed.


df <- df_wide_encoded


# -------------------------------------------------------------------------
# Stage 0: dropout between t0 and t1
# -------------------------------------------------------------------------

baseline_covars <- df |>
  select(
    starts_with("t0_"),
    -ends_with("_lost"),
    -ends_with("lost_following_wave"),
    -ends_with("_weights")
  ) |>
  names() |>
  sort()


X0 <- as.matrix(
  df[, baseline_covars]
)

D0 <- factor(
  df$t0_lost_following_wave,
  levels = c(0, 1)
)


pf0 <- grf::probability_forest(
  X0,
  D0
)

p_dropout_0 <- predict(
  pf0,
  X0
)$predictions[, 2]


# IPCW = inverse probability of remaining observed.

w0 <- ifelse(
  D0 == 1,
  0,
  1 / (1 - p_dropout_0)
)

df$w0 <- w0


# -------------------------------------------------------------------------
# Stage 1: dropout between t1 and t2
# -------------------------------------------------------------------------

df1 <- df |>
  filter(
    t0_lost_following_wave == 0
  )

# The second censoring model is conditional on baseline covariates
# and the observed t1 exposure.

stage1_model_data <- df1 |>
  filter(
    !is.na(.data[[t1_exposure_binary]])
  )


X1 <- cbind(
  as.matrix(
    stage1_model_data[, baseline_covars]
  ),
  treatment =
    stage1_model_data[[t1_exposure_binary]]
)

D1 <- factor(
  stage1_model_data$t1_lost_following_wave,
  levels = c(0, 1)
)


pf1 <- grf::probability_forest(
  X1,
  D1
)

p_dropout_1 <- predict(
  pf1,
  X1
)$predictions[, 2]


w1 <- ifelse(
  D1 == 1,
  0,
  1 / (1 - p_dropout_1)
)


# Map the stage-1 weights back to the survivor dataset.
# Participants without a measured t1 exposure receive zero weight.

df1$w1 <- 0

df1$w1[
  match(
    stage1_model_data$id,
    df1$id
  )
] <- w1


# -------------------------------------------------------------------------
# Combine survey-design and censoring weights
# -------------------------------------------------------------------------

w0_matched <- df$w0[
  match(
    df1$id,
    df$id
  )
]


raw_weight <-
  df1$t0_sample_weights *
  w0_matched *
  df1$w1


# Trim extreme weights at the 99th percentile.

positive_weights <- raw_weight[
  !is.na(raw_weight) &
    raw_weight > 0
]

upper_bound <- quantile(
  positive_weights,
  0.99,
  na.rm = TRUE
)

trimmed_weight <- pmin(
  raw_weight,
  upper_bound
)


# Normalise weights to have mean approximately 1.

df1$combo_weights <-
  trimmed_weight /
  mean(
    trimmed_weight,
    na.rm = TRUE
  )


# -------------------------------------------------------------------------
# Final analysis sample
# -------------------------------------------------------------------------

# Retain participants observed through the outcome wave.

df_analysis <- df1 |>
  filter(
    t1_lost_following_wave == 0
  ) |>
  droplevels()


# Covariate names used by the causal-forest model.

E <- baseline_covars


# Reorder columns to make the dataset easier to inspect.

df_grf <- df_analysis |>

  relocate(
    ends_with("_weights"),
    .before = starts_with("t0_")
  ) |>

  relocate(
    ends_with("_weight"),
    .before = ends_with("_weights")
  ) |>

  relocate(
    starts_with("t0_"),
    .before = starts_with("t1_")
  ) |>

  relocate(
    starts_with("t1_"),
    .before = starts_with("t2_")
  ) |>

  relocate(
    any_of("t0_not_lost_following_wave"),
    .before = starts_with("t1_")
  ) |>

  relocate(
    all_of(t1_exposure_binary),
    .before = starts_with("t2_")
  ) |>

  droplevels()


# -------------------------------------------------------------------------
# Final checks
# -------------------------------------------------------------------------

stopifnot(
  all(
    df_grf[[t1_exposure_binary]][
      !is.na(df_grf[[t1_exposure_binary]])
    ] %in% 0:1
  )
)

stopifnot(
  all(
    is.finite(
      df_grf$combo_weights[
        !is.na(df_grf$combo_weights)
      ]
    )
  )
)

message(
  "Final causal-forest sample: ",
  nrow(df_grf),
  " participants"
)

message(
  "Number of baseline covariates: ",
  length(E)
)


# -------------------------------------------------------------------------
# Save objects for later analysis scripts
# -------------------------------------------------------------------------

qs::qsave(
  df_grf,
  here(
    "derived",
    "df_grf.qs"
  )
)

saveRDS(
  E,
  here(
    "derived",
    "covariates.rds"
  )
)

qs::qsave(
  df_wide,
  here(
    "derived",
    "df_wide.qs"
  )
)

qs::qsave(
  df_wide_encoded,
  here(
    "derived",
    "df_wide_encoded.qs"
  )
)


message(
  "Data preparation complete. ",
  "Objects saved to derived/."
)

