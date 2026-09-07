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


# 02_ate_analysis.R
# ------------------------------------------------------------
# Purpose:
#   1. Load the prepared longitudinal analysis dataset
#   2. Fit causal forests for eight intergroup-warmth outcomes
#   3. Estimate population Average Treatment Effects (ATEs)
#   4. Apply family-wise multiple-testing correction
#   5. Produce and save a portfolio-ready ATE figure
#
# Exposure:
#   Higher Openness to Experience
#
# Contrast:
#   Openness > 5 versus Openness <= 5
#
# Outcomes:
#   Warmth toward eight minority / social groups


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

derived_dir <- here("derived")
figures_dir <- here("figures")

dir.create(
  derived_dir,
  showWarnings = FALSE
)

dir.create(
  figures_dir,
  showWarnings = FALSE
)


# -------------------------------------------------------------------------
# Load prepared analysis objects
# -------------------------------------------------------------------------

df_grf <- qs::qread(
  here(
    "derived",
    "df_grf.qs"
  )
)

E <- readRDS(
  here(
    "derived",
    "covariates.rds"
  )
)

# The wide-format dataset is used by the plotting / interpretation
# functions to recover original variable information.

original_df <- qs::qread(
  here(
    "derived",
    "df_wide.qs"
  )
)


# -------------------------------------------------------------------------
# Exposure
# -------------------------------------------------------------------------

treatment_var <- "t1_openness_binary"


# Verify binary coding.

stopifnot(
  all(
    df_grf[[treatment_var]][
      !is.na(df_grf[[treatment_var]])
    ] %in% 0:1
  )
)


W <- as.vector(
  df_grf[[treatment_var]]
)


# -------------------------------------------------------------------------
# Outcomes
# -------------------------------------------------------------------------

outcome_vars <- c(
  "t2_warm_asians_z",
  "t2_warm_chinese_z",
  "t2_warm_immigrants_z",
  "t2_warm_indians_z",
  "t2_warm_maori_z",
  "t2_warm_muslims_z",
  "t2_warm_pacific_z",
  "t2_warm_refugees_z"
)


# Human-readable labels used in figures and tables.

outcome_labels <- list(

  "t2_warm_asians_z" =
    "Warmth Toward Asians",

  "t2_warm_chinese_z" =
    "Warmth Toward Chinese",

  "t2_warm_immigrants_z" =
    "Warmth Toward Immigrants",

  "t2_warm_indians_z" =
    "Warmth Toward Indians",

  "t2_warm_maori_z" =
    "Warmth Toward Māori",

  "t2_warm_muslims_z" =
    "Warmth Toward Muslims",

  "t2_warm_pacific_z" =
    "Warmth Toward Pacific Peoples",

  "t2_warm_refugees_z" =
    "Warmth Toward Refugees"
)


# Verify that all required outcomes exist.

stopifnot(
  all(
    outcome_vars %in%
      names(df_grf)
  )
)


# -------------------------------------------------------------------------
# Covariates
# -------------------------------------------------------------------------

# Remove attributes that can interfere with downstream forest fitting.
#
# E contains the baseline covariates prepared in 01_data_preparation.R.

X <- margot::remove_numeric_attributes(
  df_grf[E]
)


# -------------------------------------------------------------------------
# Analysis weights
# -------------------------------------------------------------------------

# The weights combine:
#
#   baseline survey-design weights
#     ×
#   stage-0 IPCW
#     ×
#   stage-1 IPCW
#
# They were trimmed and normalised in 01_data_preparation.R.

weights <- df_grf$combo_weights


# Basic diagnostics.

stopifnot(
  length(weights) == nrow(df_grf)
)

stopifnot(
  all(
    is.finite(
      weights[
        !is.na(weights)
      ]
    )
  )
)


message(
  "Analysis sample: ",
  nrow(df_grf),
  " participants"
)

message(
  "Baseline covariates: ",
  ncol(X)
)

message(
  "Outcomes: ",
  length(outcome_vars)
)


# -------------------------------------------------------------------------
# Causal-forest settings
# -------------------------------------------------------------------------

# Generalized Random Forest settings used in the submitted analysis.
#
# `stabilize.splits = TRUE` improves splitting stability when treatment
# probabilities vary across covariate space.
#
# 2,000 trees provide a reasonably stable forest for this analysis.

grf_defaults <- list(

  seed = 123,

  stabilize.splits = TRUE,

  num.trees = 2000
)


# -------------------------------------------------------------------------
# Fit causal forests
# -------------------------------------------------------------------------

# One causal forest is fitted for each outcome.
#
# The margot wrapper applies the EPIC Lab causal-forest workflow and
# retains the objects required for subsequent RATE / Qini / policy-tree
# analyses.
#
# A 70/30 train/evaluation split is retained for later heterogeneity
# assessment.

models_binary <-
  margot::margot_causal_forest_parallel(

    data = df_grf,

    outcome_vars = outcome_vars,

    covariates = X,

    W = W,

    weights = weights,

    grf_defaults = grf_defaults,

    top_n_vars = 15,

    save_models = TRUE,

    save_data = TRUE,

    train_proportion = 0.70
  )


# -------------------------------------------------------------------------
# Save fitted forests
# -------------------------------------------------------------------------

qs::qsave(

  models_binary,

  here(
    "derived",
    "causal_forest_models.qs"
  )
)


message(
  "Causal forests fitted and saved."
)


# -------------------------------------------------------------------------
# ATE plot configuration
# -------------------------------------------------------------------------

ate_title <-
  "Average Treatment Effects of Openness on Intergroup Warmth"


plot_defaults <- list(

  type = "RD",

  title = ate_title,

  # Sensitivity-analysis threshold used in the coursework workflow.
  e_val_bound_threshold = 1.2,

  # Keep the portfolio figure visually concise.
  include_coefficients = FALSE,

  x_offset = -0.5,

  x_lim_lo = -0.5,

  x_lim_hi = 0.5,

  text_size = 4,

  linewidth = 0.5,

  estimate_scale = 1,

  base_size = 18,

  point_size = 2,

  title_size = 18,

  subtitle_size = 14,

  legend_text_size = 10,

  legend_title_size = 10
)


plot_options <-
  margot::margot_plot_create_options(

    title = "",

    subtitle = "",

    base_defaults = plot_defaults,

    filename_prefix =
      "openness_warmth_ate"
  )


# -------------------------------------------------------------------------
# Estimate and visualise Average Treatment Effects
# -------------------------------------------------------------------------

# Because eight outcomes are tested simultaneously, confidence intervals
# and inferential decisions use a Bonferroni family-wise adjustment
# at alpha = 0.05.
#
# This matches the submitted coursework analysis.

ate_results <-
  margot::margot_plot(

    models_binary$combined_table,

    options = plot_options,

    label_mapping = outcome_labels,

    include_coefficients = FALSE,

    save_output = FALSE,

    # Order results by sensitivity-analysis strength.
    order = "evaluebound_asc",

    original_df = original_df,

    e_val_bound_threshold = 1.2,

    rename_ate = TRUE,

    adjust = "bonferroni",

    alpha = 0.05
  )


# -------------------------------------------------------------------------
# Inspect results
# -------------------------------------------------------------------------

print(
  ate_results$plot
)

cat(
  "\nATE interpretation\n",
  "------------------\n",
  ate_results$interpretation,
  "\n"
)


# -------------------------------------------------------------------------
# Save portfolio figure
# -------------------------------------------------------------------------

ggsave(

  filename = here(
    "figures",
    "ate_effects.png"
  ),

  plot = ate_results$plot,

  width = 9,

  height = 6,

  dpi = 300
)


# -------------------------------------------------------------------------
# Save ATE results for later use
# -------------------------------------------------------------------------

qs::qsave(

  ate_results,

  here(
    "derived",
    "ate_results.qs"
  )
)


# -------------------------------------------------------------------------
# Save a machine-readable results table
# -------------------------------------------------------------------------

if (
  "transformed_table" %in%
    names(ate_results)
) {

  readr::write_csv(

    as.data.frame(
      ate_results$transformed_table
    ),

    here(
      "derived",
      "ate_results.csv"
    )
  )
}


# -------------------------------------------------------------------------
# Completion message
# -------------------------------------------------------------------------

message(
  "ATE analysis complete."
)

message(
  "Figure saved to: figures/ate_effects.png"
)

message(
  "Model objects saved to: derived/causal_forest_models.qs"
)

