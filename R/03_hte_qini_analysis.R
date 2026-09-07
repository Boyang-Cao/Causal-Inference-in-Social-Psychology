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


# 03_hte_qini_analysis.R
# ------------------------------------------------------------
# Purpose:
#   1. Evaluate treatment-effect heterogeneity
#   2. Compute RATE-AUTOC and RATE-Qini statistics
#   3. Apply false-discovery-rate correction across outcomes
#   4. Identify outcomes with evidence of useful heterogeneity
#   5. Produce Qini curves for selected outcomes
#
# This script focuses on whether estimated CATEs provide useful
# treatment-prioritisation information beyond the population ATE.


# -------------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------------

library(tidyverse)
library(here)
library(qs)
library(margot)
library(knitr)
library(patchwork)


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
# Load fitted causal-forest models
# -------------------------------------------------------------------------

models_binary <- qs::qread(
  here(
    "derived",
    "causal_forest_models.qs"
  )
)


# Original wide-format data are used by margot for labels,
# back-transformation, and visualisation.

original_df <- qs::qread(
  here(
    "derived",
    "df_wide.qs"
  )
)


# -------------------------------------------------------------------------
# Outcome labels
# -------------------------------------------------------------------------

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


# -------------------------------------------------------------------------
# Outcome direction
# -------------------------------------------------------------------------

# All outcomes measure intergroup warmth.
#
# Higher scores are therefore desirable, so no outcome needs to be
# reversed before evaluating treatment prioritisation.

flipped_outcomes <- character(0)


# -------------------------------------------------------------------------
# RATE analysis
# -------------------------------------------------------------------------
#
# RATE evaluates whether ranking individuals by predicted treatment
# effects produces useful treatment-prioritisation information.
#
# We report two weighting schemes:
#
# AUTOC:
#   Places greater emphasis on individuals with the largest predicted
#   treatment effects.
#
# Qini:
#   Gives relatively more weight to treatment gains across broader
#   portions of the population.
#
# Multiple heterogeneity tests are corrected using FDR.


rate_results <-
  margot::margot_rate(

    models = models_binary,

    policy = "treat_best",

    alpha = 0.10,

    adjust = "fdr",

    label_mapping = outcome_labels
  )


# -------------------------------------------------------------------------
# Interpret RATE results
# -------------------------------------------------------------------------

rate_interpretation <-
  margot::margot_interpret_rate(

    rate_results,

    flipped_outcomes =
      flipped_outcomes
  )


# -------------------------------------------------------------------------
# Display RATE tables
# -------------------------------------------------------------------------

cat(
  "\n",
  "RATE-AUTOC results\n",
  "==================\n"
)

print(
  knitr::kable(
    rate_results$rate_autoc,
    digits = 3
  )
)


cat(
  "\n",
  "RATE-Qini results\n",
  "=================\n"
)

print(
  knitr::kable(
    rate_results$rate_qini,
    digits = 3
  )
)


# -------------------------------------------------------------------------
# Display plain-language interpretation
# -------------------------------------------------------------------------

if (
  !is.null(
    rate_interpretation$autoc_results
  )
) {

  cat(
    "\nAUTOC interpretation\n",
    "--------------------\n",
    rate_interpretation$autoc_results,
    "\n"
  )
}


if (
  !is.null(
    rate_interpretation$qini_results
  )
) {

  cat(
    "\nQini interpretation\n",
    "-------------------\n",
    rate_interpretation$qini_results,
    "\n"
  )
}


if (
  !is.null(
    rate_interpretation$comparison
  )
) {

  cat(
    "\nAUTOC versus Qini\n",
    "-----------------\n",
    rate_interpretation$comparison,
    "\n"
  )
}


# -------------------------------------------------------------------------
# Identify outcomes with evidence of heterogeneity
# -------------------------------------------------------------------------

model_groups <- list(

  autoc =
    rate_interpretation$autoc_model_names,

  qini =
    rate_interpretation$qini_model_names,

  either =
    rate_interpretation$either_model_names
)


# Outcomes selected by either heterogeneity metric.

model_keep <-
  model_groups$either

model_keep_autoc <-
  model_groups$autoc

model_keep_qini <-
  model_groups$qini


cat(
  "\nOutcomes retained by AUTOC: ",
  length(model_keep_autoc),
  "\n"
)

cat(
  "Outcomes retained by Qini: ",
  length(model_keep_qini),
  "\n"
)

cat(
  "Outcomes retained by either metric: ",
  length(model_keep),
  "\n"
)


# -------------------------------------------------------------------------
# Save heterogeneity-screening results
# -------------------------------------------------------------------------

saveRDS(

  model_groups,

  here(
    "derived",
    "heterogeneity_model_groups.rds"
  )
)


qs::qsave(

  rate_results,

  here(
    "derived",
    "rate_results.qs"
  )
)


qs::qsave(

  rate_interpretation,

  here(
    "derived",
    "rate_interpretation.qs"
  )
)


# Save machine-readable tables.

readr::write_csv(

  as.data.frame(
    rate_results$rate_autoc
  ),

  here(
    "derived",
    "rate_autoc_results.csv"
  )
)


readr::write_csv(

  as.data.frame(
    rate_results$rate_qini
  ),

  here(
    "derived",
    "rate_qini_results.csv"
  )
)


# -------------------------------------------------------------------------
# Qini-curve settings
# -------------------------------------------------------------------------

decision_tree_defaults <- list(

  span_ratio = 0.30,

  text_size = 3.8,

  y_padding = 0.25,

  edge_label_offset = 0.002,

  border_size = 0.05
)


policy_tree_defaults <- list(

  point_alpha = 0.50,

  title_size = 12,

  subtitle_size = 12,

  axis_title_size = 12,

  legend_title_size = 12
)


# -------------------------------------------------------------------------
# Qini curves
# -------------------------------------------------------------------------
#
# Qini curves visualise the gain from prioritising individuals according
# to estimated treatment effects as the proportion receiving the higher
# exposure increases.
#
# Curves are only generated for outcomes retained by the Qini
# heterogeneity screen.


if (
  length(model_keep_qini) == 0
) {

  message(
    "No outcomes passed the Qini heterogeneity screen."
  )

} else {

  message(
    "Generating Qini curves for ",
    length(model_keep_qini),
    " outcome(s)."
  )


  qini_results <-
    margot::margot_policy(

      models_binary,

      save_plots = FALSE,

      output_dir =
        derived_dir,

      decision_tree_args =
        decision_tree_defaults,

      policy_tree_args =
        policy_tree_defaults,

      model_names =
        model_keep_qini,

      original_df =
        original_df,

      label_mapping =
        outcome_labels,

      max_depth = 2L,

      output_objects = c(
        "qini_plot",
        "diff_gain_summaries"
      )
    )


  # -----------------------------------------------------------------------
  # Extract Qini plots
  # -----------------------------------------------------------------------

  qini_plots <-
    purrr::map(
      qini_results,
      ~ .x$qini_plot
    ) |>
    purrr::compact()


  # -----------------------------------------------------------------------
  # Save Qini results
  # -----------------------------------------------------------------------

  qs::qsave(

    qini_results,

    here(
      "derived",
      "qini_results.qs"
    )
  )


  # -----------------------------------------------------------------------
  # Combine Qini plots for portfolio display
  # -----------------------------------------------------------------------

  if (
    length(qini_plots) > 0
  ) {

    combined_qini <-
      patchwork::wrap_plots(

        qini_plots,

        ncol = min(
          2,
          length(qini_plots)
        )
      ) +

      patchwork::plot_annotation(

        title =
          "Qini Curves for Treatment-Effect Heterogeneity",

        subtitle =
          paste(
            "Targeting gains based on predicted",
            "conditional treatment effects"
          )
      )


    print(
      combined_qini
    )


    # Save a high-resolution portfolio figure.

    ggsave(

      filename = here(
        "figures",
        "qini_curves.png"
      ),

      plot =
        combined_qini,

      width = 10,

      height =
        max(
          5,
          4 * ceiling(
            length(qini_plots) / 2
          )
        ),

      dpi = 300
    )


    message(
      "Qini figure saved to: figures/qini_curves.png"
    )
  }
}


# -------------------------------------------------------------------------
# Summary for next analysis stage
# -------------------------------------------------------------------------

cat(
  "\n",
  "Heterogeneity screening complete.\n",
  "---------------------------------\n",
  "RATE-AUTOC retained: ",
  length(model_keep_autoc),
  " outcome(s)\n",
  "RATE-Qini retained: ",
  length(model_keep_qini),
  " outcome(s)\n",
  "Either metric retained: ",
  length(model_keep),
  " outcome(s)\n",
  sep = ""
)


if (
  length(model_keep) > 0
) {

  cat(
    "\nSelected model names:\n"
  )

  print(
    model_keep
  )

} else {

  cat(
    "\nNo outcomes showed sufficiently strong evidence of useful ",
    "treatment-effect heterogeneity under the selected screening rule.\n",
    sep = ""
  )
}


message(
  "Objects required by 04_policy_tree_analysis.R ",
  "saved to derived/."
)