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


# 04_policy_tree_analysis.R
# ------------------------------------------------------------
# Purpose:
#   1. Load outcomes retained by the heterogeneity screen
#   2. Fit shallow policy trees
#   3. Translate heterogeneous treatment effects into
#      simple, interpretable treatment-allocation rules
#   4. Save policy-tree figures for portfolio presentation
#
# Policy trees are fitted only for outcomes showing evidence of useful
# treatment-effect heterogeneity in 03_hte_qini_analysis.R.


# -------------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------------

library(tidyverse)
library(here)
library(qs)
library(margot)
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
# Load fitted causal forests
# -------------------------------------------------------------------------

models_binary <- qs::qread(
  here(
    "derived",
    "causal_forest_models.qs"
  )
)


# Original wide-format data are used for labels and
# back-transformation of covariates.

original_df <- qs::qread(
  here(
    "derived",
    "df_wide.qs"
  )
)


# -------------------------------------------------------------------------
# Load heterogeneity-screening results
# -------------------------------------------------------------------------

model_groups <- readRDS(
  here(
    "derived",
    "heterogeneity_model_groups.rds"
  )
)


# Retain outcomes selected by either RATE-AUTOC or RATE-Qini.

model_keep <- model_groups$either


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
# Policy-tree plotting settings
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
# Check whether any outcomes survived heterogeneity screening
# -------------------------------------------------------------------------

if (
  length(model_keep) == 0
) {

  message(
    "No outcomes passed the heterogeneity screen."
  )

  message(
    "Policy-tree analysis was therefore not performed."
  )

} else {


  # -----------------------------------------------------------------------
  # Fit depth-2 policy trees
  # -----------------------------------------------------------------------
  #
  # Policy trees convert estimated treatment-effect heterogeneity into
  # simple decision rules.
  #
  # A depth-2 tree is deliberately shallow:
  # the aim is interpretability rather than maximum predictive complexity.
  #
  # In this project:
  #
  #   treatment = higher Openness (> 5)
  #
  # The resulting tree identifies covariate-defined groups for whom the
  # estimated benefit of the higher-exposure condition differs.


  message(
    "Fitting policy trees for ",
    length(model_keep),
    " outcome(s)."
  )


  policy_results <-
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
        model_keep,

      original_df =
        original_df,

      label_mapping =
        outcome_labels,

      max_depth = 2L,

      output_objects =
        "combined_plot"
    )


  # -----------------------------------------------------------------------
  # Extract policy-tree figures
  # -----------------------------------------------------------------------

  policy_plots <-
    purrr::map(

      policy_results,

      ~ .x[[1]]
    ) |>
    purrr::compact()


  # -----------------------------------------------------------------------
  # Display individual trees
  # -----------------------------------------------------------------------

  if (
    length(policy_plots) > 0
  ) {

    purrr::walk(
      policy_plots,
      print
    )
  }


  # -----------------------------------------------------------------------
  # Save individual policy-tree figures
  # -----------------------------------------------------------------------

  if (
    length(policy_plots) > 0
  ) {

    plot_names <- names(policy_plots)


    # Use generic names if the returned object is unnamed.

    if (
      is.null(plot_names) ||
      any(plot_names == "")
    ) {

      plot_names <-
        paste0(
          "outcome_",
          seq_along(policy_plots)
        )
    }


    purrr::walk2(

      policy_plots,

      plot_names,

      function(plot_object, plot_name) {


        clean_name <-
          plot_name |>

          stringr::str_remove(
            "^model_"
          ) |>

          stringr::str_remove(
            "^t2_"
          ) |>

          stringr::str_remove(
            "_z$"
          ) |>

          stringr::str_replace_all(
            "[^A-Za-z0-9]+",
            "_"
          ) |>

          stringr::str_to_lower()


        ggsave(

          filename = here(
            "figures",
            paste0(
              "policy_tree_",
              clean_name,
              ".png"
            )
          ),

          plot =
            plot_object,

          width = 8,

          height = 8,

          dpi = 300
        )
      }
    )
  }


  # -----------------------------------------------------------------------
  # Combine policy trees for portfolio presentation
  # -----------------------------------------------------------------------

  if (
    length(policy_plots) > 0
  ) {

    combined_policy_trees <-

      patchwork::wrap_plots(

        policy_plots,

        ncol = min(
          2,
          length(policy_plots)
        )
      ) +

      patchwork::plot_annotation(

        title =
          "Policy Trees for Heterogeneous Treatment Effects",

        subtitle =
          paste(
            "Interpretable allocation rules derived from",
            "causal-forest estimates"
          )
      )


    print(
      combined_policy_trees
    )


    ggsave(

      filename = here(
        "figures",
        "policy_trees_combined.png"
      ),

      plot =
        combined_policy_trees,

      width = 11,

      height =
        max(
          7,
          6 * ceiling(
            length(policy_plots) / 2
          )
        ),

      dpi = 300
    )


    message(
      "Combined policy-tree figure saved to: ",
      "figures/policy_trees_combined.png"
    )
  }


  # -----------------------------------------------------------------------
  # Generate plain-language interpretations
  # -----------------------------------------------------------------------

  policy_interpretation <-

    margot::margot_interpret_policy_batch(

      models_binary,

      model_names =
        model_keep
    )


  cat(
    "\n",
    "Policy-tree interpretation\n",
    "==========================\n",
    policy_interpretation,
    "\n",
    sep = ""
  )


  # -----------------------------------------------------------------------
  # Save results
  # -----------------------------------------------------------------------

  qs::qsave(

    policy_results,

    here(
      "derived",
      "policy_tree_results.qs"
    )
  )


  saveRDS(

    policy_interpretation,

    here(
      "derived",
      "policy_tree_interpretation.rds"
    )
  )


  # -----------------------------------------------------------------------
  # Summary
  # -----------------------------------------------------------------------

  cat(
    "\n",
    "Policy-tree analysis complete.\n",
    "------------------------------\n",
    "Number of outcomes analysed: ",
    length(model_keep),
    "\n",
    sep = ""
  )


  cat(
    "\nModels included:\n"
  )

  print(
    model_keep
  )
}


# -------------------------------------------------------------------------
# Interpretation warning
# -------------------------------------------------------------------------
#
# IMPORTANT:
#
# A variable appearing in a policy-tree split is evidence that the
# variable is useful for predicting treatment-effect heterogeneity.
#
# It does NOT imply that changing that covariate would itself change the
# treatment effect.
#
# For example:
#
#   If household income appears as a split variable, this means income
#   helps identify groups with different estimated treatment responses.
#
# It does not establish a causal effect of income on treatment response.
#
# Policy trees should therefore be interpreted as treatment-allocation
# rules, not as causal models of the moderators themselves.


message(
  "04_policy_tree_analysis.R complete."
)