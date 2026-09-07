# Causal Machine Learning with Simulated NZAVS Data

An applied causal-inference and causal machine-learning project using simulated longitudinal social-science data modelled on the New Zealand Attitudes and Values Study (NZAVS).

The project investigates whether higher **Openness to Experience** is associated with causal changes in warmth toward eight minority groups, and whether estimated treatment effects vary meaningfully across individuals.

> **Portfolio note:** This repository is a cleaned portfolio adaptation of academic coursework. The analysis follows standard causal-inference workflows developed by the EPIC Lab and implemented using the `margot` / `boilerplate` ecosystem. My contribution focused on the study-specific research design, analysis configuration, execution, interpretation, and substantive research question.

---

## Research question

The project asks two related questions:

1. **Average effects:**  
   Under the stated causal-identification assumptions, how would intergroup warmth differ if Openness were set above 5 rather than at or below 5?

2. **Treatment-effect heterogeneity:**  
   Do estimated effects vary across individuals enough for a targeted strategy to potentially outperform a uniform one-size-fits-all approach?

The analysis uses a three-wave longitudinal structure:

```text
Baseline covariates
      ↓
Openness exposure
      ↓
Intergroup warmth outcomes
```

The binary exposure is defined as:

```text
Treatment: Openness > 5
Control:   Openness ≤ 5
```

---

## Data

The analysis uses **simulated longitudinal data modelled on the NZAVS** for instructional purposes.

The portfolio analysis contains:

- 37,560 participants in the baseline cohort
- 21,077 participants in the final causal-forest analysis sample
- 53 baseline covariates
- 8 intergroup-warmth outcomes

The eight outcomes measure warmth toward:

- Asians
- Chinese
- Immigrants
- Indians
- Māori
- Muslims
- Pacific Peoples
- Refugees

The instructional dataset itself is **not redistributed in this repository**.

I previously adapted a related workflow to restricted real NZAVS longitudinal data during psychology research. That version is not included here because the underlying data are subject to access and confidentiality restrictions.

---

## Methods

### Data preparation and attrition adjustment

The longitudinal data are transformed from long to wide format and prepared for causal-forest estimation.

To address loss to follow-up, the analysis constructs **two-stage inverse-probability-of-censoring weights (IPCW)**:

1. Baseline → exposure wave
2. Exposure wave → outcome wave

These are combined with baseline survey-design weights. Extreme combined weights are trimmed at the 99th percentile and normalised.

### Average Treatment Effects

Generalized Random Forests are fitted separately for each of the eight outcomes.

The analysis estimates population-level **Average Treatment Effects (ATEs)** and applies a **Bonferroni correction at α = .05** across the eight outcomes.

### Heterogeneous Treatment Effects

Conditional treatment-effect heterogeneity is evaluated using causal forests and **Rank-Weighted Average Treatment Effects (RATE)**.

Two prioritisation metrics are considered:

- **AUTOC**, which places greater emphasis on the highest-ranked responders
- **Qini**, which evaluates treatment prioritisation across a broader share of the population

Because eight outcomes are screened for heterogeneity, RATE inference uses **false-discovery-rate control at q = .10**.

### Policy trees

For outcomes showing evidence of useful heterogeneity, a shallow depth-2 policy tree is fitted to provide an interpretable representation of treatment-allocation rules.

Policy-tree split variables should be interpreted as **predictors of treatment-effect heterogeneity**, not as evidence that the split variables themselves causally modify the outcome.

---

## Key results

### 1. Average Treatment Effects

After Bonferroni adjustment, none of the eight average treatment effects provided reliable evidence of a population-wide causal effect.

![Average Treatment Effects](figures/ate_effects.png)

This suggests that a uniform intervention based on higher Openness is not strongly supported by the estimated population-average effects.

---

### 2. Treatment-effect heterogeneity

The AUTOC analysis did not identify reliable heterogeneity for any of the eight outcomes after FDR adjustment.

The Qini RATE analysis identified **exploratory evidence of treatment-effect heterogeneity for warmth toward Māori**:

```text
RATE-Qini = 0.015
95% CI    = [0.003, 0.027]
FDR-adjusted p = 0.099
```

No other outcome passed the pre-specified `q = .10` FDR threshold.

![Qini Curve for Warmth Toward Māori](figures/qini_curves.png)

The result illustrates an important distinction between **average effects** and **heterogeneous effects**: a weak or null population-average effect does not necessarily imply that estimated responses are identical across all individuals.

---

### 3. Interpretable policy tree

Because warmth toward Māori passed the Qini heterogeneity screen, a depth-2 policy tree was fitted for this outcome.

The resulting tree used:

- Short Form Health
- Age
- Household income

to form simple subgroups with different model-based treatment recommendations.

![Policy Tree for Warmth Toward Māori](figures/policy_tree_warm_maori.png)

This tree is best interpreted as an **interpretable treatment-prioritisation model**. The appearance of a variable in a split does not establish that changing that variable would itself change the treatment effect.

---

## Analysis workflow

```text
01_data_preparation.R
        │
        ├── longitudinal data preparation
        ├── binary exposure construction
        ├── two-stage IPCW
        └── analysis weights
        ↓
02_ate_analysis.R
        │
        ├── generalized random forests
        ├── average treatment effects
        ├── Bonferroni adjustment
        └── sensitivity summaries
        ↓
03_hte_qini_analysis.R
        │
        ├── RATE-AUTOC
        ├── RATE-Qini
        ├── FDR adjustment
        └── Qini curves
        ↓
04_policy_tree_analysis.R
        │
        └── interpretable policy tree
```

---

## Repository structure

```text
causal-inference-social-psychology/
│
├── README.md
│
├── R/
│   ├── 01_data_preparation.R
│   ├── 02_ate_analysis.R
│   ├── 03_hte_qini_analysis.R
│   └── 04_policy_tree_analysis.R
│
├── figures/
│   ├── ate_effects.png
│   ├── qini_curves.png
│   └── policy_tree_warm_maori.png
│
├── data/
│   └── README.md
│
├── .gitignore
└── .gitattributes
```

Generated model objects and analysis datasets are excluded from version control.

---

## Tools

The project is implemented in **R**.

Main tools include:

- `grf` — generalized random forests and causal forests
- `margot` — EPIC Lab causal-inference workflow
- `tidyverse` — data manipulation and visualisation
- `qs` — efficient storage of intermediate R objects
- `patchwork` — figure composition

The analysis involves:

- longitudinal data preparation
- inverse-probability weighting
- causal machine learning
- average treatment-effect estimation
- heterogeneous treatment-effect analysis
- RATE / Qini evaluation
- multiple-testing adjustment
- policy learning
- statistical visualisation

---

## Reproducibility

The scripts are organised in execution order:

```text
R/01_data_preparation.R
R/02_ate_analysis.R
R/03_hte_qini_analysis.R
R/04_policy_tree_analysis.R
```

The simulated instructional dataset is not bundled with the public repository. With access to the corresponding teaching dataset, the workflow can be reproduced by running the scripts sequentially.

Large derived datasets and fitted model objects are excluded from Git using `.gitignore`.

---

## Interpretation and limitations

This is an observational causal-inference exercise rather than a randomized experiment.

Causal interpretation therefore depends on assumptions including:

- consistency
- positivity
- sufficient control of confounding
- appropriate handling of attrition
- correct temporal ordering

The exposure is also a dichotomised psychological trait rather than a directly administered intervention. Accordingly, the policy-tree results should be treated primarily as a demonstration of **causal machine-learning and treatment-prioritisation methods**, rather than as a recommendation to intervene directly on personality.

The heterogeneity finding for warmth toward Māori is exploratory and uses an FDR threshold of `q = .10`; it should therefore not be interpreted as definitive substantive evidence without independent validation.

---

## Attribution

This project was originally completed as academic coursework using simulated NZAVS-style data.

The causal-inference workflow follows standard protocols developed by the **EPIC Lab** and implemented through the `margot` / `boilerplate` R ecosystem.

This portfolio version reorganises the analysis into readable standalone scripts and highlights my:

- research-design decisions
- study-specific variable configuration
- causal-analysis implementation
- interpretation of statistical results
- documentation and communication of the workflow

It does **not** claim authorship of the underlying EPIC Lab methodology, `margot`, `boilerplate`, `grf`, or `policytree`.

---

## About this portfolio project

This project reflects my broader interest in combining:

**research design + causal inference + statistical modelling + machine learning + substantive domain knowledge**

to answer applied questions where both the method and the limitations of the data matter.