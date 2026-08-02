# Dairy intake and muscle aging in older women: A prospective analysis of data obtained from the OsteoLaus cohort

This repository contains the code, documentation, and analysis pipeline for the project **"Dairy intake and muscle aging in older women: A prospective analysis of data obtained from the OsteoLaus cohort."** The study investigated whether habitual dairy intake is associated with changes in muscle health and the development of sarcopenia in older women using longitudinal data from the OsteoLaus population-based cohort in Lausanne, Switzerland.

## Background

Sarcopenia, the age-related loss of muscle mass, strength, and physical function, is a major cause of disability and loss of independence in older adults. Diet, particularly dairy intake, may help maintain muscle health because dairy products provide high-quality protein and other nutrients important for muscle function. However, evidence from prospective studies remains limited.

This study examined the association between cumulative average dairy intake and four muscle health outcomes:

- Handgrip strength (muscle strength)

- Appendicular lean mass index (ALMI, muscle mass measured by DXA)

- Gait speed (physical performance)

- Incident sarcopenia (EWGSOP2 and FNIH definitions)

Dairy intake was assessed using repeated food frequency questionnaires. Associations were analysed using longitudinal mixed-effects models and Cox proportional hazards models with adjustment for relevant confounders (selected from the causal model in [`02_scripts/DAG.R`](02_scripts/DAG.R)).

## Study design

- **CoLaus**: population-based cohort of men and women aged 35–75 at baseline, recruited 2003–2006 in Lausanne, Switzerland. Four visits — Baseline, F1 (2009–2013), F2 (2014–2018), F3 (2018–2021/2022–2026). Food-frequency questionnaire (dietary exposure) data are available from F1 onwards. CoLaus provides the dairy intake exposure and most covariates.
- **OsteoLaus**: a female sub-cohort nested within CoLaus (age 50–80 at baseline, BMI 15–40 kg/m², with DXA bone density and heel ultrasound data), enrolling 1475 women. Five visits roughly 2.5 years apart — Baseline (2010–2012, n = 1475), V2 (2012–2015, n = 1349), V3 (2015–2018, n = 1242), V4 (2017–2020, n = 1104), V5 (2020–2022, n = 944). OsteoLaus provides the sarcopenia outcome measures (DXA lean mass, handgrip strength, gait speed).

Visits from the two cohorts are matched by participant ID and a fixed visit-pairing scheme (OsteoLaus Baseline↔CoLaus F1, V3↔F2, V4↔F3; OsteoLaus V2 is dropped, V5 is kept as an OsteoLaus-only time point) — see [`02_scripts/R/02_04_visit_match.R`](02_scripts/R/02_04_visit_match.R).

Full variable provenance, cohort eligibility criteria, and merge logic are documented in [04_data_dictionary/variable_definitions.md](04_data_dictionary/variable_definitions.md).

## Analysis pipeline

![Analysis pipeline overview: data preparation, MICE analysis, descriptives, then four parallel outcome branches (handgrip strength, ALMI, gait speed, sarcopenia incidence) each with exclusion, model fitting, and dairy exposure definitions](images/Analysis_Pipeline.png)

The pipeline is orchestrated with [`targets`](https://books.ropensci.org/targets/), defined in [`02_scripts/_targets.R`](02_scripts/_targets.R), and runs in four stages (numbers below match the `02_scripts/R/` file prefixes):

1.  **Data preparation** (`01_*.R`) — import the raw CoLaus/OsteoLaus CSVs, harmonise types and factor levels per visit, run participant-level QC, and stack visits into one long tibble per cohort.
2.  **MICE analysis** (`02_*.R`) — multiple imputation by chained equations (m = 20), derivation of exposure/outcome/covariate variables, column selection, visit matching between cohorts, and derivation of the sarcopenia outcome variables.
3.  **Descriptives** (`05_*.R`) — Table 1, CONSORT participant-flow diagram, missingness analysis, and trajectory plots.
4.  **Outcome-specific modelling** (`03_*.R`, `04_*.R`) — for each of the four outcomes, an outcome-specific exclusion step (`03_exclusion.R`), followed by linear mixed models (continuous outcomes) or Cox proportional hazards models (sarcopenia incidence), each fit against four dairy exposure definitions (cumulative average, quartiles, guideline adherence, sub-categories) and, for Cox models, an additional spline specification. Every MICE-route model is fit on all 20 imputed datasets and pooled using Rubin's rules.

## Repository structure

```         
01_data/            Raw data (not tracked in git, see 01_data/README.md)
02_scripts/
  _targets.R        targets pipeline definition (import → derive → exclude → model)
  DAG.R             Directed acyclic graph of the assumed causal structure
  R/                Pipeline functions, sourced by _targets.R (one file per step)
03_outputs/         Pipeline outputs: tables, figures, logs, model results
04_data_dictionary/ Variable definitions, cohort/merge documentation
05_thesis/          Master's thesis manuscript and presentation
images/             Static images used in this README (e.g. pipeline overview)
renv.lock           Locked R package versions
```

## Requirements

- R 4.5.1
- [renv](https://rstudio.github.io/renv/) for package management
- [targets](https://books.ropensci.org/targets/) for pipeline execution

## Setup

1.  Clone the repository and open `OsteoLaus-Dairy-Sarcopenia.Rproj` in RStudio.

2.  Restore the exact package versions used for the analysis:

    ``` r
    renv::restore()
    ```

3.  Obtain the raw data (see [01_data/README.md](01_data/README.md)) and make it visible to the pipeline, either by:

    - placing the CSVs directly into `01_data/` (works with zero configuration), or
    - keeping them elsewhere and pointing `02_scripts/_targets.R` at that folder via the `DAIRY_DATA_DIR` environment variable — copy `.Renviron.example` (project root) to `.Renviron` and edit it (`.Renviron` is gitignored, so this is per-machine only).

## Running the pipeline

The analysis is orchestrated with `targets`. From the project root:

``` r
targets::tar_make()
```

This runs the full pipeline: data import and harmonization, derivation of exposure/outcome/ covariate variables, multiple imputation (MICE) and complete-case exclusion routes, linear mixed models, Cox models, and descriptive/diagnostic outputs. Targets are cached, so `tar_make()` only re-runs steps affected by code or data changes.

Inspect the pipeline graph before running with:

``` r
targets::tar_visnetwork()
```

## Data availability

The underlying CoLaus and OsteoLaus data are not publicly available due to participant privacy. See [01_data/README.md](01_data/README.md) for how to request access.

## Contact

Anna Boss ([anna.boss\@students.unibe.ch](mailto:anna.boss@students.unibe.ch))
