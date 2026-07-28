# Dairy intake and muscle aging in older women: A prospective analysis of data obtained from the OsteoLaus cohort

Longitudinal analysis of the association between dairy product intake and markers of
sarcopenia — handgrip strength, appendicular lean mass index (ALMI), gait speed,
— in the OsteoLaus population-based cohort (Lausanne, Switzerland).

## Study design

- **CoLaus**: population-based adult cohort, 4 visits (Baseline, F1, F2, F3). Provides
  exposure (dietary, from food-frequency questionnaires) and most covariate data.
- **OsteoLaus**: female sub-cohort of CoLaus (age 50–80 at baseline), 5 visits (Baseline,
  V2–V5). Provides the sarcopenia outcome measures.
- Visits from the two cohorts are matched by participant ID and study visit.

Full variable provenance and merge logic are documented in
[05_supplements/variable_definitions.md](05_supplements/variable_definitions.md).

## Repository structure

```
01_data/          Raw data (not tracked in git, see 01_data/README.md)
02_scripts/
  _targets.R      targets pipeline definition (import → derive → exclude → model)
  DAG.R           Directed acyclic graph of the assumed causal structure
  R/              Pipeline functions, sourced by _targets.R (one file per step)
03_outputs/       Pipeline outputs: tables, figures, logs, model results
04_thesis/        Master's thesis manuscript and presentation
05_supplements/   Variable definitions and analysis notes
renv.lock         Locked R package versions
```

## Requirements

- R 4.5.1
- [renv](https://rstudio.github.io/renv/) for package management
- [targets](https://books.ropensci.org/targets/) for pipeline execution

## Setup

1. Clone the repository and open `OsteoLaus-Dairy-Sarcopenia.Rproj` in RStudio.
2. Restore the exact package versions used for the analysis:

   ```r
   renv::restore()
   ```

3. Obtain the raw data (see [01_data/README.md](01_data/README.md)) and update the file
   paths at the top of `02_scripts/_targets.R` (the `path_targets` list) to point to your
   local copies.

## Running the pipeline

The analysis is orchestrated with `targets`. From the project root:

```r
targets::tar_make()
```

This runs the full pipeline: data import and harmonization, derivation of exposure/outcome/
covariate variables, multiple imputation (MICE) and complete-case exclusion routes, linear
mixed models, Cox models, and descriptive/diagnostic outputs. Targets are cached, so
`tar_make()` only re-runs steps affected by code or data changes.

Inspect the pipeline graph before running with:

```r
targets::tar_visnetwork()
```

## Data availability

The underlying CoLaus and OsteoLaus data are not publicly available due to participant
privacy. See [01_data/README.md](01_data/README.md) for how to request access.

## Contact

Anna Boss ([anna.boss@students.unibe.ch](mailto:anna.boss@students.unibe.ch))
