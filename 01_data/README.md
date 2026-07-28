# Data

This project uses individual participant data from the **CoLaus** and **OsteoLaus** cohort study
(Lausanne, Switzerland).

## Availability

The data used in this analysis are **not openly available**. Access requires approval from
the CoLaus study team and is subject to a data transfer/collaboration agreement.
Data can be requested via the official CoLaus study website:

<https://www.colaus-psycolaus.ch/>

## Expected files

The pipeline (`02_scripts/_targets.R`) expects the following CSV files, referenced by the
`path_targets` list at the top of that script:

| File | Description |
|---|---|
| `Dairy_sarcopenia_base.csv` | CoLaus baseline visit |
| `Dairy_sarcopenia_FU1.csv` | CoLaus follow-up 1 |
| `Dairy_sarcopenia_FU2.csv` | CoLaus follow-up 2 |
| `Dairy_sarcopenia_FU3.csv` | CoLaus follow-up 3 |
| `Dairy_sarcopenia_OstBas.csv` | OsteoLaus baseline visit |
| `Dairy_sarcopenia_OstV2.csv` | OsteoLaus visit 2 |
| `Dairy_sarcopenia_OstV3.csv` | OsteoLaus visit 3 |
| `Dairy_sarcopenia_OstV4.csv` | OsteoLaus visit 4 |
| `Dairy_sarcopenia_OstV5.csv` | OsteoLaus visit 5 |
| `Baseline_additionalFood.csv` | Additional food-frequency items, CoLaus baseline |
| `FU1_additionalFood.csv` | Additional food-frequency items, CoLaus follow-up 1 |
| `FU2_additionalFood.csv` | Additional food-frequency items, CoLaus follow-up 2 |
| `FU3_additionalFood.csv` | Additional food-frequency items, CoLaus follow-up 3 |
| `Deaths.csv` | Mortality/censoring data |

After obtaining access, place these files in this folder (or elsewhere and update the
paths in `02_scripts/_targets.R` accordingly).

## Variable documentation

See [05_supplements/variable_definitions.md](../05_supplements/variable_definitions.md) for
a full description of source variables, derived variables, and how each was used in the
analysis.
