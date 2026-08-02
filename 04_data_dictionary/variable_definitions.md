# Variable Definition

This document is an overview of all variables provided by the CoLaus\|PsyCoLaus Scientific Committee for this project and how their were used. Each table below documents one variable per row, using the following columns:

- **Original Name**: the variable name as provided in the raw CoLaus/OsteoLaus export.
- **Derived Variable**: name(s) of any downstream variable(s) calculated from this one, so the original can be traced through the pipeline. "None" means no derived variable was created from it.
- **Data Type**: the storage/measurement type of the variable (e.g., integer, Factor, Binary, numeric float, Date).
- **Role**: how the variable was used in the analysis pipeline, e.g., Primary Key, Source (if used as source for a derived variable), Time-Varying Covariate, Fixed Covariant, Outcome, or Excluded if it was not used.
- **Multiplicity**: how many times the variable is expected per participant, expressed as cardinality (e.g., `1..1` once only, `0..*` zero to many visits, `1..*` at least once across visits).
- **Reference / Levels**: the coding scheme for categorical variables (value = meaning), or the unit/definition for continuous ones.
- **Notes**: free-text context on the variable, e.g., what it measures or known data-quality caveats.
- **Available Time Points**: the visit(s) at which the variable was actually recorded. For OsteoLaus lean-mass variables carried over into V5's Hologic/Lunar dual-scanner export, this also notes which scanner column (`H_`/`L_`) supplies the V5 value.
- **Missing value handling**: the rule applied to rows/participants when this variable is missing or invalid.
- **Imputation Notes**: whether and how the variable was imputed by MICE, per `02_01_mice_impute.R` (e.g., not imputed since derived, imputed via pmm/logreg/polr, or excluded from imputation for a specific visit).
- **Export**: Date of when the variable extract was received from the CoLaus\|PsyCoLaus Scientific Committee, where recorded. Multiple dates when different time points were exported to at different occasions.
- **Range**: the observed minimum–maximum value in the analysis dataset, where applicable.

## Cohorts

CoLaus is a population-based cohort of men and women aged 35 to 75 years at baseline, living in Lausanne, Switzerland, recruited between June 2003 and May 2006 at the Centre Hospitalier Universitaire Vaudois. It has four available visit time points: Baseline, F1, F2, and F3, corresponding to follow-up assessments conducted at 2009–2013, 2014–2018, and 2018–2021/2022–2026 respectively. Individual CSV files are provided per time point. Food frequency questionnaire (FFQ) data are available from F1 onwards. CoLaus mainly contains exposure (dairy intake) and covariate variables.

OsteoLaus is a sub-cohort nested within CoLaus, comprising women who participated in the CoLaus baseline examination and were invited to an additional study focused on bone health. It includes female participants only, aged 50 to 80 years at baseline visit, and enrolled 1475 women in total. Eligibility required written informed consent, a BMI between 15 and 40 kg/m², and availability of DXA-derived bone mineral density measurements of the lumbar spine and proximal femur, together with heel quantitative ultrasound measurements and information on previous osteoporotic fractures; women with known diseases affecting bone metabolism or skeletal sites were excluded. OsteoLaus has five visit time points: Baseline (2010–2012, n = 1475), V2 (2012–2015, n = 1349), V3 (2015–2018, n = 1242), V4 (2017–2020, n = 1104), and V5 (2020–2022, n = 944), collected approximately every 2.5 years. Loss to follow-up was mainly due to death, institutionalization, refusal to participate, BMI above 40 kg/m², or severe psychiatric disease. OsteoLaus contains mainly outcome variables and some covariate variables.

## Merge Logic

### Dataset matching

Visits from the CoLaus and OsteoLaus cohorts were aligned using a predefined visit-matching scheme, with OsteoLaus serving as the backbone dataset. OsteoLaus Baseline was paired with CoLaus follow-up 1 (F1), OsteoLaus visit 3 (V3) with CoLaus follow-up 2 (F2), and OsteoLaus visit 4 (V4) with CoLaus follow-up 3 (F3), corresponding to study time points T1, T2, and T3, respectively.

```         
OsteoLaus Baseline  ↔  CoLaus F1   → T1
OsteoLaus V2        →  (discarded, no CoLaus match)
OsteoLaus V3        ↔  CoLaus F2   → T2
OsteoLaus V4        ↔  CoLaus F3   → T3
OsteoLaus V5        →  (OsteoLaus-only)         → T4
```

OsteoLaus visit 2 (V2) was excluded because it had no corresponding CoLaus assessment and was not part of the longitudinal analysis framework. Similarly, the CoLaus baseline visit was excluded. OsteoLaus visit 5 (V5) was retained as an OsteoLaus-only assessment and defined as time point T4, to retain two available time points for gait speed assessment.

Each participant has at most one visit per time point in each study, so matching is a plain inner join on `pt` (the primary key) within each visit pair. Only participants present in both studies for a given visit pair are retained for T1–T3; T4 (V5) requires no CoLaus match since it is OsteoLaus-only.

### Variable precedence

For variables available in both cohorts, CoLaus is retained by default, with the corresponding OsteoLaus value used only when the CoLaus value is missing. Variables collected exclusively at CoLaus visits, such as dairy intake, are retained unchanged. Variables collected exclusively at OsteoLaus visits (e.g., `ALM`) are likewise retained unchanged.

See [02_04_visit_match.R](../02_scripts/R/02_04_visit_match.R) for the implementation.

## Assumptions

- pt is unique and refers to the same participants across time points and study cohorts

- OsteoLaus only includes female participants

- FFQ68amount (ice cream/ sorbet) is assumed to be dairy based intake

## CoLaus Dataset

<table>
<colgroup>
<col style="width: 15%" />
<col style="width: 17%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 15%" />
</colgroup>
<tbody>
<tr>
<td><p><strong>Original Name</strong></p></td>
<td><p><strong>Derived Variable</strong></p></td>
<td><p><strong>Data Type</strong></p></td>
<td><p><strong>Role</strong></p></td>
<td><p><strong>Multiplicity</strong></p></td>
<td><p><strong>Reference / Levels</strong></p></td>
<td><p><strong>Notes</strong></p></td>
<td><p><strong>Available Time Points</strong></p></td>
<td><p><strong>Missing value handling</strong></p></td>
<td><p><strong>Imputation Notes</strong></p></td>
<td><p><strong>Export</strong></p></td>
<td><p><strong>Range</strong></p></td>
</tr>
<tr>
<td><p><strong>pt</strong></p></td>
<td><p>None</p></td>
<td><p>Integer</p></td>
<td><p>Primary Key</p></td>
<td><p>1..1</p></td>
<td><p>NA</p></td>
<td><p>Unique identifier for the CoLaus/OsteoLaus cohort.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>If not recorded unidentified row participant is excluded from analysis</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>sex</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>1..1</p></td>
<td><p>0=female,<br />
1=male</p></td>
<td><p>Excluded since OsteLaus only contains female participants.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>datexam</strong></p></td>
<td><p><code>exam_date_iso</code></p></td>
<td><p>Numeric daily date</p></td>
<td><p>Source</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>In Day–abbreviated month–year DDMonYYYY format, date of physical examination if done</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>exam_date_iso</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>exam_date_iso</strong></p></td>
<td><p><code>Age</code></p></td>
<td><p>Date</p></td>
<td><p>Temporal Key</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>ISO 8601 YYYY-MM-DD format. Join key.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded.</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>datquest</strong></p></td>
<td><p>None</p></td>
<td><p>Date</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Date of questionnaire if done</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>10 May 2022</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>datbirth</strong></p></td>
<td><p><code>Age</code></p></td>
<td><p>Date</p></td>
<td><p>Source</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Birth date collected at baseline</p></td>
<td><p>Baseline</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not used</p></td>
<td><p>10 May 2022</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>age</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated with <code>birthdate</code> and <code>datquest</code></p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>34.9 - 90.1</p></td>
</tr>
<tr>
<td><p><strong>Age</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated with <code>birthdate</code> and <code>exam_iso_date</code></p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, used as a same-visit predictor for most imputed variables, but explicitly excluded from PMM and never itself an imputation target see <code>02_01_mice_impute.R</code>.</p></td>
<td><p>NA</p></td>
<td><p>50.2 - 91.5 merged dataset</p></td>
</tr>
<tr>
<td><p><strong>ethori_self</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..1</p></td>
<td><p>A=Asian,<br />
B=Black / African / African American,<br />
W=White,<br />
O=Other,<br />
X=Unknown / Not reported,<br />
K=does not know</p></td>
<td><p>Self-reported ethnicity.</p></td>
<td><p>Baseline</p></td>
<td><p>Only used as descriptive value, not as covariant, thus participants are kept and values is kept missing</p></td>
<td><p>Not imputed and not used as predictor since only available at baseline</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>edtyp4</strong></p></td>
<td><p><code>education_level</code></p></td>
<td><p>Factor</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>1=University education,<br />
2=High school,<br />
3=Apprenticeship,<br />
4=Mandatory education</p></td>
<td><p>Self-reported highest level of achieved educational level.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>education_level</code></p></td>
<td><p>Imputed polr.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>education_level</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Fixed Covariant</p></td>
<td><p>0..1</p></td>
<td><p>1= Low ISCED 0-2,<br />
2= Medium ISCED 3-4,<br />
3= High ISCED 5-8.</p></td>
<td><p>Mapping: University education= ISCED 6–8 High school Gymnasium / Matura=ISCED 3 Apprenticeship VET=ISCED 3–4 Mandatory education=ISCED 1–2</p></td>
<td><p>Baseline</p></td>
<td><p>Participants with missing values are excluded</p></td>
<td><p>Not imputed since a derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>mrtsts2</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Covariant</p></td>
<td><p>0..*</p></td>
<td><p>0=Living alone,<br />
1=Living in couple</p></td>
<td><p>Marital status</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Participants with missing values are excluded</p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>alcuse</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Initial Question, Participant that said No here should not filled in follow up questions on alcohol usage, but some did anyway</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>alcool4</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=Non Drinker,<br />
1=Drinker</p></td>
<td><p>Categorisation based on <code>conso_hebdo</code> result</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed and not used as predictor since it is a derived variable</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>conso_hebdo</strong></p></td>
<td><p><code>alcool4 (</code>already in the dataset,<code>alcohol_category</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Units/week. Primary source for alcohol_category 1 unit = 10-12g.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>alcohol_category</code></p></td>
<td><p>Imputed, not normally distributed many 0 and long tail</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 108</p></td>
</tr>
<tr>
<td><p><strong>sumalco</strong></p></td>
<td><p><code>alcohol_category</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Alcohol consumption in g ethanol/day</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see <code>alcohol_category</code></p></td>
<td><p>Imputed but not for baseline since missing by design</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>alcohol_category</strong></p></td>
<td><p><code>alcohol_category_imp</code></p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0= Non-drinker 0g/day,<br />
1= Light &gt;0–6g/day,<br />
2= Moderate &gt;6–12g/day,<br />
3= Heavy &gt;12g/day.</p></td>
<td><p>Calculate using <code>conso_hedo</code></p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed nor used as predictor since a derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>sbsmk</strong></p></td>
<td><p><code>smoking_status</code></p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=Never,<br />
1=Former,<br />
2=Current</p></td>
<td><p>Screening question for smoking history.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>smoking_status</code></p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>smoking_status</strong></p></td>
<td><p><code>smoking_status_imp</code></p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=Never,<br />
1=Former,<br />
2=Current</p></td>
<td><p>Correction of values of participants that switch from never to former or current within trajectory or the other way round -&gt; former,</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed nor used as predictor since a derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>dbtld</strong></p></td>
<td><p><code>diabetes_status</code></p></td>
<td><p>Factor</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>Baseline/F1/F3:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes,</p></li>
<li><p>9=Does not know</p></li>
</ul>
<p>F2:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes,</p></li>
<li><p>9=No data</p></li>
</ul></td>
<td><p>Self-reported diagnosis Ever told by doctor.</p>
<p>Different sentinel coding for 9 does not matter since both are recoded to NA</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>diabetes_status</code></p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>agediag_dbts</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Age at diagnosis of diabetes. Could be used to distinguish between Type 1 and Type 2.</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used at all due to high missingness</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>3 - 74</p></td>
</tr>
<tr>
<td><p><strong>dbdrg</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>Baseline/F1/F3:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes,</p></li>
<li><p>9=Does not know</p></li>
</ul>
<p>F2:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes,</p></li>
<li><p>8=Does not apply,</p></li>
<li><p>9=No data</p></li>
</ul></td>
<td><p>Self-reported history of treatment.</p>
<p>Different sentinel coding does not matter since all get recoded to NA.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed — not included in <code>.CL_LOGREG_VARS</code> despite the missingness noted above.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>orldrg</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes,<br />
8=Does not apply,<br />
9=No data</p></td>
<td><p>Use of oral antidiabetic agents.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used due to high missingness</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>insn</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes,<br />
8=Does not apply,<br />
9=No data</p></td>
<td><p>Use of insulin</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used due to high missingness</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>antiDIAB</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>General antidiabetic drug flag.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Used as predictor</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>DIAB</strong></p></td>
<td><p><code>diabetes_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>FPG 7.0 mmol/L. Primary clinical marker.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>diabetes_status</code></p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>DIAB_Hb</strong></p></td>
<td><p><code>diabetes_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>HbA1c 48 mmol/mol. Missing by design, available from F2 onwards.</p></td>
<td><p>F2, F3</p></td>
<td><p>see <code>diabetes_status</code></p></td>
<td><p>Imputed, not for baseline and F1 since missing by design</p></td>
<td><p>14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>DIAB2</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>Baseline/F2/F3:<br />
- 0=Normal,<br />
- 1=IFG,<br />
- 2=Diabetes</p>
<p>F1:<br />
- 0=No,<br />
- 1=Yes</p></td>
<td><p>Combined IGT/Diabetes flag; could be used to verify prediabetes.</p>
<p>Recoding:</p>
<ul>
<li><p>F1 : 0 = No, 1 = Yes -&gt; recoded to 2 = Diabetes</p></li>
<li><p>All others : 0 = Normal, 1 = IFG, 2 = Diabetes</p></li>
</ul></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used since same source as <code>DIAB</code></p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>metab_synd</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Metabolic Syndrome ATP-III. Redundant if using specific diabetes status.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Used as predictor</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>diabetes_status</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Yes if:</p>
<ul>
<li><p>DIAB_Hb == Yes, OR</p></li>
<li><p>DIAB_Hb is missing AND DIAB == Yes, OR</p></li>
<li><p>DIAB_Hb and DIAB are missing AND dbtld == Yes</p></li>
</ul>
<p>otherwise No</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>ATC1...21</strong></p></td>
<td><p><code>hypolip_status</code>, <code>corticoids_status</code>, <code>calcium_status</code>, <code>vitD_status</code>,<code>benzo_status</code></p></td>
<td><p>Character</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Raw ATC codes for all medications reported. Used to derive specific statuses.</p></td>
<td><p>Baseline, F1, F2, F3 individual ATC slots vary in count per visit; ATC1-12 present at all four, ATC13-21 added at F2/F3 as more drugs were reported</p></td>
<td><p>see <code>hypolip_status</code>, <code>corticoids_status</code>, <code>calcium_status</code>, <code>vitD_status</code>,<code>benzo_status</code></p></td>
<td><p>Not imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>ATC_OTC1...17</strong></p></td>
<td><p>None</p></td>
<td><p>Character</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Over the counter drugs. Excluded since no standard formulation available.</p></td>
<td><p>F1, F2, F3 OTC codes not collected at Baseline; ATC_OTC1-11 from F1, ATC_OTC12-17 added at F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>Diet_compl</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total number of dietary complements</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 7</p></td>
</tr>
<tr>
<td><p><strong>hypolip_drug_status</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate &amp; Validation</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC code starts with C10.</p>
<p>Used to verify <code>hypolip</code> or <code>hctld</code>.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, computed from raw ATC codes, not itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>corticoids_status</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC code starts with H02.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, computed from raw ATC codes, not itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>vitD_status</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC starts with A11</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, computed from raw ATC codes, not itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>calcium_status</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Yes 1: If any ATC code starts with A12A incl. A12AX.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, computed from raw ATC codes, not itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>benzo_status</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC starts with N05B</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, computed from raw ATC codes, not itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>miac</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Myocardial Infarction history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>strk</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Stroke history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>chf</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Congestive Heart Failure history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>cad</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Coronary Artery Disease history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>angn</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Angina history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>cmp</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Cardiomyopathy history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>hdc</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Hypertensive heart disease.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>hdv</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Heart valve disease.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>artm</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Arrhythmia.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>vslg</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Vascular surgery history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>ccth</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Cardiac catheterization history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>cabg</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Bypass surgery CABG history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>pcin</strong></p></td>
<td><p><code>cdv_event</code></p></td>
<td><p>Factorial</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Stent PCI history.</p></td>
<td><p>Baseline</p></td>
<td><p>see <code>cdv_event</code></p></td>
<td><p>Imputed logreg.</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>agediag...</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Ages for all CV diagnoses listed above.</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not imputed and not used as predictor</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>depending on variable</p></td>
</tr>
<tr>
<td><p><strong>cvdbase_adj</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Validation of <code>cdv_event</code></p></td>
<td><p>0..1</p></td>
<td><p>1=yes</p></td>
<td><p>Personal histories of previous CVD have been validated</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used due to high missingness</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>cdv_event</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..1</p></td>
<td><p>0=No,<br />
1=yes</p></td>
<td><p>Available only at baseline for available data. Final Composite: Yes if any CV flag is positive.</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>crbpmed</strong></p></td>
<td><p><code>htn_status</code></p></td>
<td><p>Factor</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes,<br />
8=Not relevant,<br />
9=Does not know</p></td>
<td><p>Self-report: Do you take medication for high BP?</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>htn_status</code></p></td>
<td><p>Imputed logreg, except at F2 where imputation is explicitly disabled <code>where[,"crbpmed_F2"] &lt;- FALSE</code> — F2 missing values are kept as NA.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>antiHTA</strong></p></td>
<td><p><code>htn_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Documented antihypertensive drug treatment.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>htn_status</code></p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>HTA</strong></p></td>
<td><p><code>htn_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p><strong>Hypertension:</strong> Yes if BP 140/90 mmHg OR on treatment <code>antiHTA</code>.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>htn_status</code></p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>htn_status</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Covariant</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><ul>
<li><p>htn_status = Yes if HTA == Yes</p></li>
<li><p>htn_status = Yes if HTA missing AND antiHTA == Yes</p></li>
<li><p>htn_status = Yes if HTA and antiHTA missing AND crbpmed == Yes</p></li>
<li><p>htn_status = No if at least one source is non-NA and none indicate Yes</p></li>
<li><p>htn_status = NA if all three sources are NA</p></li>
</ul></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed since derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>hctld</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>Baseline/ F1/F3:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes,</p></li>
<li><p>9=Does not know</p></li>
</ul>
<p>F2:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes,</p></li>
<li><p>9=No data</p></li>
</ul></td>
<td><p>Self-report: Ever told you had high cholesterol.</p>
<p>Different sentinel coding does not matter since both are recoded to NA</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>hypolip</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Hypolipidemic treatment flag.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>esthrp</strong></p></td>
<td><p><code>hrt_status</code></p></td>
<td><p>Factor</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes,<br />
9=Does not know</p></td>
<td><p>Self-report: Ever taken HRT. Baseline Screening.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>hrt_status</code></p></td>
<td><p>imputed, high missingness &gt; 47%</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>esthrpage</strong></p></td>
<td><p><code>hrt_status</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>99= Does not know</p></td>
<td><p>Start age for HRT. Use to verify if HRT was active at Baseline.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>hrt_status</code></p></td>
<td><p>imputed, high missingness &gt; 82%</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>hrt_status</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>1..*</p></td>
<td><p>1 = Never,<br />
2= Former,<br />
3 = Current.</p></td>
<td><p>Calculation of hormonalreplacement therapy:</p>
<ul>
<li><p>hrt_status = Current HRT if esthrp == Yes</p></li>
<li><p>hrt_status = Past HRT if esthrp == No and esthrpage is non-missing and non-sentinel</p></li>
<li><p>hrt_status = Never / Not current if esthrp == No and no past-use evidence</p></li>
<li><p>hrt_status = NA when esthrp is NA</p></li>
</ul></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed since derived from imputed <code>esthrp</code>/<code>esthrpage</code>, not itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>handgrip</strong> renamed to HGS_MAX to match OsteoLaus naming</p></td>
<td><p><code>ewgsop2_sarcopenia_stage</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Outcome</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Peak force measured in UK pounds and transformed to kg</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>2.28 - 88.45</p></td>
</tr>
<tr>
<td><p><strong>lateralite</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>1=Right,<br />
2=Left,<br />
3=Ambidextrous</p></td>
<td><p>hand preference</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>used as predictor</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>handgrip_com</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>Baseline:</p>
<ul>
<li><p>0=No,</p></li>
<li><p>1=Yes -&gt; recoded to 3= unspecified issue</p></li>
</ul>
<p>F2/F3:</p>
<ul>
<li><p>0=No problem,</p></li>
<li><p>1=Pain/arthrosis,</p></li>
<li><p>2=No time, home, rejected</p></li>
</ul></td>
<td><p>Qualitative notes e.g., pain in wrist.</p></td>
<td><p>Baseline, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>ht</strong> renamed to <code>Height</code> to match naming in OsteoLaus</p></td>
<td><p><code>BMI</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Height in cm.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>130.0 - 199.0</p></td>
</tr>
<tr>
<td><p><strong>wt</strong> renamed to <code>Weight</code> to match naming in OsteoLaus</p></td>
<td><p><code>BMI</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Weight in kg.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>34.2 - 175.4</p></td>
</tr>
<tr>
<td><p><strong>BMI</strong></p></td>
<td><p><code>BMI_category</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated as weight/height<sup>2</sup> .</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>see <code>BMI_category</code></p></td>
<td><p>Not imputed and not used as predictor since it is a derived variable</p></td>
<td><p>NA</p></td>
<td><p>13.7 - 79.7</p></td>
</tr>
<tr>
<td><p><strong>BMI_category</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>1=Underweight<br />
2=Normal,<br />
3=Overweight,<br />
4=Obese. Normal is reference</p></td>
<td><p>Mapping:<br />
&lt; 18.5 = Underweight,<br />
18.5 - &lt; 25.0 Normal reference<br />
25.0 – &lt; 30.0 = Overweight,<br />
&gt;= 30.0 = Obese</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed and not used as predictor since it is a derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>WHR</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Waist-to-Hip ratio; marker of central adiposity.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0.55 - 2.40</p></td>
</tr>
<tr>
<td><p><strong>bmpsc</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Bioimpedance Analysis BIA % Fat Mass.</p></td>
<td><p>Baseline, F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 64.9</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_SE</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sedentary time min/day.</p></td>
<td><p>F1, F2</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 1050</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_SE_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time spent sedentary.</p></td>
<td><p>F1, F2</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 100</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_LPA</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Light PA min/day.</p></td>
<td><p>F1, F2</p></td>
<td><p>Excluded</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 1019</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_LPA_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time in Light PA.</p></td>
<td><p>F1, F2</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 100</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_MPA</strong></p></td>
<td><p><code>met_min_week</code>, <strong><code>m</code></strong><code>vpa_min_day</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Moderate PA min/day.</p></td>
<td><p>F1, F2</p></td>
<td><p>see <code>met_min_week</code></p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 990</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_MPA_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time in Moderate PA.</p></td>
<td><p>F1, F2</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 100</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_VPA</strong></p></td>
<td><p><code>met_min_week</code>, <strong><code>m</code></strong><code>vpa_min_day</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vigorous PA min/day</p></td>
<td><p>F1, F2</p></td>
<td><p>see <code>met_min_week</code></p></td>
<td><p>imputed, not normally distributed long tail and many low numbers</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 692</p></td>
</tr>
<tr>
<td><p><strong>PAFQ_VPA_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time in Vigorous PA.</p></td>
<td><p>F1, F2</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 72.1</p></td>
</tr>
<tr>
<td><p><strong>met_min_week</strong></p></td>
<td><p><code>pa_levels_who_f1</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculation: PAFQ_MPA * 4 + PAFQ_VPA * 8 * 7</p></td>
<td><p>F1, F2</p></td>
<td><p>see <code>pa_levels_who_f1</code></p></td>
<td><p>Not used since derived variables</p></td>
<td><p>NA</p></td>
<td><p>0 - 1468</p></td>
</tr>
<tr>
<td><p><strong>mvpa_min_day</strong></p></td>
<td><p><code>pa_levels_tertiles_f1</code>, <code>pa_levels_tertiles_f2</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculation: PAFQ_MPA + PAFQ_VPA</p></td>
<td><p>F1, F2</p></td>
<td><p>see</p></td>
<td><p>Not used since derived variables</p></td>
<td><p>NA</p></td>
<td><p>0 - 1004</p></td>
</tr>
<tr>
<td><p><strong>pa_levels_tertiles_f1</strong></p></td>
<td><p>None</p></td>
<td><p>Factorial</p></td>
<td><p>Fixed Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p><code>mvpa_min_day</code> range divided into three categorise from F1 time point: low, moderate, and high</p></td>
<td><p>Categories of physical activity defined by tertiles based on cohort distributions of <code>met_min_week</code></p></td>
<td><p>NA</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not used since derived variables</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>pa_levels_tertiles_f2</strong></p></td>
<td><p>None</p></td>
<td><p>Factorial</p></td>
<td><p>Fixed Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p><code>met_min_week</code> range divided into three categorise from F2 time point: low, moderate, and high</p></td>
<td><p>Categories of physical activity defined by tertiles based on cohort distributions of <code>met_min_week</code></p></td>
<td><p>NA</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not used since derived variables</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>pa_levels_who_f1</strong></p></td>
<td><p>None</p></td>
<td><p>Factorial</p></td>
<td><p>Fixed Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>1=Low &lt; 600 MET-min/week,<br />
2=Moderate 600-2999 MET-min/week,<br />
3=High &gt;= 3000 MET-min/week</p></td>
<td><p>Categories of physical activity defined by WHO definition using <code>met_min_week</code> values from F1</p></td>
<td><p>NA</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variables</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>mnwlk</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Walking time minutes/day.</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>0 - 720</p></td>
</tr>
<tr>
<td><p><strong>phyact</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exclude</p></td>
<td><p>0..</p></td>
<td><p>0=never,<br />
1=once a week,<br />
2=Twice a week,<br />
9=Does not know</p></td>
<td><p>Physical activity min 20 min/week.</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>FFQ1amount</strong></p></td>
<td><p><code>dairy_total_gday</code>,<code>dairy_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Plain yogurt g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 675</p></td>
</tr>
<tr>
<td><p><strong>FFQ2amount</strong></p></td>
<td><p><code>dairy_total_gday</code>,<code>dairy_fermented_gday</code>, <code>dairy_lowfat_gday, dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Low-fat yogurt g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 675</p></td>
</tr>
<tr>
<td><p><strong>FFQ3amount</strong></p></td>
<td><p><code>dairy_total_gday</code>,<code>dairy_fermented_gday</code>,<code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Fruit/aroma yogurt g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 675</p></td>
</tr>
<tr>
<td><p><strong>FFQ4amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_fermented_gday</code>, <code>dairy_lowfat_gday, dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cottage cheese 0% g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 500</p></td>
</tr>
<tr>
<td><p><strong>FFQ5amount</strong></p></td>
<td><p><code>dairy_total_gday</code>,<code>dairy_fermented_gday</code>,<code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cottage cheese/ricotta g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 375</p></td>
</tr>
<tr>
<td><p><strong>FFQ6amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Feta/mozzarella g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 225</p></td>
</tr>
<tr>
<td><p><strong>FFQ7amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Gruyère/tomme/camembert g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 300</p></td>
</tr>
<tr>
<td><p><strong>FFQ8amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cheese fondue g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 1312.5</p></td>
</tr>
<tr>
<td><p><strong>FFQ52amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Butter g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 37.5</p></td>
</tr>
<tr>
<td><p><strong>FFQ53amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cream 35% g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 57.5</p></td>
</tr>
<tr>
<td><p><strong>FFQ63amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code>,</p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cream tart/cake g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 250</p></td>
</tr>
<tr>
<td><p><strong>FFQ68amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Ice cream/sorbet g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 450</p></td>
</tr>
<tr>
<td><p><strong>FFQ71amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Butter for cooking g/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 37.5</p></td>
</tr>
<tr>
<td><p><strong>FFQ82amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_lowfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk in coffee 0% mL/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 113</p></td>
</tr>
<tr>
<td><p><strong>FFQ83amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk in coffee non-0% mL/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 113</p></td>
</tr>
<tr>
<td><p><strong>FFQ84amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Coffee creamer mL/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 45</p></td>
</tr>
<tr>
<td><p><strong>FFQ85amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_lowfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk drink 0% mL/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 563</p></td>
</tr>
<tr>
<td><p><strong>FFQ86amount</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_portion_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk drink non-0% mL/day.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 563</p></td>
</tr>
<tr>
<td><p><strong>dairy_total_gday</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of amount of all dairy products</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>0 - 2089</p></td>
</tr>
<tr>
<td><p><strong>dairy_fermented_gday</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of yogurt and cheese amount variables</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>0 - 2089</p></td>
</tr>
<tr>
<td><p><strong>dairy_non_fermented_gday</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of all non fermented dairy products</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>0 - 1070</p></td>
</tr>
<tr>
<td><p><strong>dairy_lowfat_gday</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of all 0% / Low-fat yogurt and milk products</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>0 - 1363</p></td>
</tr>
<tr>
<td><p><strong>dairy_highfat_gday</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of all whole-milk, cheese, and cream products</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>0 - 1984</p></td>
</tr>
<tr>
<td><p><strong>Dairy</strong></p></td>
<td><p><code>Dairy_OK</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total freq excl. butter/cream.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used since derived variable</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 12.5</p></td>
</tr>
<tr>
<td><p><strong>Dairy_OK</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Adherence to Swiss guidelines, amount of servings:</p>
<ul>
<li><p>0: &lt;3/day,</p></li>
<li><p>1: &gt;=3/day</p></li>
</ul></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used since derived variable</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>dairy_portion_total</strong></p></td>
<td><p><code>dairy_guidelines_port</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total freq excl. butter/cream adjusted with portion size:</p>
<ul>
<li><p>small: 0.5</p></li>
<li><p>normal: 1.0</p></li>
<li><p>large: 1.5</p></li>
</ul></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>0 - 11.5</p></td>
</tr>
<tr>
<td><p><strong>dairy_guidelines_port</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Adherence to Swiss guidelines, amount of servings:</p>
<ul>
<li><p>0: &lt;3/day,</p></li>
<li><p>1: &gt;=3/day</p></li>
</ul></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>*_cumavg</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_guidelines_port</code>, <code>dairy_quartile_baseline</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculating the cumulative average for each dairy variable by averaging all available values up to and including the current assessment.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>depending on variable</p></td>
</tr>
<tr>
<td><p><strong>*_lag</strong></p></td>
<td><p><code>dairy_total_gday</code>, <code>dairy_non_fermented_gday</code>, <code>dairy_highfat_gday</code>, <code>dairy_guidelines_port</code>, <code>dairy_quartile_baseline</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculating the lagged cumulative average by excluding the current assessment from the cumulative average.</p></td>
<td><p>NA</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>depending on variable</p></td>
</tr>
<tr>
<td><p><strong>dairy_quartile_baseline</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>From lowest quartile Q1 to highest quartile Q4</p></td>
<td><p>Cut offs defined by <code>total_dairy_gday_cumavg</code> at F1 and applied to all time points.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>FFQ1...86</strong></p></td>
<td><p><code>FFQfreq</code> already in raw dataset</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>1=Never in the past 4 weeks,<br />
2=Once per month,<br />
3=2 to 3 times per month,<br />
4=1 to 2 times per week,<br />
5=3 to 4 times per week,<br />
6=Once per day,<br />
7=2 or more times per day</p></td>
<td><p>1-7 levels, Response frequency code. Redundant since <code>amount</code> is already calculated.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed — redundant raw response code; <code>FFQ...amount</code>/<code>freqFFQ</code>/<code>FFQp</code> are the imputed targets instead.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>FFQp1...p86</strong></p></td>
<td><p><code>FFQ...amount</code> already in raw dataset</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>1=Less,<br />
2=Equal,<br />
3=More</p></td>
<td><p>1-3 levels, Portion size code. Redundant since <code>amount</code> is already calculated.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Imputed pmm; values not normally distributed.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>freqFFQ1...86</strong></p></td>
<td><p><code>FFQ...amount</code> already in raw dataset</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated daily frequency. Redundant since <code>amount</code> is already calculated.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Imputed pmm; values not normally distributed.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>depending on variable</p></td>
</tr>
<tr>
<td><p><strong>sumtot1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total energy incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>Visits with missing/ invalid values are excluded</p>
<p>Invalid values: below 500 or above 4200</p></td>
<td><p>impute</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 9745</p></td>
</tr>
<tr>
<td><p><strong>sumtot3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total Energy excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 9450</p></td>
</tr>
<tr>
<td><p><strong>sumprot1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total protein incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 2242</p></td>
</tr>
<tr>
<td><p><strong>sumprot3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total protein excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 2242</p></td>
</tr>
<tr>
<td><p><strong>sumpveg1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vegetal protein incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 397</p></td>
</tr>
<tr>
<td><p><strong>sumpveg3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vegetal protein excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 367</p></td>
</tr>
<tr>
<td><p><strong>sumpani1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Animal protein incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 2097</p></td>
</tr>
<tr>
<td><p><strong>sumpani3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Animal protein excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 2097</p></td>
</tr>
<tr>
<td><p><strong>sumgluc1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total carbs incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Imputed pmm.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 4293</p></td>
</tr>
<tr>
<td><p><strong>sumgluc3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total carbs excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 3893</p></td>
</tr>
<tr>
<td><p><strong>sumlipi1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total fat incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Imputed pmm.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 5255</p></td>
</tr>
<tr>
<td><p><strong>sumlipi3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total fat excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 4843</p></td>
</tr>
<tr>
<td><p><strong>sumvitd1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vitamin D incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>Not imputed.</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 64.2</p></td>
</tr>
<tr>
<td><p><strong>sumvitd3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vitamin D intake excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 64.2</p></td>
</tr>
<tr>
<td><p><strong>pct_prot1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Protein incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 72.7</p></td>
</tr>
<tr>
<td><p><strong>pct_prot3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Protein excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>4.1 - 217 unrealistic outlier</p></td>
</tr>
<tr>
<td><p><strong>pct_pveg1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Veg. protein incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 72.7</p></td>
</tr>
<tr>
<td><p><strong>pct_pveg3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Veg. protein excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 217 unrealistic outlier</p></td>
</tr>
<tr>
<td><p><strong>pct_pani1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Ani. protein incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 84.7</p></td>
</tr>
<tr>
<td><p><strong>pct_pani3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Ani. protein excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 61.2</p></td>
</tr>
<tr>
<td><p><strong>pct_gluc1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Carbs incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 69.2</p></td>
</tr>
<tr>
<td><p><strong>pct_gluc3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Carbs excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 1188 unrealistic outlier</p></td>
</tr>
<tr>
<td><p><strong>pct_lipi1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Fat incl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 69.2</p></td>
</tr>
<tr>
<td><p><strong>pct_lipi3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Fat excl. alc.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 69.7</p></td>
</tr>
<tr>
<td><p><strong>pct_alco1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Alcohol as % of energy intake.</p></td>
<td><p>F1, F2, F3</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 14 Apr 2026</p></td>
<td><p>0 - 97.1</p></td>
</tr>
</tbody>
</table>

## OsteoLaus Dataset

OsteoLaus has 5 measurement time points labelled Baseline, V2, V3, V4, and V5. For each of these time point are following variables available.

<table>
<colgroup>
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
</colgroup>
<tbody>
<tr>
<td><p><strong>Original Name</strong></p></td>
<td><p><strong>Derived Variable</strong></p></td>
<td><p><strong>Data Type</strong></p></td>
<td><p><strong>Role</strong></p></td>
<td><p><strong>Multiplicity</strong></p></td>
<td><p><strong>Reference / Levels</strong></p></td>
<td><p><strong>Notes</strong></p></td>
<td><p><strong>Available Time Points</strong></p></td>
<td><p><strong>Missing value handling</strong></p></td>
<td><p><strong>Imputation Notes</strong></p></td>
<td><p><strong>Export</strong></p></td>
<td><p><strong>Range</strong></p></td>
</tr>
<tr>
<td><p><strong>pt</strong></p></td>
<td><p>None</p></td>
<td><p>Integer</p></td>
<td><p>Primary Key</p></td>
<td><p>1..1</p></td>
<td><p>NA</p></td>
<td><p>Unique identifier for the CoLaus/OsteoLaus cohort.</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>if not recorded unidentified row is excluded from analysis</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>id_pat/ PATIENT_KEY</strong></p></td>
<td><p>None</p></td>
<td><p>Character</p></td>
<td><p>Excluded</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Additional ID to <code>pt</code> in OsteoLaus dataset</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>Ethnicity</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>1..1</p></td>
<td><p>1=White,2=?, 3=Other</p></td>
<td><p>Redundant since <code>ethori_self</code> from CoLaus.</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>Ethnicity_other</strong></p></td>
<td><p>None</p></td>
<td><p>Character</p></td>
<td><p>Excluded</p></td>
<td><p>1..1</p></td>
<td><p>NA</p></td>
<td><p>No information</p></td>
<td><p>Baseline</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SCAN_date</strong></p></td>
<td><p><code>exam_date_iso</code></p></td>
<td><p>numeric daily date</p></td>
<td><p>Source</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Date of DXA/Physical visit. Required to link to OsteoLaus measurement time points to CoLaus measurement time points. In Day–abbreviated month–year DDMonYYYY format</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>see <code>exam_date_iso</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>exam_date_iso</strong></p></td>
<td><p>None</p></td>
<td><p>Date</p></td>
<td><p>Temporal Key</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>ISO 8601 YYYY-MM-DD format. Join key.</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not used</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>Age</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated with <code>birthdate</code> and <code>exam_iso_date</code></p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, used as a same-visit predictor for most imputed variables, but explicitly excluded from PMM and never itself an imputation target see <code>02_01_mice_impute.R</code>.</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>50.2 - 91.5</p></td>
</tr>
<tr>
<td><p><strong>Weight</strong></p></td>
<td><p><code>BMI</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Height in cm.</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>36 - 120</p></td>
</tr>
<tr>
<td><p><strong>Height</strong></p></td>
<td><p><code>BMI</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Weight in kg.</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>138 - 183</p></td>
</tr>
<tr>
<td><p><strong>BMI</strong></p></td>
<td><p><code>BMI_category</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated as weight/height<sup>2</sup> . Already provided in data.</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>see <code>BMI_category</code></p></td>
<td><p>not used since derived variables</p></td>
<td><p>NA</p></td>
<td><p>15.1 - 45.5</p></td>
</tr>
<tr>
<td><p><strong>BMI_category</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>1=Underweight<br />
2=Normal,<br />
3=Overweight,<br />
4=Obese. Normal is reference</p></td>
<td><p>Mapping:<br />
&lt; 18.5 = Underweight,<br />
18.5 - &lt; 25.0 Normal reference<br />
25.0 – &lt; 30.0 = Overweight,<br />
&gt;= 30.0 = Obese</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variables</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>HEAD_LEAN_MASS</strong> H_HEAD_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Recalculated Total - Subtotal in grams.</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_HEAD_LEAN_MASS; Lunar L_HEAD_LEAN_MASS also exists at V5 but is unused</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>1855 - 4207</p></td>
</tr>
<tr>
<td><p><strong>LARM_LEAN_MASS</strong> H_LARM_LEAN_MASS for V5</p></td>
<td><p><code>ALM</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Left arm lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_LARM_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>1068 - 3533</p></td>
</tr>
<tr>
<td><p><strong>RARM_LEAN_MASS</strong> H_RARM_LEAN_MASS for V5</p></td>
<td><p><code>ALM</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Right arm lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_RARM_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>1132 - 3533</p></td>
</tr>
<tr>
<td><p><strong>ARMS_LEAN_MASS</strong> H_ARMS_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Arms lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_ARMS_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>2347 - 7066</p></td>
</tr>
<tr>
<td><p><strong>LLEG_LEAN_MASS</strong> H_LLEG_LEAN_MASS for V5</p></td>
<td><p><code>ALM</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Left leg lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_LLEG_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>3033 - 10804</p></td>
</tr>
<tr>
<td><p><strong>RLEG_LEAN_MASS</strong> H_RLEG_LEAN_MASS for V5</p></td>
<td><p><code>ALM</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Right leg lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_RLEG_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>2843 - 11424</p></td>
</tr>
<tr>
<td><p><strong>LEGS_LEAN_MASS</strong> H_LEGS_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Legs lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_LEGS_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>6528 - 21609</p></td>
</tr>
<tr>
<td><p><strong>TRUNK_LEAN_MASS</strong> H_TRUNK_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Lean mass of the torso in g.</p></td>
<td><p>V3, V4, V5 V5 via H_TRUNK_LEAN_MASS; not measured at Baseline/V2</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>11647 - 25984</p></td>
</tr>
<tr>
<td><p><strong>LTRUNK_LEAN_MASS</strong> H_LTRUNK_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Left trunk lean mass g.</p></td>
<td><p>V3, V4, V5 V5 via H_LTRUNK_LEAN_MASS; not measured at Baseline/V2</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>5724 - 12959</p></td>
</tr>
<tr>
<td><p><strong>RTRUNK_LEAN_MASS</strong> H_RTRUNK_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Right trunk lean mass g.</p></td>
<td><p>V3, V4, V5 V5 via H_RTRUNK_LEAN_MASS; not measured at Baseline/V2</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>5661 - 13025</p></td>
</tr>
<tr>
<td><p><strong>SUBTOT_LEAN_MASS</strong> H_SUBTOT_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Whole body less head lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_SUBTOT_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>21860 - 60195</p></td>
</tr>
<tr>
<td><p><strong>WBTOT_LEAN_MASS</strong> H_WBTOT_LEAN_MASS for V5</p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Whole body lean mass g</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_WBTOT_LEAN_MASS</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>24116 - 63,072</p></td>
</tr>
<tr>
<td><p><strong>LTOTAL_LEAN_MASS</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Left total lean mass g.</p></td>
<td><p>V3, V4 only no Hologic V5 equivalent; Lunar-only L_LTOTAL_LEAN_MASS exists at V5 but is unused</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>12276 - 26615</p></td>
</tr>
<tr>
<td><p><strong>RTOTAL_LEAN_MASS</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Right total lean mass g.</p></td>
<td><p>V3, V4 only no Hologic V5 equivalent; Lunar-only L_RTOTAL_LEAN_MASS exists at V5 but is unused</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>12155 - 26603</p></td>
</tr>
<tr>
<td><p><strong>ANDROID_LEAN_MASS</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Android lean mass g</p></td>
<td><p>Baseline, V2, V3, V4 only no Hologic V5 equivalent; Lunar-only L_ANDROID_LEAN_MASS exists at V5 but is unused</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>1600 - 5912</p></td>
</tr>
<tr>
<td><p><strong>GYNOID_LEAN_MASS</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Gynoid lean mass g</p></td>
<td><p>Baseline, V2, V3, V4 only no Hologic V5 equivalent; Lunar-only L_GYNOID_LEAN_MASS exists at V5 but is unused</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>3096 - 10243</p></td>
</tr>
<tr>
<td><p><strong>AND_plus_GYN_LEAN_MASS</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Lean mass in grams AND + GYN.</p></td>
<td><p>Baseline, V2 only not collected/derived at V3, V4, or V5 in either scanner</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>17 Mar 2026</p></td>
<td><p>4724 - 16155</p></td>
</tr>
<tr>
<td><p><strong>ALM</strong></p></td>
<td><p><code>ALM_harmonised</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Appendicular lean mass in grams Arms + Legs.</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_ALM</p></td>
<td><p>NA</p></td>
<td><p>Not imputed, derived post-imputation as the sum of imputed <code>LARM</code>/<code>RARM</code>/<code>LLEG</code>/<code>RLEG_LEAN_MASS</code>; never itself a MICE target.</p></td>
<td><p>NA</p></td>
<td><p>9062 - 27682</p></td>
</tr>
<tr>
<td><p><strong>ALM_harmonised</strong></p></td>
<td><p><code>ALM_HT2_harmonised</code>, <code>ALM_BMI_harmonised</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Appendicular lean mass harmonised between the two DXA devices</p></td>
<td><p>Baseline, V2, V3, V4, V5</p></td>
<td><p>NA</p></td>
<td><p>Not imputed since derived value</p></td>
<td><p>NA</p></td>
<td><p>9062 - 27682</p></td>
</tr>
<tr>
<td><p><strong>ALM_HT2_harmonised</strong></p></td>
<td><p><code>ewgsop2_sarcopenia_stage</code></p></td>
<td><p>Numeric</p></td>
<td><p>Outcome</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sarcopenia Marker: ALM/Height² in kg/m<sup>2.</sup></p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_ALM_HT2</p></td>
<td><p>Visits with missing/ invalid values are excluded for ALM analysis</p></td>
<td><p>not used</p></td>
<td><p>NA</p></td>
<td><p>3.8 - 10.9</p></td>
</tr>
<tr>
<td><p><strong>ALM_WT_harmonised</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Appendicular lean mass of arms + legs kg /weight kg.</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_ALM_WT</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>NA</p></td>
<td><p>0.17 - 0.38</p></td>
</tr>
<tr>
<td><p><strong>ALM_BMI_harmonised</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Sensitive analysis</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>ALM scaled by BMI.</p></td>
<td><p>Baseline, V2, V3, V4, V5 V5 via H_ALM_BMI</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>NA</p></td>
<td><p>0.35 - 1.1</p></td>
</tr>
<tr>
<td><p><strong>TUG_GETUP</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=?,<br />
1=?</p>
<p>not known</p></td>
<td><p>Timed Up and Go: 1-Get up from a chair with crossed hands on chest, 0-help with.</p></td>
<td><p>V4, V5</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>TUG_GO</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=stop or uncomplete,<br />
1= 1-Walk 3 meters</p></td>
<td><p>Timed Up and Go: 1-Walk 3 meters, 0-stop or uncomplete.</p></td>
<td><p>V4, V5</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>TUG_TURN</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=dysbalance, stop or uncomplete,<br />
1=Turn back</p></td>
<td><p>Timed Up and Go: 1-Turn back, 0-dysbalance, stop or uncomplete.</p></td>
<td><p>V4, V5</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>TUG_GOBACKSIT</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=?,<br />
1=Come back 3m and sit</p></td>
<td><p>Timed Up and Go: 1-Come back 3m and sit, 0-if help with hands or no braking during sit.</p></td>
<td><p>V4, V5</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>TUG_TIME</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>-</p></td>
<td><p>Time to complete Timed Up and Go in seconds.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>6.6 - 40.3</p></td>
</tr>
<tr>
<td><p><strong>TUG_SCORE</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=?,<br />
1=?,<br />
2=?</p>
<p>unknown<br />
</p></td>
<td><p>Timed Up and Go: Total of points: 0-1-2-3-4.</p></td>
<td><p>V4, V5</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>6MGS</strong> renamed to gait_speed to conform to R language</p></td>
<td><p><code>ewgsop2_sarcopenia_stage</code></p></td>
<td><p>Numeric</p></td>
<td><p>Outcome</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Gait Speed: 6-meter walk time in m/s.</p></td>
<td><p>V4, V5</p></td>
<td><p>Visits with missing/ invalid values are excluded for gait speed analysis</p></td>
<td><p>impute, normally distribution, only for V4/V5</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>0.30 - 1.7</p></td>
</tr>
<tr>
<td><p><strong>PHYSTEST_COMMENT</strong></p></td>
<td><p>None</p></td>
<td><p>Character</p></td>
<td><p>Exclude</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Comment on Timed Up and Go or Gait Speed. High misingness and not needed for main analysis.</p></td>
<td><p>V4, V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SARCF_STRENGHT</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>0=None,<br />
1=Some,<br />
2=A lot or unable</p></td>
<td><p>SARC-F: Difficulty in lifting and carrying 10 pounds. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SARCF_WALK</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>None=0,<br />
Some=1,<br />
2=A lot, use aids, or unable</p></td>
<td><p>SARC-F: Difficulty walking across a room. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SARCF_CHAIR</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>0=None,<br />
1=Some,<br />
2=A lot or unable</p></td>
<td><p>SARC-F: Difficulty transferring from a chair or bed.Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SARCF_STAIRS</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>0=None,<br />
1=Some,<br />
2=A lot or unable</p></td>
<td><p>SARC-F: Difficulty climbing a flight of 10 stairs. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SARCF_FALL</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>0=None,<br />
1=1-3 falls,<br />
2=4+ falls</p></td>
<td><p>SARC-F: How many times have you fallen in the past year. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>SARCF_TOTAL</strong></p></td>
<td><p>None</p></td>
<td><p>Integer</p></td>
<td><p>Excluded</p></td>
<td><p>0..1</p></td>
<td><p>unknown</p></td>
<td><p>Sarcopenia Screening: total of each other items. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>HGS_R1</strong></p></td>
<td><p><code>HGS_MAX</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Hand grip 1st measure on right hand measured if hand is dominant. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>see <code>HGS_MAX</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>0 - 34</p></td>
</tr>
<tr>
<td><p><strong>HGS_R2</strong></p></td>
<td><p><code>HGS_MAX</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Hand grip 2st measure on right hand measured if hand is dominant. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>see <code>HGS_MAX</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>0 - 38</p></td>
</tr>
<tr>
<td><p><strong>HGS_R3</strong></p></td>
<td><p><code>HGS_MAX</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Hand grip 3st measure on right hand measured if hand is dominant. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>see <code>HGS_MAX</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>0 - 38</p></td>
</tr>
<tr>
<td><p><strong>HGS_L1</strong></p></td>
<td><p><code>HGS_MAX</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Hand grip 1st measure on left hand measured if hand is dominant. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>see <code>HGS_MAX</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>2 - 33</p></td>
</tr>
<tr>
<td><p><strong>HGS_L2</strong></p></td>
<td><p><code>HGS_MAX</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Hand grip 2st measure on left hand measured if hand is dominant. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>see <code>HGS_MAX</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>4 - 40</p></td>
</tr>
<tr>
<td><p><strong>HGS_L3</strong></p></td>
<td><p><code>HGS_MAX</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Hand grip 3st measure on left hand measured if hand is dominant. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>see <code>HGS_MAX</code></p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>3 - 36</p></td>
</tr>
<tr>
<td><p><strong>HGS_MAX</strong></p></td>
<td><p><code>ewgsop2_sarcopenia_stage</code></p></td>
<td><p>Numeric</p></td>
<td><p>Outcome</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Max Grip Strength Peak of 6 measures in kg. Only available for V5.</p></td>
<td><p>V5</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>imputed</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>3 - 40</p></td>
</tr>
<tr>
<td><p><strong>L_...LEAN_MASS/ L_ALM_...</strong></p></td>
<td><p>None</p></td>
<td><p>multiple data types</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>multiple values for lean mass from Lunar DXA for V5, will not be used</p></td>
<td><p>V5 only Lunar-scanner lean-mass variables; not used in analysis</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>ewgsop2_sarcopenia_stage</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Outcome</p></td>
<td><p>0..*</p></td>
<td><p>0=No sarcopenia, 1=Probable sarcopenia, 2=Confirmed Sarcopenia, 3=Severe sarcopenia</p></td>
<td><p>Mapping:</p>
<ul>
<li><p>no</p></li>
<li><p>probable: handgrip strength&lt;16 kg for women</p></li>
<li><p>confirmed: criteria of probable + low muscle quantity or quality. &lt;5.5 kg/m² women</p></li>
<li><p>severe: criteria of confirmed + &lt;= 0.8 m/s</p></li>
</ul></td>
<td><p>Baseline, V2, V3, V4, V5 severity tiers limited by component availability: gait-speed-based severe tier only computable from V4 onward</p></td>
<td><p>for Aim 2: participant with missing baseline value are excluded</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>FNIH_sarcopenia</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Secondary Outcome</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Sarcopenia</p></td>
<td><p>FNIH criteria for sarcopenia for sensitivity analysis, Sarcopenia when grip strength &lt; 16 kg AND ALM/BMI &lt; 0.512</p></td>
<td><p>Baseline, V2, V3, V4, V5 via merged CoLaus handgrip for earlier visits; OsteoLaus-native grip strength only at V5</p></td>
<td><p>for Aim 3: participant with missing baseline value are excluded</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
<tr>
<td><p><strong>DXA_method</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Source</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>To distinguish the method used to measure ALM</p></td>
<td><p>Baseline, V2, V3, V4, V5 available for all</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td><p>5 Mar 2026; 17 Mar 2026</p></td>
<td><p>NA</p></td>
</tr>
</tbody>
</table>
