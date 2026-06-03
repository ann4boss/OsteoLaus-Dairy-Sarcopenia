# Variable Definition

This document is an overview of all variables provided by the research team for this project and how their were used. The column "Derived Variables" shows which variables were derived from the original source for traceability. The "Role" explains how it was used in the analysis pipeline. <!--# continue -->

## Cohorts

CoLaus is a population-based cohort of adults from Lausanne, Switzerland. It has four available visit time points: Baseline, F1, F2, and F3. Individual CSV files are provided per time point. Food frequency questionnaire (FFQ) data are available from F1 onwards. CoLaus mainly contains exposure and covariate variables.

OsteoLaus is a sub-cohort of CoLaus including female participants only from age 50 to 80 years at baseline visit. It has five visit time points: Baseline, V2, V3, V4, and V5. OsteoLaus contains mainly outcome variables and some covariate variables.

## Merge Logic

CoLaus visits are mapped to the closest OsteoLaus time point (one-to-many).

```         
CoLaus (Baseline–F4) ──── mapped to nearest OsteoLaus wave ──── exam_date_iso (join key)
        │
        │  merge on: pt  (1:1 within wave)
        │
OsteoLaus (Baseline–V5) 
```

- The `pt` variable is the **primary key** (participant-level unique identifier).
- `exam_date_iso` (ISO 8601 date) is the **temporal join key**.
- OsteoLaus → CoLaus mapping is **many-to-one**: multiple OsteoLaus visits may link to one CoLaus wave → use the most recent CoLaus visits compared to OsteoLaus visit regardless it is before or after.
- CoLaus visits that were not matched to any OsteoLaus visit were retained for completeness

## Assumptions

- pt is unique and refers to the same participants across time points and study cohorts

- OsteoLaus only includes female participants

- FFQ68amount (ice cream/ sorbet) is assumed to be dairy based intake

## CoLaus Dataset

<table>
<colgroup>
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 22%" />
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
<td><p><strong>Missing value handling</strong></p></td>
<td><p><strong>Imputation Notes</strong></p></td>
<td><p><strong>Export</strong></p></td>
</tr>
<tr>
<td><p><strong>pt</strong></p></td>
<td><p>None</p></td>
<td><p>integer</p></td>
<td><p>Primary Key</p></td>
<td><p>1..1</p></td>
<td><p>NA</p></td>
<td><p>Unique identifier for the CoLaus/OsteoLaus cohort.</p></td>
<td><p>If not recorded unidentified row participant is excluded from analysis</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td><p>Thursday March 5 08:11:43</p></td>
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
<td><p>NA</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>datexam</strong></p></td>
<td><p><code>exam_date_iso</code></p></td>
<td><p>numeric daily date</p></td>
<td><p>Source</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>In Day–abbreviated month–year DDMonYYYY format</p></td>
<td><p>see <code>exam_date_iso</code></p></td>
<td><p>Not imputed and not used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>exam_date_iso</strong></p></td>
<td><p><code>Age</code></p></td>
<td><p>Date</p></td>
<td><p>Temporal Key</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>ISO 8601 YYYY-MM-DD format. Join key.</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>datquest</strong></p></td>
<td><p>None</p></td>
<td><p>Date</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Date of questionnaire if done</p></td>
<td><p>NA</p></td>
<td><p>NA</p></td>
<td><p>Tuesday May 10 09:21:06 2022</p></td>
</tr>
<tr>
<td><p><strong>datbirth</strong></p></td>
<td><p><code>Age</code></p></td>
<td><p>Date</p></td>
<td><p>Source</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Birth date collected at baseline</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td><p>Tuesday May 10 09:20:56 2022</p></td>
</tr>
<tr>
<td><p><strong>age</strong> renamed to <code>Age</code> to fit naming of OsteoLaus</p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Not sure how it is calculated</p></td>
<td><p>NA</p></td>
<td><p>Imputed and used as predictor, not necessarily normally distributed, more like a block</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>Age</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated with birthdate and exam_iso_date</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed nor used as predictor since a derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>ethori_self</strong></p></td>
<td><p>None</p></td>
<td><p>Factor</p></td>
<td><p>Descriptive</p></td>
<td><p>0..1</p></td>
<td><p>A=Asian,<br />
B=Black / African / African American,<br />
W=White,<br />
O=Other,<br />
X=Unknown / Not reported,<br />
K=does not know</p></td>
<td><p>Only recorded at baseline</p></td>
<td><p>Only used as descriptive value, not as covariant, thus participants are kept and values is kept missing</p></td>
<td><p>Not imputed and not used as predictor since only available at baseline</p></td>
<td></td>
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
<td><p>Self-reported highest level of achieved educational level, only recorded at baseline</p></td>
<td><p>see <code>education_level</code></p></td>
<td><p>Imputed only at baseline</p></td>
<td></td>
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
<td><p>Participants with missing values are excluded</p></td>
<td><p>Not imputed nor used as predictor since a derived variable</p></td>
<td></td>
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
<td><p>Participants with missing values are excluded</p></td>
<td><p>Imputed</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Not imputed, used as predictor</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Not imputed and not used as predictor since it is a derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>conso_hebdo</strong></p></td>
<td><p><code>alcool4</code>already in the dataset,<code>alcohol_category</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Units/week. Primary source for alcohol_category 1 unit = 10-12g.</p></td>
<td><p>see <code>alcohol_category</code></p></td>
<td><p>Imputed, not normally distributed many 0 and long tail</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumalco</strong></p></td>
<td><p><code>alcohol_category</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Missing by design for baseline. Alcohol consumption in g ethanol/day</p></td>
<td><p>see <code>alcohol_category</code></p></td>
<td><p>Imputed but not for baseline since missing by design</p></td>
<td></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed nor used as predictor since a derived variable</p></td>
<td></td>
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
<td><p>see <code>smoking_status</code></p></td>
<td><p>Imputed</p></td>
<td></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed nor used as predictor since a derived variable</p></td>
<td></td>
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
<td><p>see <code>diabetes_status</code></p></td>
<td><p>Imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>agediag_dbts</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Age at diagnosis of diabetes. Could be used to distinguish between Type 1 and Type 2.</p></td>
<td><p>NA</p></td>
<td><p>Not used at all due to high missingness</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Imputed but has high missingness: Baseline: 31.4%, F1: 91.8%, F2: 89.9%, F3: 90.9%</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Not used due to high missingness</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Not used due to high missingness</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>DIAB</strong></p></td>
<td><p><code>diabetes_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>FPG <span class="math inline">≥</span> 7.0 mmol/L. Primary clinical marker.</p></td>
<td><p>see <code>diabetes_status</code></p></td>
<td><p>Imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>DIAB_Hb</strong></p></td>
<td><p><code>diabetes_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>HbA1c <span class="math inline">≥</span> 48 mmol/mol. Missing by design, available from F2 onwards.</p></td>
<td><p>see <code>diabetes_status</code></p></td>
<td><p>Imputed, not for baseline and F1 since missing by design</p></td>
<td></td>
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
<p>Recoding ??.</p></td>
<td><p>NA</p></td>
<td><p>Not used since same source as <code>DIAB</code></p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Used as predictor</p></td>
<td></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
</tr>
<tr>
<td><p><strong>ATC1...21</strong></p></td>
<td><p><code>hypolip_status</code>, <code>corticoids_status</code>, <code>calcium_status</code>, <code>vitD_status</code>,<code>benzo_status</code></p></td>
<td><p>Character</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Raw ATC codes for all medications reported. Used to derive specific statuses.</p></td>
<td><p>see <code>hypolip_status</code>, <code>corticoids_status</code>, <code>calcium_status</code>, <code>vitD_status</code>,<code>benzo_status</code></p></td>
<td><p>only derived variables can be imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>ATC_OTC1...17</strong></p></td>
<td><p>None</p></td>
<td><p>Character</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Over the counter drugs. Excluded since no standard formulation available.</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>Diet_compl</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total number of dietary complements</p></td>
<td><p>NA</p></td>
<td><p>Not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>hypolip_drug_status</strong></p></td>
<td><p><code>hypolip_drug_status_imp</code></p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate &amp; Validation</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC code starts with C10.</p>
<p>Used to verify <code>hypolip</code> or <code>hctld</code>. </p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
</tr>
<tr>
<td><p><strong>corticoids_status</strong></p></td>
<td><p><code>corticoids_status_imp</code></p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC code starts with H02.</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
</tr>
<tr>
<td><p><strong>vitD_status</strong></p></td>
<td><p><code>vitD_status_imp</code></p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC starts with A11</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
</tr>
<tr>
<td><p><strong>calcium_status</strong></p></td>
<td><p><code>calcium_status_imp</code></p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Yes 1: If any ATC code starts with A12A incl. A12AX.</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
</tr>
<tr>
<td><p><strong>benzo_status</strong></p></td>
<td><p><code>benzo_status_imp</code></p></td>
<td><p>Binary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>If any ATC starts with N05B</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
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
<td><p>see <code>cdv_event</code></p></td>
<td><p>imputed only at baseline</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>agediag...</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Ages for all CV diagnoses listed above.</p></td>
<td><p>NA</p></td>
<td><p>Not imputed and not used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>cvdbase_adj</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Validation of <code>cdv_event</code></p></td>
<td><p>0..1</p></td>
<td><p>1=yes</p></td>
<td><p>Personal histories of previous CVD have been validated</p></td>
<td><p>NA</p></td>
<td><p>Not used due to high missingness</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>Not used since derived variable</p></td>
<td></td>
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
<td><p>see <code>htn_status</code></p></td>
<td><p>imputed, F1-F3 a bit high missingness</p></td>
<td></td>
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
<td><p>see <code>htn_status</code></p></td>
<td><p>imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>HTA</strong></p></td>
<td><p><code>htn_status</code></p></td>
<td><p>Binary</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p><strong>Hypertension:</strong> Yes if BP <span class="math inline">≥</span> 140/90 mmHg OR on treatment <code>antiHTA</code>.</p></td>
<td><p>see <code>htn_status</code></p></td>
<td><p>imputed</p></td>
<td></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not imputed since derived variable</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>hypolip</strong></p></td>
<td><p>None</p></td>
<td><p>Binary</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Missing at baseline by design. Hypolipidemic treatment flag F1 onwards.</p></td>
<td><p>NA</p></td>
<td><p>used as predictor not available for baseline and high missingness for F2/F3</p></td>
<td></td>
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
<td><p>see <code>hrt_status</code></p></td>
<td><p>imputed, high missingness 47%+</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>esthrpage</strong></p></td>
<td><p><code>hrt_status</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>99= Does not know</p></td>
<td><p>Start age for HRT. Use to verify if HRT was active at Baseline.</p></td>
<td><p>see <code>hrt_status</code></p></td>
<td><p>imputed, high missingness 82%+</p></td>
<td></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td></td>
<td></td>
</tr>
<tr>
<td><p><strong>handgrip </strong>renamed to HGS_MAX to match OsteoLaus naming</p></td>
<td><p><code>HGS_MAX_imp</code>, <code>ewgsop2_sarcopenia_stage</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Primary Outcome</p></td>
<td><p>1..*</p></td>
<td><p>NA</p></td>
<td><p>Peak force measured in UK pounds and transformed to kg</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>imputed</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>used as predictor</p></td>
<td></td>
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
<td><p>Qualitative notes e.g., pain in wrist. Not available for F1</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>ht</strong> renamed to <code>Height</code> to match naming in OsteoLaus</p>
<p></p></td>
<td><p><code>BMI</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Height in cm.</p></td>
<td><p>NA</p></td>
<td><p>impute</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>wt</strong> renamed to <code>Weight</code> to match naming in OsteoLaus</p></td>
<td><p><code>BMI</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Weight in kg.</p></td>
<td><p>NA</p></td>
<td><p>impute</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>BMI</strong></p></td>
<td><p><code>BMI_category</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated as weight/height<sup>2</sup> .</p></td>
<td><p>see <code>BMI_category</code></p></td>
<td><p>Not imputed and not used as predictor since it is a derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>BMI_category</strong></p></td>
<td><p><code>BMI_category_imp</code></p></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>Not imputed and not used as predictor since it is a derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>WHR</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Waist-to-Hip ratio; marker of central adiposity.</p></td>
<td><p>NA</p></td>
<td><p>used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>bmpsc</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Bioimpedance Analysis BIA % Fat Mass.</p></td>
<td><p>NA</p></td>
<td><p>imputed and used as predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_SE</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sedentary time min/day.</p></td>
<td><p>see <code>met_min_week</code></p></td>
<td><p>predictor and imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_SE_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time spent sedentary.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_LPA</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Light PA min/day.</p></td>
<td><p>see <code>met_min_week</code></p></td>
<td><p>predictor and imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_LPA_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time in Light PA.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_MPA</strong></p></td>
<td><p><code>met_min_week</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Moderate PA min/day.</p></td>
<td><p>see <code>met_min_week</code></p></td>
<td><p>imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_MPA_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time in Moderate PA.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_VPA</strong></p></td>
<td><p><code>met_min_week</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vigorous PA min/day</p></td>
<td><p>see <code>met_min_week</code></p></td>
<td><p>imputed, not normally distributed long tail and many low numbers</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>PAFQ_VPA_pct</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% of daily time in Vigorous PA.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>met_min_week</strong></p></td>
<td><p><code>pa_levels</code></p></td>
<td><p>Numeric</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculation: PAFQ_MPA * 4 + PAFQ_VPA * 8 * 7</p></td>
<td><p>see <code>pa_levels</code></p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pa_levels_tertiles</strong></p></td>
<td><p><code>pa_levels_quartiles_imp</code></p></td>
<td><p>Factorial</p></td>
<td><p>Fixed Varying Covariate</p>
<p></p></td>
<td><p>0..*</p></td>
<td><p></p></td>
<td><p>Categories of physical activity defined by tertiles based on cohort distributions of <code>met_min_week</code></p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variables</p></td>
<td></td>
</tr>
<tr>
<td><p>pa_levels_who</p>
<p></p></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td><p>1=Low &lt; 600 MET-min/week,<br />
2=Moderate 600-2999 MET-min/week,<br />
3=High &gt;= 3000 MET-min/week</p></td>
<td></td>
<td></td>
<td><p>not used since derived variables</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>mnwlk</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exclude</p></td>
<td><p>0..1</p></td>
<td><p>NA</p></td>
<td><p>Walking time minutes/day, only for Baseline available</p></td>
<td><p>NA</p></td>
<td><p>predictor only at baseline</p></td>
<td></td>
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
<td><p>Physical activity min 20 min/week, only available for Baseline</p></td>
<td><p>NA</p></td>
<td><p>predictor only at baseline</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ1amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_highfat</code><strong>,</strong> <code>dairy_fermented, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Plain yogurt g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ2amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>, <code>dairy_lowfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Low-fat yogurt g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ3amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>,<code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Fruit/aroma yogurt g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ4amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>, <code>dairy_lowfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cottage cheese 0% g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ5amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>,<code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cottage cheese/ricotta g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ6amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>, <code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Feta/mozzarella g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ7amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>, <code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Gruyère/tomme/camembert g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ8amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_fermented</code>, <code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cheese fondue g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ52amount</strong></p></td>
<td><p><code>dairy_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Butter g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ53amount</strong></p></td>
<td><p><code>dairy_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cream 35% g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ63amount</strong></p></td>
<td><p><code>dairy_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Cream tart/cake g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ68amount</strong></p></td>
<td><p><code>dairy_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Ice cream/sorbet g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ71amount</strong></p></td>
<td><p><code>dairy_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Butter for cooking g/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ82amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_non_fermented</code>, <code>dairy_lowfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk in coffee 0% mL/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ83amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_non_fermented</code>, <code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk in coffee non-0% mL/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ84amount</strong></p></td>
<td><p><code>dairy_total</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Coffee creamer mL/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ85amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_non_fermented</code>, <code>dairy_lowfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk drink 0% mL/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>FFQ86amount</strong></p></td>
<td><p><code>dairy_total</code>, <code>dairy_non_fermented</code>, <code>dairy_highfat``, Dairy_serving</code></p></td>
<td><p>Numeric float</p></td>
<td><p>Source</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Milk drink non-0% mL/day.</p></td>
<td><p>see derived variables</p></td>
<td><p>imputed, not normally distributed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_total</strong></p></td>
<td><p><code>dairy_total_imp</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of amount of all dairy products</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_total_imp</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Missing Values are imputed with previous visit or next value</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_fermented</strong></p></td>
<td><p><code>dairy_fermented_imp</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of yogurt and cheese amount variables, FFQ: 1, 2, 3, 4, 5, 6, 7, 8 </p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_fermented_imp</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Missing Values are imputed with previous visit or next value</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_non_fermented</strong></p></td>
<td><p><code>dairy_non_fermented_imp</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of all non fermented dairy products, FFQ: 82, 83, 85, 86</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_non_fermented_imp</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Missing Values are imputed with previous visit or next value</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_lowfat</strong></p></td>
<td><p><code>dairy_lowfat_imp</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of all 0% / Low-fat yogurt and milk products, FFQ: 2, 4, 82, 85</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_lowfat_imp</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Missing Values are imputed with previous visit or next value</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_highfat</strong></p></td>
<td><p><code>dairy_highfat_imp</code></p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Sum of all whole-milk, cheese, and cream products, FFQ: 1, 3, 5, 6, 7, 8, 83, 86</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_highfat_imp</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Exposure</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Missing Values are imputed with previous visit or next value</p></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>Dairy</strong></p></td>
<td><p><code>Dairy_OK</code> already in raw dataset</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total freq excl. butter/cream.</p></td>
<td><p>NA</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p>dairy_portion_total</p></td>
<td><p>dairy_guidelines_port</p></td>
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
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>dairy_guidelines_port</strong></p></td>
<td><p>None</p></td>
<td><p>BInary</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>0=No,<br />
1=Yes</p></td>
<td><p>Adherence to Swiss guidelines, amount of servings:</p>
<ul>
<li><p>0: &lt;3/day,</p></li>
<li><p>1: &gt;=3/day</p></li>
</ul></td>
<td><p>Visits with missing/ invalid values are excluded</p></td>
<td><p>not used since derived variable</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>impute, none are normally distributed, not for baseline</p></td>
<td></td>
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
<td><p>NA</p></td>
<td><p>impute, none are normally distributed, not for baseline</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>freqFFQ1...86</strong></p></td>
<td><p><code>FFQ...amount</code> already in raw dataset</p></td>
<td><p>Numeric float</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Calculated daily frequency. Redundant since <code>amount</code> is already calculated.</p></td>
<td><p>NA</p></td>
<td><p>impute, none are normally distributed, not for baseline</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumtot1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Time-Varying Covariate</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total energy incl. alc.</p></td>
<td><p>Visits with missing/ invalid values are excluded</p>
<p>Invalid values: below 500 or above 4200</p></td>
<td><p>impute</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumtot3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total Energy excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumprot1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Inactive</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total protein incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>imputed</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumprot3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total protein excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumpveg1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vegetal protein incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumpveg3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vegetal protein excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumpani1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Animal protein incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumpani3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Animal protein excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumgluc1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Inactive</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total carbs incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumgluc3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total carbs excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumlipi1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Inactive</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total fat incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumlipi3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Total fat excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumvitd1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Inactive</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vitamin D incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>predictor</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>sumvitd3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Vitamin D intake excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_prot1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Protein incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_prot3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Protein excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_pveg1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Veg. protein incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_pveg3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Veg. protein excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_pani1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Ani. protein incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_pani3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Ani. protein excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_gluc1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Carbs incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_gluc3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Carbs excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_lipi1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Fat incl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_lipi3</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>% Fat excl. alc.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
<tr>
<td><p><strong>pct_alco1</strong></p></td>
<td><p>None</p></td>
<td><p>Numeric</p></td>
<td><p>Excluded</p></td>
<td><p>0..*</p></td>
<td><p>NA</p></td>
<td><p>Alcohol as % of energy intake.</p></td>
<td><p>NA</p></td>
<td><p>not used</p></td>
<td></td>
</tr>
</tbody>
</table>

## OsteoLaus Dataset

OsteoLaus has 5 measurement time points labelled Baseline, V2, V3, V4, and V5. For each of these time point are following variables available. In case a variable was not measured for the entire cohort for a time point it is mentioned in the "Notes" column. FFQ variables are available for the first follow up measurement.

+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
+===========================================================+====================================+=====================+========================+==================+=====================================================================================+=======================================================================================================================================================================+==========================================================================+===============================================+
| **Original Name**                                         | **Derived Variable**               | **Data Type**       | **Role**               | **Multiplicity** | **Reference / Levels**                                                              | **Notes**                                                                                                                                                             | **Missing value handling**                                               | **Imputation Notes**                          |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **pt**                                                    | None                               | integer             | Primary Key            | 1..1             | NA                                                                                  | Unique identifier for the CoLaus/OsteoLaus cohort.                                                                                                                    | if not recorded unidentified row is excluded from analysis               | Not imputed, used as predictor                |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **id_pat/ PATIENT_KEY**                                   | None                               | character           | Excluded               | 0..1             | NA                                                                                  | Additional ID to `pt` in OsteoLaus dataset                                                                                                                            | NA                                                                       | Not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **Ethnicity**                                             | None                               | Factor              | Excluded               | 1..1             | 1=White,2=?, 3=Other                                                                | Redundant since `ethori_self` from CoLaus.                                                                                                                            | NA                                                                       | Not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **Ethnicity_other**                                       | None                               | Character           | Excluded               | 1..1             | NA                                                                                  | No information                                                                                                                                                        | NA                                                                       | Not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SCAN_date**                                             | `exam_date_iso`                    | numeric daily date  | Source                 | 1..\*            | NA                                                                                  | Date of DXA/Physical visit. Required to link to OsteoLaus measurement time points to CoLaus measurement time points. In Day–abbreviated month–year (DDMonYYYY) format | see `exam_date_iso`                                                      | Not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **exam_date_iso**                                         | None                               | Date                | Temporal Key           | 1..\*            | NA                                                                                  | ISO 8601 (YYYY-MM-DD) format. Join key.                                                                                                                               | Visits with missing/ invalid values are excluded                         | Not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **Age**                                                   | None                               | Numeric (float)     | Time-Varying Covariate | 1..\*            | NA                                                                                  | Calculated at each `dataexam` with `birthdate` (not available). Continuous variable. For OsteoLaus generic birth dates were sometimes used                            | Visits with missing/ invalid values are excluded                         | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **Weight**                                                | `BMI`                              | Numeric (float)     | Excluded               | 0..\*            | NA                                                                                  | Height in cm.                                                                                                                                                         | NA                                                                       | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **Height**                                                | `BMI`                              | Numeric (float)     | Excluded               | 0..\*            | NA                                                                                  | Weight in kg.                                                                                                                                                         | NA                                                                       | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **BMI**                                                   | `BMI_category`                     | Numeric (float)     | Source                 | 0..\*            | NA                                                                                  | Calculated as weight/height^2^ . Already provided in data.                                                                                                            | see `BMI_category`                                                       | not used since derived variables              |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **BMI_category**                                          | None                               | Factor              | Time-Varying Covariate | 0..\*            | 1=Underweight\                                                                      | Mapping:\                                                                                                                                                             | Visits with missing/ invalid values are excluded                         | not used since derived variables              |
|                                                           |                                    |                     |                        |                  | 2=Normal,\                                                                          | \< 18.5 = Underweight,\                                                                                                                                               |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 3=Overweight,\                                                                      | 18.5 - \< 25.0 Normal (reference)\                                                                                                                                    |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 4=Obese. (Normal is reference)                                                      | 25.0 – \< 30.0 = Overweight,\                                                                                                                                         |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     | \>= 30.0 = Obese                                                                                                                                                      |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HEAD_LEAN_MASS (**H_HEAD_LEAN_MASS for V5**)**          | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Recalculated (Total - Subtotal) in grams.                                                                                                                             | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **LARM_LEAN_MASS** (H_LARM_LEAN_MASS for V5)              | `ALM`                              | Numeric             | Source                 | 0..\*            | NA                                                                                  | Left arm lean mass (g)                                                                                                                                                | NA                                                                       | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **RARM_LEAN_MASS** (H_RARM_LEAN_MASS for V5)              | `ALM`                              | Numeric             | Source                 | 0..\*            | NA                                                                                  | Right arm lean mass (g)                                                                                                                                               | NA                                                                       | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ARMS_LEAN_MASS** (H_ARMS_LEAN_MASS for V5)              | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Arms lean mass (g)                                                                                                                                                    | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **LLEG_LEAN_MASS** (H_LLEG_LEAN_MASS for V5)              | `ALM`                              | Numeric             | Source                 | 0..\*            | NA                                                                                  | Left leg lean mass (g)                                                                                                                                                | NA                                                                       | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **RLEG_LEAN_MASS** (H_RLEG_LEAN_MASS for V5)              | `ALM`                              | Numeric             | Source                 | 0..\*            | NA                                                                                  | Right leg lean mass (g)                                                                                                                                               | NA                                                                       | imputed                                       |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **LEGS_LEAN_MASS** (H_LEGS_LEAN_MASS for V5)              | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Legs lean mass (g)                                                                                                                                                    | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TRUNK_LEAN_MASS** (H_TRUNK_LEAN_MASS for V5)            | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Lean mass of the torso in gram. Available for V3/V4/V5                                                                                                                | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **LTRUNK_LEAN_MASS** (H_LTRUNK_LEAN_MASS for V5)          | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Left trunk lean mass (g). Available for V3/V4/V5                                                                                                                      | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **RTRUNK_LEAN_MASS** (H_RTRUNK_LEAN_MASS for V5)          | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Right trunk lean mass (g). Available for V3/V4/V5                                                                                                                     | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SUBTOT_LEAN_MASS** (H_SUBTOT_LEAN_MASS for V5)          | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Whole body less head lean mass (g)                                                                                                                                    | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **WBTOT_LEAN_MASS** (H_WBTOT_LEAN_MASS for V5)            | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Whole body lean mass (g)                                                                                                                                              | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **LTOTAL_LEAN_MASS**                                      | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Left total lean mass (g). Available for V3/V4                                                                                                                         | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **RTOTAL_LEAN_MASS**                                      | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Right total lean mass (g). Available for V3/V4                                                                                                                        | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ANDROID_LEAN_MASS**                                     | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Android lean mass (g)                                                                                                                                                 | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **GYNOID_LEAN_MASS**                                      | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Gynoid lean mass (g)                                                                                                                                                  | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **AND_plus_GYN_LEAN_MASS**                                | None                               | Numeric             | Excluded               | 0..\*            | NA                                                                                  | Lean mass in grams (AND + GYN). Not available for V4/V5                                                                                                               | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ALM** (H_ALM for V5)                                    | `ALM_HT2`, `ALM_BMI`               | Numeric             | Source                 | 0..\*            | NA                                                                                  | Appendicular Lean Mass in grams (Arms + Legs).                                                                                                                        | NA                                                                       | imputed, normally distribution                |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ALM_HT2** (H_ALM_HT2 for V5)                            | `ewgsop2_sarcopenia_stage`         | Numeric             | Primary Outcome        | 0..\*            | NA                                                                                  | Sarcopenia Marker: ALM/Height² in kg/m^2.^                                                                                                                            | Visits with missing/ invalid values are excluded for ALM analysis        | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ALM_WT** (H_ALM_WT for V5)                              | None                               | Numeric             | Sensitive analysis     | 0..\*            | NA                                                                                  | Appendicular lean mass of arms + legs (kg) /weight (kg).                                                                                                              | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ALM_BMI** (H_ALM_BMI for V5)                            | None                               | Numeric             | Sensitive analysis     | 0..\*            | NA                                                                                  | ALM scaled by BMI.                                                                                                                                                    | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TUG_GETUP**                                             | None                               | Binary              | Excluded               | 0..\*            | 0=?,\                                                                               | Timed Up and Go: 1-Get up from a chair with crossed hands on chest, 0-help with. Only available for V4/V5.                                                            | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=?                                                                                 |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | <!--# check what coding stands for -->                                              |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TUG_GO**                                                | None                               | Binary              | Excluded               | 0..\*            | 0=stop or uncomplete,\                                                              | Timed Up and Go. Only available for V4/V5.                                                                                                                            | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1= 1-Walk 3 meters                                                                  |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TUG_TURN**                                              | None                               | Binary              | Excluded               | 0..\*            | 0=dysbalance, stop or uncomplete,\                                                  | Timed Up and Go. Only available for V4/V5.                                                                                                                            | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=Turn back                                                                         |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TUG_GOBACKSIT**                                         | None                               | Binary              | Excluded               | 0..\*            | 0=?,\                                                                               | Timed Up and Go. Only available for V4/V5.                                                                                                                            | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=Come back 3m and sit                                                              |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | <!--# check what 0 codes for -->                                                    |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TUG_TIME**                                              | None                               | Numeric             | Excluded               | 0..\*            | \-                                                                                  | Time to complete Timed Up and Go in seconds. Only available for V4/V5.                                                                                                | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **TUG_SCORE**                                             | None                               | Factor              | Excluded               | 0..\*            | 0=?,\                                                                               | Functional mobility score, 0-4 points. Only available for V4/V5. <!--# How is this score calculated? -->                                                              | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=?,\                                                                               |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 2=?, ...\                                                                           |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | <!--# check what coding stands for -->                                              |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **6MGS** (renamed to gait_speed to conform to R language) | `ewgsop2_sarcopenia_stage`         | Numeric             | Primary Outcome        | 1..\*            | NA                                                                                  | Gait Speed: 6-meter walk time in m/s.                                                                                                                                 | Visits with missing/ invalid values are excluded for gait speed analysis | impute, normally distribution, only for V4/V5 |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **PHYSTEST_COMMENT**                                      | None                               | Character           | Exclude                | 0..\*            | NA                                                                                  | Comment on Timed Up and Go or Gait Speed. High misingness and not needed for main analysis.                                                                           | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SARCF_STRENGHT**                                        | None                               | Factor              | Exclude                | 0..1             | 0=None,\                                                                            | SARC-F: Difficulty in lifting and carrying 10 pounds. Only available for V5.                                                                                          | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=Some,\                                                                            |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 2=A lot or unable                                                                   |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SARCF_WALK**                                            | None                               | Factor              | Exclude                | 0..1             | None=0,\                                                                            | SARC-F: Difficulty walking across a room. Only available for V5.                                                                                                      | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | Some=1,\                                                                            |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 2=A lot, use aids, or unable                                                        |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SARCF_CHAIR**                                           | None                               | Factor              | Exclude                | 0..1             | 0=None,\                                                                            | SARC-F: Difficulty transferring from a chair or bed.Only available for V5.                                                                                            | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=Some,\                                                                            |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 2=A lot or unable                                                                   |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SARCF_STAIRS**                                          | None                               | Factor              | Exclude                | 0..1             | 0=None,\                                                                            | SARC-F: Difficulty climbing a flight of 10 stairs. Only available for V5.                                                                                             | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=Some,\                                                                            |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 2=A lot or unable                                                                   |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SARCF_FALL**                                            | None                               | Factor              | Exclude                | 0..1             | 0=None,\                                                                            | SARC-F: How many times have you fallen in the past year. Only available for V5.                                                                                       | NA                                                                       | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=1-3 falls,\                                                                       |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  | 2=4+ falls                                                                          |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **SARCF_TOTAL**                                           | None                               | Integer             | Exclude                | 0..1             | 1=?, 2=?, ... <!--# check what coding stands for -->                                | Sarcopenia Screening: total of each other items. Only available for V5.                                                                                               | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_R1**                                                | `HGS_MAX` (already in raw dataset) | Numeric             | Source                 | 0..1             | NA                                                                                  | Hand grip 1st measure on right hand (measured if hand is dominant). Only available for V5.                                                                            | see `HGS_MAX`                                                            | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_R2**                                                | `HGS_MAX` (already in raw dataset) | Numeric             | Source                 | 0..1             | NA                                                                                  | Hand grip 2st measure on right hand (measured if hand is dominant). Only available for V5.                                                                            | see `HGS_MAX`                                                            | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_R3**                                                | `HGS_MAX` (already in raw dataset) | Numeric             | Source                 | 0..1             | NA                                                                                  | Hand grip 3st measure on right hand (measured if hand is dominant). Only available for V5.                                                                            | see `HGS_MAX`                                                            | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_L1**                                                | `HGS_MAX` (already in raw dataset) | Numeric             | Source                 | 0..1             | NA                                                                                  | Hand grip 1st measure on left hand (measured if hand is dominant). Only available for V5.                                                                             | see `HGS_MAX`                                                            | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_L2**                                                | `HGS_MAX` (already in raw dataset) | Numeric             | Source                 | 0..1             | NA                                                                                  | Hand grip 2st measure on left hand (measured if hand is dominant). Only available for V5.                                                                             | see `HGS_MAX`                                                            | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_L3**                                                | `HGS_MAX` (already in raw dataset) | Numeric             | Source                 | 0..1             | NA                                                                                  | Hand grip 3st measure on left hand (measured if hand is dominant). Only available for V5.                                                                             | see `HGS_MAX`                                                            | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **HGS_MAX**                                               | `ewgsop2_sarcopenia_stage`         | Numeric             | Primary Outcome        | 0..\*            | NA                                                                                  | Max Grip Strength (Peak of 6 measures) in kg. Only available for V5.                                                                                                  | <!--# define -->                                                         | impute                                        |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **L\_...LEAN_MASS/ L_ALM\_...**                           | None                               | multiple data types | Excluded               | 0..\*            | NA                                                                                  | multiple values for lean mass from Lunar DXA for V5, will not be used                                                                                                 | NA                                                                       | not used                                      |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **ewgsop2_sarcopenia_stage**                              | None                               | Factor              | Primary Outcome        | 0..\*            | 0=No sarcopenia, 1=Probable sarcopenia, 2=Confirmed Sarcopenia, 3=Severe sarcopenia | Mapping:                                                                                                                                                              | for Aim 2: participant with missing baseline value are excluded          | not used                                      |
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     | - no                                                                                                                                                                  |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     | - probable: handgrip strength\<16 kg (for women)                                                                                                                      |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     | - confirmed: criteria of probable + low muscle quantity or quality. \<5.5 kg/m² (women)                                                                               |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     |                                                                                                                                                                       |                                                                          |                                               |
|                                                           |                                    |                     |                        |                  |                                                                                     | - severe: criteria of confirmed + \<= 0.8 m/s                                                                                                                         |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| **FNIH_sarcopenia**                                       | None                               | Factor              | Secondary Outcome      | 0..\*            | 0=No,\                                                                              | FNIH criteria for sarcopenia for sensitivity analysis, Sarcopenia when grip strength \< 16 kg AND ALM/BMI \< 0.512                                                    | for Aim 3: participant with missing baseline value are excluded          | not used                                      |
|                                                           |                                    |                     |                        |                  | 1=Sarcopenia                                                                        |                                                                                                                                                                       |                                                                          |                                               |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
| DXA_method                                                | None                               | Factor              | For filtering          | 1..\*            | <!--# fill in -->                                                                   | To distinguish the method used to measure ALM                                                                                                                         | available for all                                                        | used as predictor                             |
+-----------------------------------------------------------+------------------------------------+---------------------+------------------------+------------------+-------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------+-----------------------------------------------+
