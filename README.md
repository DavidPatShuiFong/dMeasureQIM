# dMeasureQIM
[Quality Improvement Measures](https://www1.health.gov.au/internet/main/publishing.nsf/Content/PIP-QI_Incentive_guidance) module for dMeasure

## Structure of exported `CSV` (comma-separated-value) file

Based on [PIP QI Annotated Specifications (May 2020)](https://www.health.gov.au/resources/publications/practice-incentives-program-quality-improvement-measures-annotated-specifications?language=en), [PIP QI Technical Specifications V1.2](https://www.health.gov.au/resources/publications/practice-incentives-program-quality-improvement-measures-technical-specifications?language=en) and [PIP QI User Guide](https://www.health.gov.au/resources/collections/practice-incentives-program-quality-improvement-incentive-guidance).

### Columns

1. `QIM` - `QIM 01` to `QIM 10`
  + `QIM 01` - Proportion of patients with diabetes with a current HbA1c result 
  + `QIM 02` - Proportion of patients whose smoking status has been recorded (a)/result (b)
  + `QIM 03` - Proportion of patients with a weight classification recorded (a)/result (b)
  + `QIM 04` - Proportion of patients aged 65 and over who were immunised against influenza
  + `QIM 05` - Proportion of patients with diabetes who were immunised against influenza
  + `QIM 06` - Proportion of patients with COPD who were immunised against influenza
  + `QIM 07` - Proportion of patients with an alcohol consumption status 
  + `QIM 08` - Proportion of patients with the necessary risk factors assessed to enable CVD assessment
  + `QIM 09` - Proportion of female patients with an up-to-date cervical screening
  + `QIM 10` - Proportion of patients with diabetes with a blood pressure result

2. `Age10` - Age group
  + `0` (0-4), `5` (5-14), `15` (15-24), `25` (25-34), `35` (35-44), `45` (45-54), `55` (55-64), `65` (65 and over *or* 65-69), `70` (70 to 74)
  + `QIM 01`, `QIM 05`, `QIM 10` - 8 groups (`0`, `5`, `15`, `25`, `35`, `45`, `55`, `65`)
  + `QIM 02`, `QIM 03`, `QIM 06`, `QIM 07` - 6 groups, 15 plus (`15`, `25`, `35`, `45`, `55`, `65`)
  + `QIM 04` - 65 plus (`65`) only
  + `QIM 08` - 3 groups, 45 plus (`45`, `55`, `65`) *plus* `35` for `Indigenous` state in (`Aboriginal`, `Torres Strait Islander`, `Both Aboriginal and Torres Strait Islander`)
  + `QIM 09` - 6 groups, 25 plus (`25`, `35`, `45`, `55`, `65`, `70`)

3. `Sex` - Person-sex, METeOR identifier 635126
  + `Male` - `1` Male
  + `Female` - `2` Female
  + `X` - `X` Indeterminate/Intersex/Unspecified
  + `NA` - not available (not specified)

4. `Indigenous` - Indigenous status, METeOR identifier 602543
  + `Aboriginal` - `1` 'Aboriginal but not Torres Strait Islander'
  + `Torres Strait Islander` - `2` 'Torres Strait Islander but not Aboriginal'
  + `Both Aboriginal and Torres Strait Islander` - `3` 'Both Aboriginal and Torres Strait Islander'
  + `Neither` - `4` 'Neither Aboriginal or Torres Strait Islander'
  + `Not stated` - `9` 'Not stated'
  
5. `Diabetes Type` - Diabetes status. METeOR identifier 270194. Only relevant to `QIM 01`, `QIM 05` and `QIM 10`. Left empty in all other QIM
  + `Type 1`
  + `Type 2`
  + `NA` - not available (not specified)
  
6. `Measure`
  + `QIM 01` - `HbA1C`
  + `QIM 02` - `Smoking`
  + `QIM 03` - `BMIClass`
  + `QIM 04` - `InfluenzaDone`
  + `QIM 05` - `InfluenzaDone`
  + `QIM 06` - `InfluenzaDone`
  + `QIM 07` - `AlcoholDone`
  + `QIM 08` - `CVDRiskDone`
  + `QIM 09` - `CSTDone`
  + `QIM 10` - `BPDone`
  
7. `State`
  + `QIM 01`, `QIM 04`, `QIM 05`, `QIM 06`, `QIM 07`, `QIM 08`, `QIM 10` - `TRUE` or `FALSE`
  + `QIM 02` - Smoking status : `Ex Smoker` (ex-smoker), `Non smoker` (never smoked), `Smoker` (current smoker) or `NA` (not available)
  + `QIM 03` - Body mass Index classification (METeOR identifier 270474) : `Healthy`, `Obese`, `Overweight`, `Underweight` or `NA` (not available)
  
8. `n` - the number of regular clients (three contacts - usually defined as three billed visits - over a two year period) who are in a group defined by columns 2, 3, 4, 5, 6 and 7 i.e. `Age10`, `Sex`, `Indigenous`, `Diabetes Type` (if applicable), `Measure` and `State`.
  + Supports small-cell-suppression - change `n` values less than 5 to 0 (zero). Section 2.4 of [PIP QI User Guide](https://www1.health.gov.au/internet/main/publishing.nsf/Content/46506AF50A4824B6CA25848600113FFF/$File/PIP%20QI%20-%20User%20Guide.pdf).

Additional columns of information which can either be extrapolatd from columns 1 to 8, or further define the nature of the information collection.

9. `ProportionDemographic` - the proportion of the demographic group (as defined by columns 2, 3, 4, 5 - (`Age10`, `Sex`, `Indigenous` and, if relevant, `Diabetes Type`) with the defined `State`

10. `DateFrom` and `DateTo` - the period (typically two years) of contacts

11. `ContactType` - the contact type, usually services i.e. billings

12. `MinContact` - the minimum number of contacts in the defined period, usually 3.

13. `Clinicians` - the clinicians who were contacted.

## Number of rows/groups

* QIM 01/05/10 : $8\times4\times5\times3\times2$ each
* QIM 02 : $6\times4\times5\times4$
* QIM 03 : $6\times4\times5\times5$
* QIM 06/07 : $6\times4\times5\times2$ each 
* QIM 04 : $1\times4\times5\times2$
* QIM 08 : $3\times4\times5\times2+4\times3\times2$
* QIM 09 : allowing more than `Female` sex : $6\times4\times5\times2$

Total : $960+480+600+40+960+240+240+(120+24)+240+960=4684$

## Structure of exported `JSON` (JavaScript Objection Notation) file

Based on [PIP QI Eligible Data Set - JSON Specification - version 1.1](https://www.health.gov.au/resources/publications/practice-incentives-program-eligible-data-set-json-file-specifications?language=en)
