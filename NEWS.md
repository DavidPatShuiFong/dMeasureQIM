Changelog

# version 0.6.0
1st August 2021

## New

* `group_identification_suppression` option for `writeReportJSON`
  + suppress if numerator 2 or less OR difference between numerator and denominator is less than or equal to 2.
  + This is a simple probability-based disclosure suppression (l-diversity) against homogeneity attack,
    which overlaps with 'small cell suppression' of cells where the denominator is less than 5.

## Change

* `getReport` accepts `min_date` and `max_date` parameters
* QIM 04 (influenza 65+) `report_qim_65plus` - exclude patients who have been marked not to have an influenza immunization reminder
  + as per PIP QI Improvement Measures Technical Specifications V1.2 (22102020)
  + QIM report (called from `QIM_report_UI.R`) - do not include patients who have not visited within the last 15 months
  + as per PIP QI Improvement Measures Technical Specifications V1.2 (22102020)
* QIM 05 (influenza diabetes) `report_qim_diabetes`
  + if only 'influenza' measure chosen then exclude patients who have been marked not to have an influenza immunization reminder
  + if 'influenza' *and* another measure chosen, then if a patient has been marked as not to have an influenza immunization reminder,
    then the 'InfluenzaDone' value will be set to `NA`
  + as per PIP QI Improvement Meausres Technical Specifications V1.2 (22102020)
* QIM 06 (influenza COPD) `report_qim_copd` - exclude patients who have been marked not to have an influenza immunization reminder
  + as per PIP QI Improvement Measures Technical Specifications V1.2 (22102020)
* QIM 09 (cervical screening) `repot_qim_cost` - excludes patients who have been marked not have cervical screening or
  an indication that result may only be available elsewhere
  + as per PIP QI Improvement Measures Technical Specifications V1.2 (22102020)
  + Does not include if 'No longer requires cervical screening' is set
    OR 'Opt out of cervical screening' (reasons excluded include 'has screening at another clinic/elsewhere' and
    'refuses'  but does not include "Doesn't want reminders sent")

## Fix

* Add 'small cell suppression' and 'group identification suppression' to QIM 02a and QIM 03a

# version 0.5.0
12th January 2021

## New

* PIP QI Eligible Data set JSON specification version 1.1 export 

# version 0.4.1
16th September 2020

## Changes

* add 10-year cardiovascular (CVD) risk `frisk10` to QIM 08.
* increase maximum upload filesize to 300 megabytes

# version 0.4.0
18th August 2020

## New

* report creation download (comma-separated-values '.csv') and restore ('upload')
  + `getReport` to modify QIM-specific reports to generic 'long-format' which can be used by all QIM reports
  + support small number suppression
  + save and restore reports
  + charting
* compliance with version 1.1 of [PIP QI Improvement Measures - Technical Specifications v1.1 04052020](https://www1.health.gov.au/internet/main/publishing.nsf/Content/46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-Technical-Specifications.pdf)
  + change `Age5` to `Age10` (minimum 0, maximum 65)
  + add `Indigenous`
  + `Sex` categories "Female", "Male", "X" (for indeterminate/intersec/unspecified) and "Not stated" (not available 'NA')
  + default demographic groups are `Age10`, `Sex`, `Indigenous`
  + for Smoking measure (QIM 02), `SmokingStatus` is used
  + for weight classification measure (QIM 03), `BMIclass` is used
    - now `BMIClass` is computed with age- and sex- specific ranges
  + for diabetes (QIM 01 and others), option to add diabetes type to description
    - `report_qim_diabetes` and others accepts a `type_diabetes` option
    - also determined  by active `self$qim_diabetes_showType` (and private and reactive versions)
    - userInterface adds pushbutton to turn on/off `self$qim_diabetes_showType` (default ON)
  + for CVD risk (QIM 08) add the condition that either diabetes must be diagnosed
    or has been checked (BSL or HbA1C) within the past two years
* `Proportion_Demographic` : proportion of measurement in each demographic sub-group
* `add_demographics` : function to add demographics to dataframe

## Changes

* add `store` option for `report_qim_*`, `list_qim_*` and `list_qim_*_appointments` methods.
  + by default, set to `TRUE`, store result to `self$qim_*_report`
  + if `FALSE`, the results are returned, but not stored to `self$qim_*_report`

## Bugfix

* QIM 02 15+ smoking `list_qim15_plus` : `ignoreOld` now ignores results more than one year old
* replace `dplyr::filter(x == max(x))` with `dplyr::arrange(dplyr::desc(x), .by_group = TRUE) %>>% dplyr::filter(dplyr::row_number() == 1)`
  + `arrange`/`row_number` breaks 'ties', where more than one of 'max' value
* 'Active' now not shown during 'Appointment' view (which is what was intended)

## Changes

* removal of `shinycssloaders`

# version 0.3.0
19th July 2020

* `dMeasureIntegration` for auto-loading module

# version 0.2.0
17th July 2020

* move DailyMeasure/GPstat webUI from `DailyMeasure` to `dMeasureQIM`
  + adds `shinydashboardmenuItem`, `dMeasureShinytabItems`,
    `datatableUI`, `datatableServer` in `userInterface.R`

# version 0.1.6
10th July 2020

* `WeightDone` in `report_qim_15plus` depends on `BMIDate` instead of `WeightDate`

# version 0.1.5
16th May 2020

* add `max_date` to
  + `report_qim_active`,
  + `list_qim_*`, `list_qim_*_appointments`, `report_qim_*`

# version 0.1.4
3rd May 2020

* add `max_date` to call to `list_contact_count`

# version 0.1.3
10th April 2020

* rintrojs walkthrough (introduction) `steps_introduction_df`

# version 0.1.2
19th February 2020

* change in QIM_15plus
    DaysPerWeek -> AlcoholDaysPerWeek and DrinksPerDay -> AlcoholDrinksPerDay
* change in cvdrisk_list
    remove redundant fields in `qim_cvdrisk_list`
    UrineAlbuminUnit ->  UrineAlbuminUnits

# version 0.1.1
22nd September 2019

* add 'contact' parameter to `list_qim_active`
