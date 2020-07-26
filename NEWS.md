Changelog

# version 0.4.0

* compliance with version 1.1 of [PIP QI Improvement Measures - Technical Specifications v1.1 04052020](https://www1.health.gov.au/internet/main/publishing.nsf/Content/46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-Technical-Specifications.pdf)
  + change `Age5` to `Age10` (minimum 0, maximum 65)
  + add `Indigenous`
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
* add `store` option for `report_qim_*`, `list_qim_*` and `list_qim_*_appointments` methods.
  + by default, set to `TRUE`, store result to `self$qim_*_report`
  + if `FALSE`, the results are returned, but not stored to `self$qim_*_report`

## Bugfix

* QIM 02 15+ smoking `list_qim15_plus` : `ignoreOld` now ignores results more than one year old

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
