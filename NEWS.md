Changelog

# version 0.4.0

* compliance with versioin 1.1 of [PIP QI Improvement Measures - Technical Specifications v1.1 04052020](https://www1.health.gov.au/internet/main/publishing.nsf/Content/46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-Technical-Specifications.pdf)
  + change `Age5` to `Age10`
* `Proportion_Demographic` : proportion of measurement in each demographic sub-group

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
