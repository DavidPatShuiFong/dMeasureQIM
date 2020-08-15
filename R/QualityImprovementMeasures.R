# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureQIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @md
#'
#' @name qim
#' @title dMeasure Quality Improvement Measures
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureIntegration
#'
#' @name dMeasureIntegration
#'
#' @description integration with dMeasure
#'   (especially DailyMeasure)
#'
#' @param information the information required
#'   `Provides` - modules provided (in this case, `dMeasureQIMRept` and `dMeasureQIMAppt`)
#'   `Requires` - the modules required (including `dMeasure`)
#'   `moduleID` - IDs of modules to create
#'
#' @return vector of required information
#'
#' @export
dMeasureIntegration <- function(information) {
  if (information == "Provides") {return(c("dMeasureQIMRept", "dMeasureQIMAppt"))}
  if (information == "Requires") {return(c("dMeasure"))}
  if (information == "moduleID") {
    return(
      list(
        list(ID = "qimRept", extraArgs = "contact = TRUE"),
        list(ID = "qimAppt", extraArgs = "contact = FALSE")
      )
    )
  }
}

#' dMeasureQIM class
#' @title dMeasureQIM class
#' @description Quality Improvement Measures reports and case-finding in EMR (Best Practice)
#'     a module for dMeasure
#' @export
dMeasureQIM <- R6::R6Class(
  "dMeasureQIM",
  public = list(
    # dM is a dMeasure object
    dM = NULL,
    initialize = function(dMeasure_obj) {
      # dMeasure_obj is a R6 dMeasure object
      self$dM <- dMeasure_obj
      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureQIM") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureQIM") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureQIM") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureQIM") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

##### special reactive functions ##########################


.private(dMeasureQIM, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasureQIM, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})

#####  ignore 'old' observations results? ############################################
# if TRUE, ignore results that are too old to be qualify for quality improvement measure

.private(dMeasureQIM, ".qim_ignoreOld", TRUE)
.active(dMeasureQIM, "qim_ignoreOld", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_ignoreOld)
  }
  if (is.logical(value)) {
    private$.qim_ignoreOld <- value
    private$set_reactive(self$qim_ignoreOldR, value)
  } else {
    warning("$qim_ignoreOld only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasureQIM, "qim_ignoreOldR", TRUE)

##### patient list to use ############################################
# if TRUE, then use 'contact' list. otherwise use appointment list
# this doesn't affect QIM_active, but affects other methods

.private(dMeasureQIM, ".qim_contact", TRUE)
.active(dMeasureQIM, "qim_contact", function(value) {
  # is the 'contact' patient list being used?
  #  this can be a combination (or even none of...) appointments, visits and billings
  #  with various criteria. dM$contact_count_list
  # if false, the 'appointment' list is being used from dM$appointments_filtered
  if (missing(value)) {
    return(private$.qim_contact)
  }
  if (is.logical(value)) {
    private$.qim_contact <- value
    private$set_reactive(self$qim_contactR, value)
  } else {
    warning("$qim_contact only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasureQIM, "qim_contactR", TRUE)

##### demographic groupings for reporting ############################################

.active(dMeasureQIM, "qim_demographicGroupings", function(value) {
  if (!missing(value)) {
    warning("$qim_demographicGroupings is read-only.")
  } else {
    return(c("Age10", "Sex", "Ethnicity", "Indigenous", "MaritalStatus", "Sexuality"))
    # vector of valid demographic groups (for QIM reporting)
    # Age in 10 year categories
    # Ethnicity
    # MaritalStatus
    # Sexuality
  }
})

.private_init(dMeasureQIM, ".qim_demographicGroup", quote(c("Age10", "Sex", "Indigenous")))
.active(dMeasureQIM, "qim_demographicGroup", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_demographicGroup)
  }
  value <- intersect(value, self$qim_demographicGroupings)
  # make sure groups are valid
  private$.qim_demographicGroup <- value
  private$set_reactive(self$qim_demographicGroupR, value)
})
.reactive(dMeasureQIM, "qim_demographicGroupR", quote(self$qim_demographicGroupings))

# names of QIM measures
measure_names <- c(
  "QIM 01 - Diabetes HbA1C results",
  "QIM 02 - 15+ smoking status",
  "QIM 03 - 15+ weight classification",
  "QIM 04 - 65+ influenza immunization",
  "QIM 05 - Diabetes influenza immunization",
  "QIM 06 - COPD influenza immunization",
  "QIM 07 - 15+ alcohol consumption status",
  "QIM 08 - Cardiovascular risk assessable",
  "QIM 09 - Cervical screening up-to-date",
  "QIM 10 - Diabetes blood pressure recording"
)
sex_choices <- c("Female", "Male", "Not stated", "X")
ethnicity_choices <- c(
  "Aboriginal",
  "Torres Strait Islander",
  "Both Aboriginal and Torres Strait Islander",
  "Not stated",
  "Neither"
)
# diabetes type 'not specified' is actually ""
# and applies to QIM measures where DiabetesType is
# not specified
diabetes_choices <- c(
  # 'not specified' refers to QIMs which are not diabetic oriented
  # 'not stated' means diabetes has not been defined as type 1 or type 2
  "Type 1", "Type 2", "Not specified", "Not stated"
)

#' add demographics
#'
#' @param d dataframe with InternalID
#' @param dM dMeasure object
#' @param reference_date date to calculate age from
#' @param ageGroups numbers to determine age group
#'
#'   Intervals are closed to the left and open to the right
#'   i.e. includes the left-most end of the interval,
#'   and does not include the right.
#'
#'   QIM 09 (Cervical screening) has a different age group
#'   interval to other QIMs, as it includes '65' and '70'.
#'   (["PIP QI Improvement Measures - Technical Specifications v1.1"](https://www1.health.gov.au/internet/main/publishing.nsf/Content/46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-Technical-Specifications.pdf),
#'   page 27)
#'
#' @return dataframe with DOB, Age10, Sex, Indigenous, RecordNo,
#'   MaritalStatus, Sexuality, Indigenous
#' @export
add_demographics <- function(
  d, dM, reference_date,
  ageGroups = c(0, 5, 15, 25, 35, 45, 55, 65)
) {
  intID <- d %>>% dplyr::pull(InternalID) %>>% c(-1)

  d <- d  %>>%
    dplyr::left_join(
      dM$db$patients %>>%
        dplyr::filter(InternalID %in% intID) %>>%
        dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
      by = "InternalID",
      copy = TRUE
    ) %>>%
    dplyr::left_join(
      dM$db$clinical %>>%
        dplyr::filter(InternalID %in% intID) %>>%
        dplyr::select(InternalID, MaritalStatus, Sexuality),
      by = "InternalID",
      copy = TRUE
    ) %>>%
    dplyr::mutate(
      DOB = as.Date(DOB, origin = "1970-01-01"),
      Age10 = ageGroups[findInterval(
        dMeasure::calc_age(DOB, reference_date),
        ageGroups
      )],
      # round age group to nearest 10 years, starting age 5, minimum 0 and maximum 65
      Ethnicity = dplyr::na_if(Ethnicity, ""), # changes "" to NA
      Indigenous = dplyr::case_when(
        Ethnicity == "Aboriginal" ~ "Aboriginal",
        Ethnicity == "Torres Strait Islander" ~ "Torres Strait Islander",
        Ethnicity == "Aboriginal/Torres Strait Islander" ~
          "Both Aboriginal and Torres Strait Islander",
        is.na(Ethnicity) ~ "Not stated",
        TRUE ~ "Neither"
      ),
      Sex = dplyr::case_when(
        Sex == "Female" ~ "Female",
        Sex == "Male" ~ "Male",
        is.na(Sex) ~ "Not stated",
        TRUE ~ "X" # for indeterminate/intersec/unspecified
      ),
      MaritalStatus = dplyr::na_if(MaritalStatus, ""),
      Sexuality = dplyr::na_if(Sexuality, "")
    )

  return(d)
}

#' complete demographics
#'
#' @md
#'
#' Create 'missing' rows, which have 'n = 0' values.
#'
#' In part, responds to columns existing  in `d`.
#'
#' Includes different types of status e.g. TRUE/FALSE
#'
#' @param d dataframe to process
#'   with QIM, Age10, Sex, Indigenous, DiabetesType
#'   Measure, State, n.
#'   optionally - ProportionDemographic, DateFrom, DateTo
#' @param qim_name name of qim
#' @param age_min minimum age group
#' @param age_max maximum age group
#' @param ageGroups numbers to determine age group
#' @param include_diabetes include diabetes demographic
#' @param measure name of measure
#' @param states vector of status types
#' @param atsi_only include only ATSI sub-groups?
#'
#' @return dataframe with DOB, Age10, Sex, Indigenous, DiabetesType
#'   Measure, State, n.
#'   optionally - ProportionDemographic, DateFrom, DateTo
#' @export
complete_demographics <- function(
  d,
  qim_name,
  age_min = 0,
  age_max = 65,
  ageGroups = c(0, 5, 15, 25, 35, 45, 55, 65),
  include_diabetes = FALSE,
  measure,
  states = c(FALSE, TRUE),
  atsi_only = FALSE
) {

  # restrict by age
  age_list <- ageGroups
  age_list <- age_list[age_list >= age_min & age_list <= age_max]
  indigenous_list <- c("Aboriginal", "Torres Strait Islander",
                       "Both Aboriginal and Torres Strait Islander")
  if (!atsi_only) {
    indigenous_list <- c(indigenous_list, "Neither", "Not stated")
  }
  if (include_diabetes) {
    # include, then include diabetes types
    diabetes_list <- c("Type 1", "Type 2", as.character(NA))
  } else {
    diabetes_list <- c("")
  }

  # missing_rows <- do.call(tidyr::expand, to_expand) %>>%
  df_complete <- d %>>%
    tidyr::expand(
      Age10 = age_list,
      Sex = c("Female", "Male", "X", "Not stated"),
      Indigenous = indigenous_list,
      DiabetesType = diabetes_list,
      State = as.character(states)
    ) %>>%
    dplyr::anti_join(
      d %>>% dplyr::mutate(State = as.character(State)),
      # need to keep 'type' of 'State' consistent!
      by = c("Age10", "Sex", "Indigenous", "State", "DiabetesType")
    ) %>>% # this finds 'missing' rows
    dplyr::mutate(
      QIM = qim_name,
      Measure = measure,
      n = 0 # no entries
    )

  if ("ProportionDemographic" %in% names(d)) {
    df_complete <- df_complete %>>%
    dplyr::mutate(
      ProportionDemographic = 0 # no entries
    )
  }
  if ("DateFrom" %in% names(d)) {
    df_complete <- df_complete %>>%
      dplyr::mutate(
        DateFrom = d[[1, "DateFrom"]] # copy first entry
      )
  }
  if ("DateTo" %in% names(d)) {
    df_complete <- df_complete %>>%
      dplyr::mutate(
        DateTo = d[[1, "DateTo"]] # copy first entry
      )
  }

  df_complete <- rbind(df_complete, d)
  # join back to original dataframe

  return(df_complete)
}

#' fill report with 'complete' quality improvement measures and demographics
#'
#' determines from the content of 'QIM' what is required to fill
#' in the 'missing' (zero) rows
#'
#' @param d dataframe to process
#'   with QIM, Age10, Sex, Indigenous, DiabetesType
#'   Measure, State, n.
#'   optionally - ProportionDemographic, DateFrom, DateTo
#'
#' @return dataframe with DOB, Age10, Sex, Indigenous, DiabetesType
#'   Measure, State, n.
#'   optionally - ProportionDemographic, DateFrom, DateTo
#'
#' @export
fill_demographics <- function(d) {

  if (nrow(d) == 0) {return(d)} # nothing to fill!

  qim_name <- d[[1, "QIM"]] # the name of the QIM
  qim <- as.numeric(stringi::stri_sub(qim_name, -2, -1)) # the number of the QIM
  measure_name = d[[1, "Measure"]]

  if (qim == 1) { # diabetes HbA1C
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 0, age_max = 65,
      include_diabetes = TRUE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  } else if (qim == 2) { # 15+ smoking
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 15, age_max = 65,
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(as.character(NA), "Non smoker", "Ex smoker", "Smoker")
      # derived from dMQIM$dM$db$SMOKINGSTATUS %>>% dplyr::pull(SMOKINGTEXT)
      #   should be "", "Non smoker", "Ex smoker", "Smoker"
      #   the "" being 'unknown', and replaced with NA
      #   in dM$smoking_obs
    )
  } else if (qim == 3) { # 15+ BMI
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 15, age_max = 65,
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(
        as.character(NA),
        "Underweight", "Healthy", "Overweight", "Obese"
      )
    )
  } else if (qim == 4) { # 65+ influenza
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 65, age_max = 65,
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  } else if (qim == 5) { # diabetes influenza
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 0, age_max = 65,
      include_diabetes = TRUE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  } else if (qim == 6) { # COPD influenza
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 15, age_max = 65,
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  } else if (qim == 7) { # 15+ alcohol
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 15, age_max = 65,
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  } else if (qim == 8) { # CVD risk
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 45, age_max = 65,
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    ) %>>%
      # cvdRisk is a special case, as it includes indigenous
      # at age10 35 (all other groups are only 45 and more)
      dMeasureQIM::complete_demographics(
        qim_name = qim_name,
        age_min = 35, age_max = 35,
        include_diabetes = FALSE,
        measure = measure_name,
        states = c(FALSE, TRUE),
        atsi_only = TRUE
      )
  } else if (qim == 9) { # cervical screening
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 25, age_max = 70,
      ageGroups = c(25, 35, 45, 55, 65, 70),
      # note that QIM 09 cervical screening has different
      # age group structure than other QIMs by having a '70' group.
      include_diabetes = FALSE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  } else if (qim == 10) { # diabetes BP
    dt <- dMeasureQIM::complete_demographics(
      d, qim_name = qim_name,
      age_min = 0, age_max = 65,
      include_diabetes = TRUE,
      measure = measure_name,
      states = c(FALSE, TRUE)
    )
  }

  return(dt)
}
