##### QIM cardiovascular risk ###################################################################

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @name qim_cvdrisk
#' @title dMeasure Quality Improvement Measures - cardiovascular risk
#'
#' @include QualityImprovementMeasures.R
NULL

##### QIM cardiovascular risk fields ############################################################
.public(
  dMeasureQIM, "qim_cvdRisk_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    RecordNo = character(),
    Age10 = integer(),
    Sex = character(),
    Indigenous = character(),
    Ethnicity = character(),
    MaritalStatus = character(),
    Sexuality = character(),
    CardiovascularDisease = logical(),
    Diabetes = logical(),
    SmokingDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    SmokingStatus = character(),
    UrineAlbuminDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    UrineAlbuminValue = double(),
    UrineAlbuminUnits = character(),
    PersistentProteinuria = logical(),
    eGFRDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    eGFRValue = double(),
    eGFRUnits = character(),
    FamilialHypercholesterolaemia = logical(),
    LVH = logical(),
    CholesterolDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Cholesterol = double(), HDL = double(), LDL = double(),
    Triglycerides = double(), CholHDLRatio = double(),
    BPDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    BP = character(),
    frisk = double(), friskHI = character(),
    stringsAsFactors = FALSE
  )
)

##### QIM cardiovascular risk assessment methods ##########################################################

.active(dMeasureQIM, "qim_cvdRisk_measureTypes", function(value) {
  if (!missing(value)) {
    warning("$qim_cvdRisk_measureTypes is read-only.")
  } else {
    return(c("Include ATSI 35-44", "Exclude 75+", "Exclude known CVD"))
    # vector of valid QIM measures options for 15 plus (for QIM reporting)
  }
})

.private_init(dMeasureQIM, ".qim_cvdRisk_measure", quote(self$qim_cvdRisk_measureTypes))
.active(dMeasureQIM, "qim_cvdRisk_measure", function(value) {
  if (missing(value)) {
    return(private$.qim_cvdRisk_measure)
  }
  value <- intersect(value, self$qim_cvdRisk_measureTypes)
  # only valid measure types allowed
  private$.qim_cvdRisk_measure <- value
  private$set_reactive(self$qim_cvdRisk_measureR, value)
})
.reactive(dMeasureQIM, "qim_cvdRisk_measureR", quote(self$qim_cvdRisk_measureTypes))

#' List of patient with information to complete cardiovascular risk assessment
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 08.Proportion of patients with the necessary risk factors assessed
#'  to enable CVD assessment
#'
#' required parameters
#'
#'  Age, Ethnicity (especially ATSI status)
#'
#'   included - Age 45 to 74 years or older
#'
#'    OR Age 35 or older + ATSI
#'     (ATSI 35+ optional - included by default - see $qim_cvdRisk_measure)
#'
#'    Age 75+ excluded
#'     (Option to include - see $qim_cvdRisk_measure)
#'
#'  Known cardiovascular disease
#'     (optional - excluded by default - see $qim_cvdRisk_measure)
#'
#'  Presence of diabetes. Diabetes and microalbuminuria
#'
#'  eGFR
#'
#'  previous diagnosis of familial hypercholesterolaemia
#'
#'  systolic blood pressure. Diastolic blood pressure
#'
#'  Serum total cholesterol. Serum HDL cholesterol
#'
#' source : National Vascular Disease Prevention Alliance (NVDPA) guidelines
#' https://www.cvdcheck.org.au/australian-absolute-cardiovascular-disease-risk-calculator
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
#'
#'     FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact at most max_date. default is $contact_maxDate
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID and measures
#' @export
list_qim_cvdRisk <- function(dMeasureQIM_obj,
                             contact = NA,
                             date_from = NA,
                             date_to = NA,
                             clinicians = NA,
                             min_contact = NA,
                             min_date = NA, max_date = NA,
                             contact_type = NA,
                             ignoreOld = NA,
                             lazy = FALSE) {
  dMeasureQIM_obj$list_qim_cvdRisk(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy
  )
}

.public(dMeasureQIM, "list_qim_cvdRisk", function(contact = NA,
                                                  date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA, max_date = NA,
                                                  contact_type = NA,
                                                  ignoreOld = NA,
                                                  lazy = FALSE) {
  if (is.na(contact)) {
    contact <- self$qim_contact
  }
  if (is.na(date_from)) {
    date_from <- self$dM$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$dM$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$dM$clinicians
  }
  if (is.na(min_contact)) {
    min_contact <- self$dM$contact_min
  }
  if (is.na(min_date)) {
    min_date <- self$dM$contact_minDate
  }
  if (is.na(max_date)) {
    max_date <- self$dM$contact_maxDate
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }
  if (ignoreOld) {
    obs_from <- NA
  } else {
    obs_from <- as.Date(-Inf, origin = "1970-01-01")
    # accept very old results
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "cvdRisk_qim",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      if (!lazy) {
        self$dM$list_contact_45_74(
          date_from, date_to, clinicians,
          min_contact, min_date, max_date,
          contact_type,
          lazy
        )
        if ("Include ATSI 35-44" %in% self$qim_cvdRisk_measure) {
          self$dM$list_contact_ATSI_35_44(
            date_from, date_to, clinicians,
            min_contact, min_date, max_date,
            contact_type,
            lazy
          )
        }
        if (!("Exclude 75+" %in% self$qim_cvdRisk_measure)) {
          self$dM$list_contact_75plus(
            date_from, date_to, clinicians,
            min_contact, min_date, max_date,
            contact_type,
            lazy
          )
        }
      }
      cvdRisk_list <- self$dM$contact_45_74_list
      if ("Include ATSI 35-44" %in% self$qim_cvdRisk_measure) {
        cvdRisk_list <- rbind(cvdRisk_list, self$dM$contact_ATSI_35_44_list)
      }
      if (!("Exclude 75+" %in% self$qim_cvdRisk_measure)) {
        cvdRisk_list <- rbind(cvdRisk_list, self$dM$contact_75plus_list)
      }
      cvdRisk_list <- dplyr::distinct(cvdRisk_list) %>>% # remove duplicates
        dplyr::select(-c(Count, Latest)) # don't need these fields

      cvdRiskID <- cvdRisk_list %>>%
        dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty vector, which is bad for SQL filter
    } else {
      if (!lazy) {
        self$dM$filter_appointments()
      }
      cvdRiskID <- c(self$dM$fortyfiveseventyfour_list(), -1)
      if ("Include ATSI 35-44" %in% self$qim_cvdRisk_measure) {
        cvdRiskID <- c(cvdRiskID, self$dM$ATSI_35_44_list())
      }
      if (!("Exclude 75+" %in% self$qim_cvdRisk_measure)) {
        cvdRiskID <- c(cvdRiskID, self$dM$seventyfiveplus_list())
      }
      cvdRiskID <- unique(cvdRiskID) # remove duplicates
      cvdRisk_list <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% cvdRiskID) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }

    cvdID <- self$dM$cvd_list(data.frame(InternalID = cvdRiskID, Date = date_to))
    # known cardiovascular disease is excluded by default
    if ("Exclude known CVD" %in% self$qim_cvdRisk_measure) {
      cvdRisk_list <- cvdRisk_list %>>%
        dplyr::filter(!(InternalID %in% cvdID))
      cvdRiskID <- cvdRisk_list %>>% # re-calculate valid ID
        dplyr::pull(InternalID) %>>%
        c(-1) # add a dummy ID to prevent empty vector
    }

    # various other history items which are already at high risk of cardiovascular disease
    diabetesID <- self$dM$diabetes_list(data.frame(InternalID = cvdRiskID, Date = date_to))
    fHypercholesterolaemiaID <-
      self$dM$familialHypercholesterolaemia_list(data.frame(InternalID = cvdRiskID, Date = date_to))
    lvhID <-
      self$dM$LVH_list(data.frame(InternalID = cvdRiskID, Date = date_to))

    cvdRisk_list <- cvdRisk_list %>>%
      dplyr::mutate(CardiovascularDisease = InternalID %in% cvdID) %>>%
      dplyr::mutate(Diabetes = InternalID %in% diabetesID) %>>%
      dplyr::left_join(self$dM$smoking_obs(cvdRiskID,
        date_from = obs_from, date_to = date_to
      ),
      by = "InternalID",
      copy = TRUE
      ) %>>%
      dplyr::left_join(self$dM$UrineAlbumin_obs(cvdRiskID,
        date_from = obs_from, date_to = date_to
      ),
      by = "InternalID",
      copy = TRUE
      ) %>>%
      dplyr::left_join(self$dM$PersistentProteinuria_obs(cvdRiskID,
        date_from = obs_from, date_to = date_to
      ),
      by = "InternalID",
      copy = TRUE
      ) %>>%
      dplyr::left_join(self$dM$eGFR_obs(cvdRiskID,
        date_from = obs_from,
        date_to = date_to
      ),
      by = "InternalID",
      copy = TRUE
      ) %>>%
      dplyr::mutate(FamilialHypercholesterolaemia = InternalID %in%
        fHypercholesterolaemiaID) %>>%
      dplyr::mutate(LVH = InternalID %in%
        lvhID) %>>%
      dplyr::left_join(self$dM$Cholesterol_obs(cvdRiskID,
        date_from = obs_from,
        date_to = date_to
      ),
      by = "InternalID",
      copy = TRUE
      ) %>>%
      dplyr::left_join(self$dM$BloodPressure_obs(cvdRiskID,
        date_from = obs_from,
        date_to = date_to
      ),
      by = "InternalID",
      copy = TRUE
      ) %>>%
      dMeasureQIM::add_demographics(self$dM, date_to) %>>%
      dplyr::mutate(Age = dMeasure::calc_age(as.Date(DOB), date_to)) %>>% {
        dplyr::left_join(., framinghamRiskEquation::framingham_riskequation(.),
          by = "InternalID"
        )
      } %>>%
      # round age group to nearest 10 years (starting age 5)
      dplyr::select(
        Patient, InternalID, RecordNo, Sex, Ethnicity, Indigenous,
        MaritalStatus, Sexuality, Age10,
        CardiovascularDisease, Diabetes, SmokingDate, SmokingStatus,
        UrineAlbuminDate, UrineAlbuminValue, UrineAlbuminUnits,
        PersistentProteinuria, eGFRDate, eGFRValue, eGFRUnits,
        FamilialHypercholesterolaemia, LVH,
        CholesterolDate, Cholesterol, HDL, LDL, Triglycerides, CholHDLRatio,
        BPDate, BP, frisk, friskHI
      )

    self$qim_cvdRisk_list <- cvdRisk_list

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(self$qim_cvdRisk_list)
})
.reactive_event(
  dMeasureQIM, "qim_cvdRisk_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$contact_45_74_listR(),
        self$dM$contact_75plus_listR(),
        self$dM$contact_ATSI_35_44_listR(),
        self$dM$appointments_filteredR(),
        self$qim_contactR(),
        self$qim_ignoreOldR(),
        self$qim_cvdRisk_measureR()
      ), {
        self$list_qim_cvdRisk(lazy = TRUE)
      }
    )
  )
)


.public(
  dMeasureQIM, "qim_cvdRisk_list_appointments",
  data.frame(
    Patient = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(0),
    Provider = character(0),
    Status = character(0),
    RecordNo = character(),
    Age10 = integer(),
    Sex = character(),
    Indigenous = character(),
    Ethnicity = character(),
    MaritalStatus = character(),
    Sexuality = character(),
    CardiovascularDisease = logical(),
    Diabetes = logical(),
    SmokingDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    SmokingStatus = character(),
    UrineAlbuminDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    UrineAlbuminValue = double(),
    UrineAlbuminUnits = character(),
    PersistentProteinuria = logical(),
    eGFRDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    eGFRValue = double(),
    eGFRUnits = character(),
    FamilialHypercholesterolaemia = logical(),
    LVH = logical(),
    CholesterolDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Cholesterol = double(), HDL = double(), LDL = double(),
    Triglycerides = double(), CholHDLRatio = double(),
    BPDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    BP = character(),
    frisk = double(), friskHI = character(),
    stringsAsFactors = FALSE
  )
)

#' List of patient with information to complete cardiovascular risk assessment and appointment details
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 08.Proportion of patients with the necessary risk factors assessed
#'  to enable CVD assessment
#'
#' required parameters
#'
#'  Age, Ethnicity (especially ATSI status)
#'
#'   included - Age 45 to 74 years or older
#'
#'    OR Age 35 or older + ATSI
#'     (ATSI 35+ optional - included by default - see $qim_cvdRisk_measure)
#'
#'    Age 75+ excluded
#'     (Option to include - see $qim_cvdRisk_measure)
#'
#'  Known cardiovascular disease
#'     (optional - excluded by default - see $qim_cvdRisk_measure)
#'
#'  Presence of diabetes. Diabetes and microalbuminuria
#'
#'  eGFR
#'
#'  previous diagnosis of familial hypercholesterolaemia
#'
#'  systolic blood pressure. Diastolic blood pressure
#'
#'  Serum total cholesterol. Serum HDL cholesterol
#'
#' source : National Vascular Disease Prevention Alliance (NVDPA) guidelines
#' https://www.cvdcheck.org.au/australian-absolute-cardiovascular-disease-risk-calculator
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
#'
#'     FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact at most max_date. default is $contact_maxDate
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_cvdRisk_appointments <- function(dMeasureQIM_obj,
                                          contact = NA,
                                          date_from = NA,
                                          date_to = NA,
                                          clinicians = NA,
                                          min_contact = NA,
                                          min_date = NA, max_date = NA,
                                          contact_type = NA,
                                          ignoreOld = NA,
                                          lazy = FALSE) {
  dMeasureQIM_obj$list_qim_cvdRisk_appointments(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy
  )
}

.public(dMeasureQIM, "list_qim_cvdRisk_appointments", function(contact = NA,
                                                               date_from = NA,
                                                               date_to = NA,
                                                               clinicians = NA,
                                                               min_contact = NA,
                                                               min_date = NA, max_date = NA,
                                                               contact_type = NA,
                                                               ignoreOld = NA,
                                                               lazy = FALSE) {
  if (is.na(contact)) {
    contact <- self$qim_contact
  }
  if (is.na(date_from)) {
    date_from <- self$dM$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$dM$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$dM$clinicians
  }
  if (is.na(min_contact)) {
    min_contact <- self$dM$contact_min
  }
  if (is.na(min_date)) {
    min_date <- self$dM$contact_minDate
  }
  if (is.na(max_date)) {
    max_date <- self$dM$contact_maxDate
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }
  if (ignoreOld) {
    obs_from <- NA
  } else {
    obs_from <- as.Date(-Inf, origin = "1970-01-01")
    # accept very old results
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "cvdRisk_qim_appointments",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_qim_cvdRisk(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date,
        contact_type, ignoreOld,
        lazy
      )
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
        lazy = lazy
      )
    }

    self$qim_cvdRisk_list_appointments <- self$qim_cvdRisk_list %>>%
      dplyr::left_join(self$dM$appointments_filtered_time,
        by = c("InternalID", "Patient"),
        copy = TRUE
      ) %>>%
      dplyr::select(
        Patient, RecordNo, AppointmentDate, AppointmentTime,
        Provider, Status, tidyselect::everything()
      )

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(self$qim_cvdRisk_list_appointments)
})
.reactive_event(
  dMeasureQIM, "qim_cvdRisk_list_appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_cvdRisk_listR(),
        self$dM$appointments_filtered_timeR()
      ), {
        self$list_qim_cvdRisk_appointments(lazy = TRUE)
      }
    )
  )
)


.public(
  dMeasureQIM, "qim_cvdRisk_report",
  data.frame(NULL,
    stringsAsFactors = FALSE
  )
)
#' Cardiovascular disease risk Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 08.Proportion of patients with the necessary risk factors assessed to enable CVD assessment
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
#'
#'     FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact at most max_date. default is $contact_maxDate
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the cvdRisk contact list?
#'
#' @return dataframe of Patient (name), Demographic, Measure (done or not), Count, Proportion,
#'   Proportion_Demographic
#' @export
report_qim_cvdRisk <- function(dMeasureQIM_obj,
                               contact = NA,
                               date_from = NA,
                               date_to = NA,
                               clinicians = NA,
                               min_contact = NA,
                               contact_type = NA,
                               min_date = NA, max_date = NA,
                               demographic = NA,
                               ignoreOld = NA,
                               lazy = FALSE) {
  dMeasureQIM_obj$report_qim_cvdRisk(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    demographic,
    ignoreOld, lazy
  )
}
.public(dMeasureQIM, "report_qim_cvdRisk", function(contact = NA,
                                                    date_from = NA,
                                                    date_to = NA,
                                                    clinicians = NA,
                                                    min_contact = NA,
                                                    min_date = NA, max_date = NA,
                                                    contact_type = NA,
                                                    demographic = NA,
                                                    ignoreOld = NA,
                                                    lazy = FALSE) {
  if (is.na(contact)) {
    contact <- self$qim_contact
  }
  if (is.na(date_from)) {
    date_from <- self$dM$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$dM$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$dM$clinicians
  }
  if (is.na(min_contact)) {
    min_contact <- self$dM$contact_min
  }
  if (is.na(min_date)) {
    min_date <- self$dM$contact_minDate
  }
  if (is.na(max_date)) {
    max_date <- self$dM$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$dM$contact_type
  }
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  report <- self$qim_cvdRisk_report # default

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "qim_cvdRisk_report",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_qim_cvdRisk(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        ignoreOld, lazy
      )
    }

    report_groups <- c(demographic, "CVDriskDone")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    report <- self$qim_cvdRisk_list %>>%
      dplyr::mutate(CVDriskDone = !is.na(frisk) | !is.na(friskHI)) %>>%
      # a measure is 'done' if it exists (not NA)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = dplyr::n()) %>>%
      dplyr::ungroup() %>>% {
        dplyr::select(., intersect(names(.), c(report_groups, "n")))
      } %>>%
      # if no rows, then grouping will not remove unnecessary columns
      dplyr::mutate(Proportion = prop.table(n)) %>>%
      dplyr::group_by_at(demographic) %>>%
      dplyr::mutate(Proportion_Demographic = prop.table(n)) %>>%
      dplyr::ungroup()
    # proportion (an alternative would be proportion = n / sum(n))

    self$qim_cvdRisk_report <- report

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(report)
})
.reactive_event(
  dMeasureQIM, "qim_cvdRisk_reportR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_cvdRisk_listR(),
        self$qim_demographicGroupR()
      ), {
        # update if change in demographic grouping
        # or change in list
        self$report_qim_cvdRisk(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)
