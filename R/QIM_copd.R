##### QIM chronic lung disease ############################################################

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @name qim_copd
#' @title dMeasure Quality Improvement Measures - chronic lung disease
#'
#' @include QualityImprovementMeasures.R
NULL

##### QIM chronic lung disease fields ############################################################
.public(
  dMeasureQIM, "qim_copd_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    RecordNo = character(),
    Age10 = integer(),
    Sex = character(),
    Indigenos = character(),
    Ethnicity = character(),
    MaritalStatus = character(),
    Sexuality = character(),
    FluvaxDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    FluvaxName = character(),
    stringsAsFactors = FALSE
  )
)

##### QIM chronic lung disease methods ##########################################################
#' List of patient with chronic lung disease, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 06 - Proportion of patients with COPD who were immunised against influenza
#'  only those age 15 years or more
#'
#'  https://www1.health.gov.au/internet/main/publishing.nsf/Content/
#'   46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-Technical-Specifications.pdf
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
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
#' @param store keep result in self$qim_copd_list
#'
#' @return dataframe of Patient (name), InternalID, measures
#' @export
list_qim_copd <- function(dMeasureQIM_obj,
                          contact = NA,
                          date_from = NA,
                          date_to = NA,
                          clinicians = NA,
                          min_contact = NA,
                          min_date = NA,
                          max_date = NA,
                          contact_type = NA,
                          ignoreOld = NA,
                          lazy = FALSE,
                          store = TRUE) {
  dMeasureQIM_obj$list_qim_copd(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_copd", function(
  contact = NA,
  date_from = NA,
  date_to = NA,
  clinicians = NA,
  min_contact = NA,
  min_date = NA,
  max_date = NA,
  contact_type = NA,
  ignoreOld = NA,
  lazy = FALSE,
  store = TRUE) {
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

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  copd_list <- self$qim_copd_list

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "copd_qim",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      if (!lazy) {
        contact_chroniclungdisease_list <-
          self$dM$list_contact_chroniclungdisease(
            date_from, date_to, clinicians,
            min_contact, min_date, max_date,
            contact_type,
            lazy, store = store
          )
      } else {
        contact_chroniclungdisease_list <-
          self$dM$contact_chroniclungdisease_list
      }
      copd_list <- contact_chroniclungdisease_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      copdID <- copd_list %>>% dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty vector, which is bad for SQL filter
      copd_list <- copd_list %>>%
        dplyr::left_join(
          self$dM$db$patients %>>%
            dplyr::filter(InternalID %in% copdID) %>>%
            dplyr::select(InternalID, DOB),
          by = "InternalID", copy = TRUE
        ) %>>%
        dplyr::mutate(DOB = as.Date(DOB),
                      Age = dMeasure::calc_age(DOB, date_to)) %>>%
        dplyr::filter(Age >= 15) %>>%
        # QIM 06 COPD requires Age of at least 15 years
        dplyr::select(-c(Age, DOB)) # don't need this anymore
      copdID <- copd_list %>>% dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty vector, which is bad for SQL filter
      # re-calculate
    } else {
      if (!lazy) {
        self$dM$filter_appointments()
      }
      copdID <- c(self$dM$chroniclungdisease_list(), -1)
      copd_list <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% copdID) %>>%
        dplyr::select(Firstname, Surname, InternalID, DOB) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(DOB = as.Date(DOB),
                      Age = dMeasure::calc_age(DOB, date_to)) %>>%
        dplyr::filter(Age >= 15) %>>%
        # QIM 06 COPD requires Age of at least 15 years
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }

    fluvaxList <- self$dM$influenzaVax_obs(
      copdID,
      date_from = ifelse(ignoreOld,
        NA,
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then influenza_vax will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns InternalID, FluVaxName, FluvaxDate

    copd_list <- copd_list %>>%
      dplyr::left_join(fluvaxList,
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dMeasureQIM::add_demographics(self$dM, date_to) %>>%
      dplyr::select(
        Patient, InternalID, RecordNo, Sex, Ethnicity, MaritalStatus, Sexuality, Age10,
        FluvaxDate, FluvaxName, Indigenous
      )

    if (store) {
      self$qim_copd_list <- copd_list
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(copd_list)
})
.reactive_event(
  dMeasureQIM, "qim_copd_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$contact_chroniclungdisease_listR(),
        self$dM$appointments_filteredR(),
        self$qim_contactR(),
        self$qim_ignoreOldR()
      ), {
        # update if reactive version of contact lists, appointment list,
        # choice between contact or appointment list ($qim_contactR),
        # or whether or not to ignore old measurements
        self$list_qim_copd(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)

.public(
  dMeasureQIM, "qim_copd_list_appointments",
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
    Ethnicity = character(),
    MaritalStatus = character(),
    Sexuality = character(),
    FluvaxDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    FluvaxName = character(),
    stringsAsFactors = FALSE
  )
)

#' List of patient with chronic lung disease, with Quality Improvement Measures, in the contact list, with appointments
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 06 - Proportion of patients with COPD who were immunised against influenza
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
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
#' @param store keep result in self$qim_copd_list_appointments
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_copd_appointments <- function(contact = NA,
                                       dMeasureQIM_obj,
                                       date_from = NA,
                                       date_to = NA,
                                       clinicians = NA,
                                       min_contact = NA,
                                       min_date = NA,
                                       max_date = NA,
                                       contact_type = NA,
                                       ignoreOld = NA,
                                       lazy = FALSE,
                                       store = TRUE) {
  dMeasureQIM_obj$list_qim_copd_appointments(
    contact,
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_copd_appointments", function(contact = NA,
                                                            date_from = NA,
                                                            date_to = NA,
                                                            clinicians = NA,
                                                            min_contact = NA,
                                                            min_date = NA,
                                                            max_date = NA,
                                                            contact_type = NA,
                                                            ignoreOld = NA,
                                                            lazy = FALSE,
                                                            store = TRUE) {
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

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  appointments <- self$qim_copd_list_appointments

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "copd_qim_appointments",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      appointments <- self$list_qim_copd(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date,
        contact_type,
        lazy, store
      )
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
        lazy = lazy
      )
    } else {
      appointments <- self$qim_copd_list
    }

    appointments <- appointments %>>%
      dplyr::left_join(self$dM$appointments_filtered_time,
        by = c("InternalID", "Patient"),
        copy = TRUE
      ) %>>%
      dplyr::select(
        Patient, RecordNo, AppointmentDate, AppointmentTime,
        Provider, Status, tidyselect::everything()
      )

    self$qim_copd_list_appointments <- appointments

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(appointments)
})
.reactive_event(
  dMeasureQIM, "qim_copd_list_appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_copd_listR(),
        self$dM$appointments_filtered_timeR()
      ), {
        self$list_qim_copd_appointments(lazy = TRUE)
      }
    )
  )
)

.public(
  dMeasureQIM, "qim_copd_report",
  data.frame(NULL,
    stringsAsFactors = FALSE
  )
)
#' Chronic Obstructive Pulmonary Disease Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 06 - Proportion of patients with COPD who were immunised against influenza
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
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
#' @param lazy recalculate the copd contact list?
#' @param store keep result in self$qim_copd_report?
#'
#' @return dataframe of Patient (name), demographic, measure (done or not), Count, Proportion,
#'   Proportion_Demographic
#' @export
report_qim_copd <- function(dMeasureQIM_obj,
                            contact = NA,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            contact_type = NA,
                            min_date = NA,
                            max_date = NA,
                            demographic = NA,
                            ignoreOld = NA,
                            lazy = FALSE,
                            store = TRUE) {
  dMeasureQIM_obj$report_qim_copd(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    demographic,
    ignoreOld, lazy, store
  )
}
.public(dMeasureQIM, "report_qim_copd", function(contact = NA,
                                                 date_from = NA,
                                                 date_to = NA,
                                                 clinicians = NA,
                                                 min_contact = NA,
                                                 min_date = NA,
                                                 max_date = NA,
                                                 contact_type = NA,
                                                 demographic = NA,
                                                 ignoreOld = NA,
                                                 lazy = FALSE,
                                                 store = TRUE) {
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
  if (length(demographic) == 1 && is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  report <- self$qim_copd_report

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "qim_copd_report",
        data = list(date_from, date_to, clinicians)
      )
    }

    report_groups <- c(demographic, "InfluenzaDone")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    if (!lazy) {
      report <- self$list_qim_copd(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        ignoreOld, lazy, store
      )
    } else {
      report <- self$qim_copd_list
    }

    report <- report %>>%
      dplyr::mutate(InfluenzaDone = !is.na(FluvaxDate)) %>>%
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

    if (store) {
      self$qim_copd_report <- report
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(report)
})
.reactive_event(
  dMeasureQIM, "qim_copd_reportR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_copd_listR(),
        self$qim_demographicGroupR()
      ), {
        # react to change in demographic grouping or list
        self$report_qim_copd(lazy = TRUE)
      }
    )
  )
)
