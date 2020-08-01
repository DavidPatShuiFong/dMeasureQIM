##### QIM 65 plus ##################################################################

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @name qim_65plus
#' @title dMeasure Quality Improvement Measures - 65 plus
#'
#' @include QualityImprovementMeasures.R
NULL

##### QIM 65 plus fields ############################################################
.public(
  dMeasureQIM, "qim_65plus_list",
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
    FluvaxDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    FluvaxName = character(),
    stringsAsFactors = FALSE
  )
)

##### QIM 65 plus methods ##########################################################
#' List of 65 years or older, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 04 - Influenza immunization - most recent. the QIM measure is within last 15 months
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
#' @param lazy recalculate the 65+ contact list?
#' @param store keep result in self$qim_65plus_list
#'
#' @return dataframe of Patient (name), InternalID and measures
#' @export
list_qim_65plus <- function(dMeasureQIM_obj,
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
  dMeasureQIM_obj$list_qim_65plus(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_65plus", function(contact = NA,
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

  sixtyfiveplus_list <- self$qim_65plus_list

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "sixtyfiveplus_qim",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      if (!lazy) {
        sixtyfiveplus_list <- self$dM$list_contact_65plus(
          date_from, date_to, clinicians,
          min_contact, min_date, max_date,
          contact_type,
          lazy, store
        )
      } else {
        sixtyfiveplus_list <- self$dM$contact_65plus_list
      }
      sixtyfiveplus_list <- sixtyfiveplus_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      sixtyfiveplusID <- sixtyfiveplus_list %>>%
        dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty vector, which is bad for SQL filter
    } else {
      if (!lazy) {
        self$dM$filter_appointments()
      }
      sixtyfiveplusID <- c(self$dM$sixtyfiveplus_list(), -1)
      sixtyfiveplus_list <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% sixtyfiveplusID) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }

    fluvaxList <- self$dM$influenzaVax_obs(sixtyfiveplusID,
      date_from = ifelse(ignoreOld,
        NA,
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then influenza_vax will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns InternalID, FluVaxName, FluvaxDate

    sixtyfiveplus_list <- sixtyfiveplus_list %>>%
      dplyr::left_join(fluvaxList,
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dMeasureQIM::add_demographics(self$dM, date_to) %>>%
      dplyr::select(
        Patient, InternalID, RecordNo, Sex, Ethnicity, Indigenous,
        MaritalStatus, Sexuality, Age10,
        FluvaxDate, FluvaxName
      )

    self$qim_65plus_list <- sixtyfiveplus_list

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(sixtyfiveplus_list)
})
.reactive_event(
  dMeasureQIM, "qim_65plus_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$contact_65plus_listR(),
        self$dM$appointments_filteredR(),
        self$qim_contactR(),
        self$qim_ignoreOldR()
      ), {
        # update if reactive version of contact or appointment lists
        # choice between contact or appointment list ($qim_contact)
        # or whether or not to ignore old measures
        self$list_qim_65plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)

.public(
  dMeasureQIM, "qim_65plus_list_appointments",
  data.frame(
    Patient = character(),
    RecordNo = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(0),
    Provider = character(0),
    Status = character(0),
    Age10 = integer(),
    Sex = character(),
    Indigenous = character(),
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

#' List of 65 years or older, with Quality Improvement Measures, in the contact list, with appointments
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 04 - Influenza immunization - most recent. the QIM measure is within last 15 months
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
#' @param lazy recalculate the 65+ contact list?
#' @param store keep result in self$qim_65plus_list_appointments
#'
#' @return dataframe of Patient (name), InternalID and measures
#' @export
list_qim_65plus_appointments <- function(dMeasureQIM_obj,
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
  dMeasureQIM_obj$list_qim_65plus_appointments(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_65plus_appointments", function(contact = NA,
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

  appointments <- self$qim_65plus_list_appointments

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "sixtyfiveplus_qim_appointments",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      appointments <- self$list_qim_65plus(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date,
        contact_type, ignoreOld,
        lazy, store
      )
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
        lazy = lazy
      )
    } else {
      appointments <- self$qim_65plus_list
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

    self$qim_65plus_list_appointments <- appointments

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(appointments)
})
.reactive_event(
  dMeasureQIM, "qim_65plus_list_appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_65plus_listR(),
        self$dM$appointments_filtered_timeR()
      ), {
        # update if reactive version of 65plus_list
        # or appointment list
        self$list_qim_65plus_appointments(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)

.public(
  dMeasureQIM, "qim_65plus_report",
  data.frame(NULL,
    stringsAsFactors = FALSE
  )
)
#' Age 65+ Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 04 -Proportion of patients aged 65 and over who were immunised against influenza
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
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -
#' @param max_date most recent contact at most max_date. default is $contact_maxDate
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the 65+ contact list?
#' @param store keep result in self$qim_65plus_report?
#'
#' @return dataframe of Patient (name), demographic, Count, Proportion, Proportion_Demographic
#' @export
report_qim_65plus <- function(dMeasureQIM_obj,
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
  dMeasureQIM_obj$report_qim_65plus(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    demographic,
    ignoreOld, lazy, store
  )
}
.public(dMeasureQIM, "report_qim_65plus", function(contact = NA,
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

  report <- self$qim_65plus_report

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "qim_65plus_report",
        data = list(date_from, date_to, clinicians)
      )
    }

    report_groups <- c(demographic, "InfluenzaDone")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    if (!lazy) {
      report <- self$list_qim_65plus(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        ignoreOld, lazy, store
      )
    } else {
      report <- self$qim_65plus_list
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
      self$qim_65plus_report <- report
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(report)
})
.reactive_event(
  dMeasureQIM, "qim_65plus_reportR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_65plus_listR(),
        self$qim_demographicGroupR()
      ), {
        # or change in demographic grouping
        # or the 65plus_list
        self$report_qim_65plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)
