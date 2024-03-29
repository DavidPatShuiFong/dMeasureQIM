##### QIM cervical screening test (CST) fields ###########################################################

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureQIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @name qim_cst
#' @title dMeasure Quality Improvement Measures - cervical screening
#'
#' @include QualityImprovementMeasures.R
NULL

.public(
  dMeasureQIM, "qim_cst_list",
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
    CSTDate = as.Date(integer(0),
                      origin = "1970-01-01"
    ),
    CSTName = character(),
    # CStName is expected to be 'CST' or 'PAP', but might
    # but could be a longer string containing 'Pap' if sourced from 'Investigations' table
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### QIM cervical screening (CST) methods ##########################################################
#' List of CST eligible patients with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 09 -Proportion of female patients with an up-to-date cervical screening
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
#' @param lazy force re-calculate?
#' @param store keep result in  self$qim_cst_list_appointments
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent CST 'observation' (test) date and name
#' @export
list_qim_cst <- function(dMeasureQIM_obj,
                         contact = NA,
                         date_from = NA,
                         date_to = NA,
                         clinicians = NA,
                         min_contact = NA,
                         min_date = NA, max_date = NA,
                         contact_type = NA,
                         ignoreOld = NA,
                         lazy = FALSE,
                         store = TRUE) {
  dMeasureQIM_obj$list_qim_cst(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_cst", function(contact = NA,
                                              date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              min_contact = NA,
                                              min_date = NA, max_date = NA,
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

  screen_cst <- self$qim_cst_list

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "cst_qim",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      if (!lazy) {
        contact_cst_list <- self$dM$list_contact_cst(
          date_from, date_to, clinicians,
          min_contact, min_date, max_date,
          contact_type,
          lazy, store
        )
      } else {
        contact_cst_list <- self$dM$contact_cst_list
      }
      screen_cst <- contact_cst_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      screen_cst_id <- contact_cst_list %>>%
        dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty list
    } else {
      if (!lazy) {
        self$dM$filter_appointments()
      }
      screen_cst_id <- c(self$dM$cst_eligible_list(), -1)
      screen_cst <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% screen_cst_id) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }

    screen_cst <- screen_cst %>>%
      dplyr::left_join(
        dplyr::bind_rows(
          self$dM$db$papsmears %>>%
            # attach reports in papsmears table
            dplyr::filter(InternalID %in% screen_cst_id) %>>%
            dplyr::rename(
              TestDate = PapDate,
              TestName = CSTType
            ) %>>%
            dplyr::select(InternalID, TestDate, TestName) %>>%
            dplyr::collect(),
          self$dM$db$investigations %>>%
            # some reports might be in investigations e.g. scanned in
            dplyr::filter(
              InternalID %in% screen_cst_id,
              (TestName %like% "%CERVICAL SCREENING%" |
                 TestName %like% "%PAP SMEAR%")
            ) %>>%
            dplyr::rename(
              TestDate = Reported,
              TestName = TestName
            ) %>>%
            dplyr::select(InternalID, TestDate, TestName) %>>%
            dplyr::collect()
        ),
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dplyr::mutate(
        TestDate = as.Date(TestDate),
        TestDate = as.Date(ifelse(TestDate > date_to,
                                  -Inf,
                                  TestDate
        ),
        origin = "1970-01-01"
        ),
        TestDate = as.Date(ifelse(is.na(TestDate),
                                  -Inf,
                                  TestDate
        ),
        origin = "1970-01-01"
        )
      ) %>>%
      # remove tests after the appointment date, and provide -Inf value to 'no test' patients
      dplyr::mutate(TestName = ifelse(TestDate == -Inf, NA, TestName)) %>>%
      # only test dates (and names) less than the joined appointment date are kept
      dplyr::group_by(InternalID) %>>%
      # group by patient ID (need most recent investigation for each patient)
      dplyr::arrange(dplyr::desc(TestDate), .by_group = TRUE) %>>%
      dplyr::filter(dplyr::row_number() == 1) %>>%
      # 'max' of TestDate, breaking 'ties'
      # 'arrange' places NA at end of list, so using 'desc' places 'max' on 'top'
      dplyr::ungroup() %>>%
      dplyr::mutate(TestAge = dMeasure::interval(TestDate, date_to)$year) %>>%
      # 'current' time is date_to
      dplyr::mutate(
        OutOfDateTest =
          dplyr::case_when(
            TestDate == -Inf ~ 1,
            # if no date (no detected test)
            TestAge < 2 ~ 3,
            # if less than two years, always 'up-to-date'
            TestAge >= 5 ~ 2,
            # if old (5 years for either cervical screening HPV or Pap)
            grepl("pap", TestName, ignore.case = TRUE) ~ 2,
            # otherwise if 'Pap' and more than two years
            # last case is 2 to 4 years (inclusive) and CST
            TRUE ~ 3
          )
      ) %>>%
      (if (ignoreOld && nrow(.) > 0) {
        # remove out-of-date tests
        dplyr::mutate(.,
                      TestDate = dplyr::if_else(
                        OutOfDateTest == 2,
                        as.Date(-Inf, origin = "1970-01-01"),
                        TestDate
                      ),
                      TestName = dplyr::if_else(
                        OutOfDateTest == 2,
                        as.character(NA),
                        as.character(TestName)
                      )
        )
      }
      else {
        .
      }) %>>%
      dplyr::select(-c(TestAge, OutOfDateTest)) %>>%
      # dplyr::select(-c(TestAge, OutOfDateTest)) %>>% # don't need these columns any more
      dMeasureQIM::add_demographics(
        self$dM, date_to,
        ageGroups = c(25, 35, 45, 55, 65, 70)
        # QIM 08 cervical screening is different to all other QIM measures
        # in having a '70' age group (70 to 74)
      ) %>>%
      dplyr::select(-DOB) %>>%
      dplyr::rename(
        CSTDate = TestDate,
        CSTName = TestName
      )

    if (store) {
      self$qim_cst_list <- screen_cst
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(screen_cst)
})
.reactive_event(
  dMeasureQIM, "qim_cst_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$contact_cst_listR(),
        self$dM$appointments_filteredR(),
        self$qim_contactR(),
        self$qim_ignoreOldR()
      ), {
        self$list_qim_cst(lazy = TRUE)
      }
    )
  )
)


.public(
  dMeasureQIM, "qim_cst_list_appointments",
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
    CSTDate = as.Date(integer(0),
                      origin = "1970-01-01"
    ),
    CSTName = character(),
    # CStName is expected to be 'CST' or 'PAP', but might
    # but could be a longer string containing 'Pap' if sourced from 'Investigations' table
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

#' List of CST eligible patients with Quality Improvement Measures,
#' in the contact list, with appointment details
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 09 -Proportion of female patients with an up-to-date cervical screening
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
#' @param lazy recalculate the diabetes contact list?
#' @param store keep result in  self$qim_cst_list_appointments
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_cst_appointments <- function(dMeasureQIM_obj,
                                      contact = NA,
                                      date_from = NA,
                                      date_to = NA,
                                      clinicians = NA,
                                      min_contact = NA,
                                      min_date = NA, max_date = NA,
                                      contact_type = NA,
                                      ignoreOld = NA,
                                      lazy = FALSE,
                                      store = TRUE) {
  dMeasureQIM_obj$list_qim_cst_appointments(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_cst_appointments", function(contact = NA,
                                                           date_from = NA,
                                                           date_to = NA,
                                                           clinicians = NA,
                                                           min_contact = NA,
                                                           min_date = NA, max_date = NA,
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

  appointments <- self$qim_cst_list_appointments

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "cst_qim_appointments",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      appointments <- self$list_qim_cst(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date,
        contact_type, ignoreOld,
        lazy, store
      )
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
                                       lazy = lazy
      )
    } else {
      appointments <- self$qim_cst_list
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

    self$qim_cst_list_appointments <- appointments

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(appointments)
})
.reactive_event(
  dMeasureQIM, "qim_cst_list_appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_cst_listR(),
        self$dM$appointments_filtered_timeR()
      ), {
        self$list_qim_cst_appointments(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)


.public(
  dMeasureQIM, "qim_cst_report",
  data.frame(NULL,
             stringsAsFactors = FALSE
  )
)
# empty data frame, number of columns dynamically change

#' Cervical Screening Test (CST) Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 09 -Proportion of female patients with an up-to-date cervical screening
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
#' @param lazy recalculate the diabetes contact list?
#' @param store keep result in self$qim_cst_report
#'
#' @return dataframe of Patient (name), demographics, measure (done or not), Count, Proportion,
#'   Proportion_Demographic
#'
#'   Does not include if 'No longer requires cervical screening' is set
#'   OR 'Opt out of cervical screening' (reasons excluded include 'has screening elsewhere' and
#'   'refuses'  but does not include "Doesn't want reminders sent")
#' @export
report_qim_cst <- function(dMeasureQIM_obj,
                           contact = NA,
                           date_from = NA,
                           date_to = NA,
                           clinicians = NA,
                           min_contact = NA,
                           contact_type = NA,
                           min_date = NA, max_date = NA,
                           demographic = NA,
                           ignoreOld = NA,
                           lazy = FALSE,
                           store = TRUE) {
  dMeasureQIM_obj$report_qim_cst(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    demographic,
    ignoreOld, lazy, store
  )
}

.public(dMeasureQIM, "report_qim_cst", function(contact = NA,
                                                date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA, max_date = NA,
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

  report <- self$qim_cst_report

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "qim_cst_report",
        data = list(date_from, date_to, clinicians)
      )
    }

    report_groups <- c(demographic, "CSTDone")
    # group by both demographic groupings and measure ('only CSTDate') of interest
    # add a dummy string in case there are no demographic groups chosen!

    if (!lazy) {
      report <- self$list_qim_cst(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        ignoreOld, lazy, store
      )
    } else {
      report <- self$qim_cst_list
    }

    report <- report %>>%
      dplyr::filter(
        # according to PIP QI Improvement Measures Technical Specifications V1.2 (22102020)
        # QIM 09, page 27
        #
        # Exclude clients from the calculation if they:
        #  - Have had a complete hysterectomy
        #  - did not have the test due to documented medical reasons, system
        #    reasons (test not available), or patient reasons (e.g. refusal); or
        #  - had results from measurements conducted outside of the service which
        #    were not available to the service; or
        #  - no longer require testing.
        !(InternalID %in%
            (self$dM$db$obgyndetail %>>%
               dplyr::filter(NoPap == 1) %>>%
               # those who have been marked as not for influenza reminders
               dplyr::pull(InternalID))
        ) &
          !(InternalID %in%
              (self$dM$db$obgyndetail %>>%
                 dplyr::filter(
                   toupper(OptOutReason) %in%
                     !!c(toupper("Has screening at another practice"),
                         toupper("Has screening done by specialist"),
                         toupper("Refuses to have screening")
                     ) # the !! evaluates c() and 'toupper' in the R session
                   # (rather than translate to SQL)
                   # note that this does *not* include "Doesn't want reminders sent"
                 ) %>>%
                 dplyr::pull(InternalID))
          )
      ) %>>%
      dplyr::mutate(CSTDone = (CSTDate != -Inf)) %>>%
      # a measure is 'done' if it exists (not equal to Infinity)
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
      self$qim_cst_report <- report
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(report)
})
.reactive_event(
  dMeasureQIM, "qim_cst_reportR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_cst_listR(),
        self$qim_demographicGroupR()
      ), {
        # or change in demographic grouping
        self$report_qim_cst(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)
