##### QIM cervical screening test (CST) fields ###########################################################

#' dMeasureQIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @name qim_cst
#' @title dMeasure Quality Improvement Measures - cervical screening
#'
#' @include QualityImprovementMeasures.R
NULL

.public(dMeasureQIM, "qim_cst_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   CSTDate = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   CSTName = character(),
                   # CStName is expected to be 'CST' or 'PAP', but might
                   # but could be a longer string containing 'Pap' if sourced from 'Investigations' table
                   stringsAsFactors = FALSE))
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
#' @param dMeasure_obj dMeasure R6 object
#' @param contact types which are accepted. default is "contact"
#'     "contact" chooses the 'contact system dM$list_contact_diabetes ('active' patients)
#'     anything else chooses the 'appointment' system dM$diabetes_list
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent CST 'observation' (test) date and name
#' @export
list_qim_cst <- function(dMeasure_obj,
                         contact = "contact",
                         date_from = NA,
                         date_to = NA,
                         clinicians = NA,
                         min_contact = NA,
                         min_date = NA,
                         contact_type = NA,
                         ignoreOld = NA,
                         lazy = FALSE) {
  dMeasure_obj$list_qim_cst(contact, date_from, date_to, clinicians,
                            min_contact, min_date, contact_type,
                            ignoreOld,
                            lazy)
}

.public(dMeasureQIM, "list_qim_cst", function(contact = "contact",
                                              date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              min_contact = NA,
                                              min_date = NA,
                                              contact_type = NA,
                                              ignoreOld = NA,
                                              lazy = FALSE) {

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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "cst_qim",
        data = list(date_from, date_to, clinicians))}

    if (contact == "contact") {
      if (!lazy) {
        self$dM$list_contact_cst(date_from, date_to, clinicians,
                                 min_contact, min_date,
                                 contact_type,
                                 lazy)}

      screen_cst <- self$dM$contact_cst_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      screen_cst_id <- self$dM$contact_cst_list %>>%
        dplyr::pull(InternalID)
    } else {
      screen_cst_id <- c(self$dM$cst_eligible_list())
      screen_cst <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% screen_cst_id) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }
    screen_cst_id <- c(screen_cst_id, -1)
    # prevent empty ID list (bad for SQL filter!)

    self$qim_cst_list <- screen_cst %>>%
      dplyr::left_join(
        dplyr::bind_rows(
          self$dM$db$papsmears %>>%
            # attach reports in papsmears table
            dplyr::filter(InternalID %in% screen_cst_id) %>>%
            dplyr::rename(TestDate = PapDate,
                          TestName = CSTType) %>>%
            dplyr::select(InternalID, TestDate, TestName) %>>%
            dplyr::collect(),
          self$dM$db$investigations %>>%
            # some reports might be in investigations e.g. scanned in
            dplyr::filter(InternalID %in% screen_cst_id &&
                            (TestName %like% "%CERVICAL SCREENING%" ||
                               TestName %like% "%PAP SMEAR%")) %>>%
            dplyr::rename(TestDate = Reported,
                          TestName = TestName) %>>%
            dplyr::select(InternalID, TestDate, TestName) %>>%
            dplyr::collect()),
        by = "InternalID",
        copy = TRUE) %>>%
      dplyr::mutate(TestDate = as.Date(TestDate),
                    TestDate = as.Date(ifelse(TestDate > date_to,
                                              -Inf,
                                              TestDate),
                                       origin = "1970-01-01"),
                    TestDate = as.Date(ifelse(is.na(TestDate),
                                              -Inf,
                                              TestDate),
                                       origin = "1970-01-01")) %>>%
      # remove tests after the appointment date, and provide -Inf value to 'no test' patients
      dplyr::mutate(TestName = ifelse(TestDate == -Inf, NA, TestName)) %>>%
      # only test dates (and names) less than the joined appointment date are kept
      dplyr::group_by(InternalID) %>>%
      # group by patient ID (need most recent investigation for each patient)
      dplyr::filter(TestDate == max(TestDate, na.rm = TRUE)) %>>%
      dplyr::ungroup() %>>%
      dplyr::mutate(TestAge = dMeasure::interval(TestDate, date_to)$year) %>>%
      # 'current' time is date_to
      dplyr::mutate(OutOfDateTest =
                      dplyr::case_when(TestDate == -Inf ~ 1,
                                       # if no date (no detected test)
                                       TestAge < 2 ~ 3,
                                       # if less than two years, always 'up-to-date'
                                       TestAge >= 5 ~ 2,
                                       # if old (5 years for either cervical screening HPV or Pap)
                                       grepl('pap', TestName, ignore.case = TRUE) ~ 2,
                                       # otherwise if 'Pap' and more than two years
                                       # last case is 2 to 4 years (inclusive) and CST
                                       TRUE ~ 3)) %>>%
      (if (ignoreOld && nrow(.) > 0) {
        # remove out-of-date tests
        dplyr::mutate(.,
                      TestDate = dplyr::if_else(OutOfDateTest == 2,
                                                as.Date(-Inf, origin = "1970-01-01"),
                                                TestDate),
                      TestName = dplyr::if_else(OutOfDateTest == 2,
                                                as.character(NA),
                                                TestName))}
       else {.}) %>>%
      dplyr::select(-c(TestAge, OutOfDateTest)) %>>%
      # dplyr::select(-c(TestAge, OutOfDateTest)) %>>% # don't need these columns any more
      dplyr::left_join(self$dM$db$patients %>>%
                         dplyr::filter(InternalID %in% screen_cst_id) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$dM$db$clinical %>>%
                         dplyr::filter(InternalID %in% screen_cst_id) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-DOB) %>>%
      dplyr::left_join(self$dM$db$patients %>>%
                         dplyr::filter(InternalID %in% screen_cst_id) %>>%
                         dplyr::select(InternalID, RecordNo),
                       by = "InternalID", # add RecordNo
                       copy = TRUE) %>>%
      dplyr::rename(CSTDate = TestDate,
                    CSTName = TestName)

    if (self$dM$Log) {self$dM$config_db$duration_log_db(log_id)}
  }

  return(self$qim_cst_list)
})
.reactive_event(dMeasureQIM, "qim_cst_listR",
                quote(
                  shiny::eventReactive(
                    c(self$dM$contact_cst_listR(),
                      self$dM$appointments_filteredR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_cst(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


.public(dMeasureQIM, "qim_cst_list_appointments",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   AppointmentDate = as.Date(integer(0),
                                             origin = "1970-01-01"),
                   AppointmentTime = character(0),
                   Provider = character(0),
                   Status = character(0),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   CSTDate = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   CSTName = character(),
                   # CStName is expected to be 'CST' or 'PAP', but might
                   # but could be a longer string containing 'Pap' if sourced from 'Investigations' table
                   stringsAsFactors = FALSE))
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
#' @param dMeasure_obj dMeasure R6 object
#' @param contact types which are accepted. default is "contact"
#'     "contact" chooses the 'contact system dM$list_contact_diabetes ('active' patients)
#'     anything else chooses the 'appointment' system dM$diabetes_list
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_cst_appointments <- function(dMeasure_obj,
                                      contact = "contact",
                                      date_from = NA,
                                      date_to = NA,
                                      clinicians = NA,
                                      min_contact = NA,
                                      min_date = NA,
                                      contact_type = NA,
                                      ignoreOld = NA,
                                      lazy = FALSE) {
  dMeasure_obj$list_qim_cst_appointments(contact, date_from, date_to, clinicians,
                                         min_contact, min_date, contact_type,
                                         ignoreOld,
                                         lazy)
}

.public(dMeasureQIM, "list_qim_cst_appointments", function(contact = "contact",
                                                           date_from = NA,
                                                           date_to = NA,
                                                           clinicians = NA,
                                                           min_contact = NA,
                                                           min_date = NA,
                                                           contact_type = NA,
                                                           ignoreOld = NA,
                                                           lazy = FALSE) {

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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "cst_qim_appointments",
        data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_cst(contact, date_from, date_to, clinicians,
                        min_contact, min_date,
                        contact_type, ignoreOld,
                        lazy)
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
                                       lazy = lazy)
    }

    self$qim_cst_list_appointments <- self$qim_cst_list %>>%
      dplyr::left_join(self$dM$appointments_filtered_time,
                       by = c("InternalID", "Patient"),
                       copy = TRUE) %>>%
      dplyr::select(Patient, RecordNo, AppointmentDate, AppointmentTime,
                    Provider, Status, tidyselect::everything())

    if (self$dM$Log) {self$dM$config_db$duration_log_db(log_id)}
  }

  return(self$qim_cst_list_appointments)
})
.reactive_event(dMeasureQIM, "qim_cst_list_appointmentsR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_cst_listR(),
                      self$dM$appointments_filtered_timeR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_cst_appointments(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


.public(dMeasureQIM, "qim_cst_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
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
#' @param dMeasure_obj dMeasure R6 object
#' @param contact types which are accepted. default is "contact"
#'     "contact" chooses the 'contact system dM$list_contact_diabetes ('active' patients)
#'     anything else chooses the 'appointment' system dM$diabetes_list
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), demographics, measure (done or not), Count, Proportion
#' @export
report_qim_cst <- function(dMeasure_obj,
                           contact = "contact",
                           date_from = NA,
                           date_to = NA,
                           clinicians = NA,
                           min_contact = NA,
                           contact_type = NA,
                           min_date = NA,
                           demographic = NA,
                           ignoreOld = NA,
                           lazy = FALSE) {
  dMeasure_obj$report_qim_cst(contact, date_from, date_to, clinicians,
                              min_contact, min_date, contact_type,
                              demographic,
                              ignoreOld, lazy)
}

.public(dMeasureQIM, "report_qim_cst", function(contact = "contact",
                                                date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA,
                                                contact_type = NA,
                                                demographic = NA,
                                                ignoreOld = NA,
                                                lazy = FALSE) {

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

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {log_id <- self$dM$config_db$write_log_db(
      query = "qim_cst_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_cst(contact, date_from, date_to, clinicians,
                        min_contact, min_date, contact_type,
                        ignoreOld, lazy)
    }

    report_groups <- c(demographic, "CSTDone")
    # group by both demographic groupings and measure ('only CSTDate') of interest
    # add a dummy string in case there are no demographic groups chosen!

    self$qim_cst_report <- self$qim_cst_list %>>%
      dplyr::mutate(CSTDone = (CSTDate != -Inf)) %>>%
      # a measure is 'done' if it exists (not equal to Infinity)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))} %>>%
      # if no rows, then grouping will not remove unnecessary columns
      dplyr::mutate(Proportion = prop.table(n))
    # proportion (an alternative would be proportion = n / sum(n))

    if (self$dM$Log) {self$dM$config_db$duration_log_db(log_id)}
  }

  return(self$qim_cst_report)
})
.reactive_event(dMeasureQIM, "qim_cst_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_cst_listR(),
                      self$qim_demographicGroupR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        self$report_qim_cst(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))

