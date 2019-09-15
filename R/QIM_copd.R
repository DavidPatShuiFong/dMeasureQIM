##### QIM chronic lung disease ############################################################

#' @name qim_copd
#' @title dMeasure Quality Improvement Measures - chronic lung disease
#'
#' @include QualityImprovementMeasures.R
NULL

##### QIM chronic lung disease fields ############################################################
.public(dMeasureQIM, "qim_copd_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   FluvaxDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   FluvaxName = character(),
                   stringsAsFactors = FALSE))

##### QIM chronic lung disease methods ##########################################################
#' List of patient with chronic lung disease, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 06 -Proportion of patients with COPD who were immunised against influenza
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
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID, measures
#' @export
list_qim_copd <- function(dMeasure_obj,
                          contact = "contact",
                          date_from = NA,
                          date_to = NA,
                          clinicians = NA,
                          min_contact = NA,
                          min_date = NA,
                          contact_type = NA,
                          ignoreOld = NA,
                          lazy = FALSE) {
  dMeasure_obj$list_qim_copd(contact, date_from, date_to, clinicians,
                             min_contact, min_date, contact_type,
                             ignoreOld,
                             lazy)
}

.public(dMeasureQIM, "list_qim_copd", function(contact = "contact",
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
    if (self$dM$Log) {log_id <- self$dM$config_db$write_log_db(
      query = "copd_qim",
      data = list(date_from, date_to, clinicians))}

    if (contact == "contact") {
      if (!lazy) {
        self$dM$list_contact_chroniclungdisease(date_from, date_to, clinicians,
                                                min_contact, min_date,
                                                contact_type,
                                                lazy)
      }
      copd_list <- self$dM$contact_chroniclungdisease_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      copdID <- copd_list %>>% dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty vector, which is bad for SQL filter
    } else {
      if (!lazy) {
        self$dM$filter_appointments()
      }
      copdID <- c(self$dM$chroniclungdisease_list(), -1)
      copd_list <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% copdID) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }

    fluvaxList <- self$dM$influenzaVax_obs(copdID,
                                           date_from = ifelse(ignoreOld,
                                                              NA,
                                                              as.Date(-Inf, origin = "1970-01-01")),
                                           # if ignoreOld, then influenza_vax will (given NA)
                                           # calculate date_from as fifteen months before date_to
                                           date_to = date_to)
    # returns InternalID, FluVaxName, FluvaxDate

    self$qim_copd_list <- copd_list %>>%
      dplyr::left_join(fluvaxList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$dM$db$patients %>>%
                         dplyr::filter(InternalID %in% copdID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$dM$db$clinical %>>%
                         dplyr::filter(InternalID %in% copdID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(Patient, InternalID, RecordNo, Sex, Ethnicity, MaritalStatus, Sexuality, Age5,
                    FluvaxDate, FluvaxName)

    if (self$dM$Log) {self$dM$config_db$duration_log_db(log_id)}
  }

  return(self$qim_copd_list)
})
.reactive_event(dMeasureQIM, "qim_copd_listR",
                quote(
                  shiny::eventReactive(
                    c(self$dM$contact_chroniclungdisease_listR(),
                      self$dM$appointments_filteredR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_copd(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))

.public(dMeasureQIM, "qim_copd_list_appointments",
        data.frame(Patient = character(),
                   AppointmentDate = as.Date(integer(0),
                                             origin = "1970-01-01"),
                   AppointmentTime = character(0),
                   Provider = character(0),
                   Status = character(0),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   FluvaxDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   FluvaxName = character(),
                   stringsAsFactors = FALSE))

#' List of patient with chronic lung disease, with Quality Improvement Measures, in the contact list, with appointments
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 06 -Proportion of patients with COPD who were immunised against influenza
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
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_copd_appointments <- function(contact = "contact",
                                       dMeasure_obj,
                                       date_from = NA,
                                       date_to = NA,
                                       clinicians = NA,
                                       min_contact = NA,
                                       min_date = NA,
                                       contact_type = NA,
                                       ignoreOld = NA,
                                       lazy = FALSE) {
  dMeasure_obj$list_qim_copd_appointments(contact,
                                          date_from, date_to, clinicians,
                                          min_contact, min_date, contact_type,
                                          ignoreOld,
                                          lazy)
}

.public(dMeasureQIM, "list_qim_copd_appointments", function(contact = "contact",
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
    if (self$dM$Log) {log_id <- self$dM$config_db$write_log_db(
      query = "copd_qim_appointments",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_copd(contact, date_from, date_to, clinicians,
                         min_contact, min_date,
                         contact_type,
                         lazy)
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
                                       lazy = lazy)
    }

    self$qim_copd_list_appointments <- self$qim_copd_list %>>%
      dplyr::left_join(self$dM$appointments_filtered_time,
                       by = c("InternalID", "Patient"),
                       copy = TRUE) %>>%
      dplyr::select(Patient, RecordNo, AppointmentDate, AppointmentTime,
                    Provider, Status, tidyselect::everything())

    if (self$dM$Log) {self$dM$config_db$duration_log_db(log_id)}
  }

  return(self$qim_copd_list_appointments)
})
.reactive_event(dMeasureQIM, "qim_copd_list_appointmentsR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_copd_listR(),
                      self$dM$appointments_filtered_timeR()), {
                        self$list_qim_copd_appointments(lazy = TRUE)
                      })
                ))

.public(dMeasureQIM, "qim_copd_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
#' Chronic Obstructive Pulmonary Disease Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 06 -Proportion of patients with COPD who were immunised against influenza
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
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), demographic, measure (done or not), Count, Proportion
#' @export
report_qim_copd <- function(dMeasure_obj,
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
  dMeasure_obj$report_qim_copd(contact, date_from, date_to, clinicians,
                               min_contact, min_date, contact_type,
                               demographic,
                               ignoreOld, lazy)
}
.public(dMeasureQIM, "report_qim_copd", function(contact = "contact",
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
      query = "qim_copd_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_copd(contact, date_from, date_to, clinicians,
                         min_contact, min_date, contact_type,
                         ignoreOld, lazy)
    }

    report_groups <- c(demographic, "InfluenzaDone")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    self$qim_copd_report <- self$qim_copd_list %>>%
      dplyr::mutate(InfluenzaDone = !is.na(FluvaxDate)) %>>%
      # a measure is 'done' if it exists (not NA)
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

  return(self$qim_copd_report)
})
.reactive_event(dMeasureQIM, "qim_copd_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_copd_listR(),
                      self$qim_demographicGroupR()), {
                        # react to change in demographic grouping
                        self$report_qim_copd(lazy = TRUE)
                      })
                ))
