
#' @name qim_active
#' @title dMeasure Quality Improvement Measures - active patient list
#'
#' @include QualityImprovementMeasures.R
NULL


##### QIM active fields #############################################################
.public(dMeasureQIM, "qim_active_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   Count = integer(),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### QIM active methods ##########################################################
#' List of active patients, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID and demographics
#' @export
list_qim_active <- function(dMeasure_obj,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            min_date = NA,
                            contact_type = NA,
                            lazy = FALSE) {
  dMeasure_obj$list_qim_active(date_from, date_to, clinicians,
                               min_contact, min_date, contact_type,
                               lazy)
}
.public(dMeasureQIM, "list_qim_active", function(date_from = NA,
                                                 date_to = NA,
                                                 clinicians = NA,
                                                 min_contact = NA,
                                                 min_date = NA,
                                                 contact_type = NA,
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

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {log_id <- self$dM$config_db$write_log_db(
      query = "active_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$dM$list_contact_count(date_from, date_to, clinicians,
                                 min_contact, min_date, contact_type,
                                 lazy)
    }

    activeID <- self$dM$contact_count_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector

    self$qim_active_list <- self$dM$contact_count_list %>>%
      dplyr::select(-c(Latest)) %>>% # don't need these fields
      dplyr::left_join(self$dM$db$patients %>>%
                         dplyr::filter(InternalID %in% activeID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$dM$db$clinical %>>%
                         dplyr::filter(InternalID %in% activeID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-DOB) %>>%
      dplyr::left_join(self$dM$db$patients %>>%
                         dplyr::filter(InternalID %in% activeID) %>>%
                         dplyr::select(InternalID, RecordNo),
                       by = "InternalID", # add RecordNo
                       copy = TRUE)

    if (self$dM$Log) {self$dM$config_db$duration_log_db(log_id)}
  }

  return(self$qim_active_list)
})
.reactive_event(dMeasureQIM, "qim_active_listR",
                quote(
                  shiny::eventReactive(
                    c(self$dM$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_qim_active(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))


.public(dMeasureQIM, "qim_active_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
# empty data frame, number of columns dynamically change

#' Quality Improvement Measure report, in the contact list. Active contacts
#'
#' Filtered by date, and chosen clinicians
#'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), demographics, Count, Proportion
#' @export
report_qim_active <- function(dMeasure_obj,
                              date_from = NA,
                              date_to = NA,
                              clinicians = NA,
                              min_contact = NA,
                              min_date = NA,
                              contact_type = NA,
                              demographic = NA,
                              lazy = FALSE) {
  dMeasure_obj$report_qim_active(date_from, date_to, clinicians,
                                 min_contact, min_date,
                                 contact_type,
                                 demographic,
                                 lazy)
}

.public(dMeasureQIM, "report_qim_active", function(date_from = NA,
                                                   date_to = NA,
                                                   clinicians = NA,
                                                   min_contact = NA,
                                                   min_date = NA,
                                                   contact_type = NA,
                                                   demographic = NA,
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

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {log_id <- self$dM$config_db$write_log_db(
      query = "qim_active_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_active(date_from, date_to, clinicians,
                           min_contact, min_date, contact_type,
                           lazy)
    }

    report_groups <- c(demographic, "")
    # group by demographic groupings
    # add a dummy string in case there are no demographic chosen!

    self$qim_active_report <- self$qim_active_list %>>%
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

  return(self$qim_active_report)
})
.reactive_event(dMeasureQIM, "qim_active_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_active_listR(),
                      self$qim_demographicGroupR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        self$report_qim_active(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))
