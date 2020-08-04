##### QIM diabetes fields ###########################################################

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureQIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @name qim_diabetes
#' @title dMeasure Quality Improvement Measures - diabetes
#'
#' @include QualityImprovementMeasures.R
NULL

.public(
  dMeasureQIM, "qim_diabetes_list",
  data.frame(
    Patient = character(),
    RecordNo = character(),
    InternalID = integer(),
    Age10 = integer(),
    Sex = character(),
    Indigenous = character(),
    Ethnicity = character(),
    MaritalStatus = character(),
    Sexuality = character(),
    HbA1CDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    HbA1CValue = double(),
    HbA1CUnits = character(),
    FluvaxDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    FluvaxName = character(),
    BPDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    BP = character(),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### QIM diabetes methods ##########################################################
#' List of diabetics, with Quality Improvement Measures, in the contact list
#'
#' @md
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 01 - HbA1C - most recent. the QIM measure is within last twelve months
#' QIM 05 - Influenza immunization - most recent. the QIM measure is within last 15 months
#' QIM 10 - Blood pressure - most recent. the QIM measure is within the last six months
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from dMeasure object.
#'     FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is dM$clinicians
#' @param min_contact minimum number of contacts. default is dM$contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is dM$contact_minDate, initially -Inf
#' @param max_date most recent contact at most max_date. default is $contact_maxDate
#' @param contact_type contact types which are accepted. default is dM$$contact_type
#' @param type_diabetes add type of diabetes?
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#' @param store keep result in self$qim_diabetes_report?
#'
#' @return dataframe of Patient (name), InternalID and measures
#'   if type_diabetes is TRUE, also add DiabetesType ('Type 1', 'Type 2', `NA`)
#' @export
list_qim_diabetes <- function(dMeasureQIM_obj,
                              contact = NA,
                              date_from = NA,
                              date_to = NA,
                              clinicians = NA,
                              min_contact = NA,
                              min_date = NA,
                              max_date = NA,
                              contact_type = NA,
                              type_diabetes = NA,
                              ignoreOld = NA,
                              lazy = FALSE,
                              store = TRUE) {
  dMeasureQIM_obj$list_qim_diabetes(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    type_diabetes, ignoreOld,
    lazy, store
  )
}

.public(dMeasureQIM, "list_qim_diabetes", function(contact = NA,
                                                   date_from = NA,
                                                   date_to = NA,
                                                   clinicians = NA,
                                                   min_contact = NA,
                                                   min_date = NA,
                                                   max_date = NA,
                                                   contact_type = NA,
                                                   type_diabetes = NA,
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
  if (is.na(type_diabetes)) {
    type_diabetes <- self$qim_diabetes_showType
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  diabetes_list <- self$qim_diabetes_list # default

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "diabetes_qim",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      # choose from 'contact' lists, which are based on appointments, billings or services
      if (!lazy) {
        diabetes_list <- self$dM$list_contact_diabetes(
          date_from, date_to, clinicians,
          min_contact, min_date, max_date,
          contact_type,
          lazy
        )
      } else {
        diabetes_list <- self$dM$contact_diabetes_list
      }
      diabetes_list <- diabetes_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      diabetesID <- diabetes_list %>>%
        dplyr::pull(InternalID) %>>%
        c(-1) # prevent empty ID list (bad for SQL filter!)
    } else {
      # choose from appointment book alone
      if (!lazy) {
        self$dM$filter_appointments()
      }
      diabetesID <- c(self$dM$diabetes_list(), -1)
      diabetes_list <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% diabetesID) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }

    if (type_diabetes) {
      intID_type1 <- c(
        self$dM$diabetes_type1_list(
          data.frame(InternalID = diabetesID, Date = date_to)
          ),
        -1
      )
      intID_type2 <- c(
        self$dM$diabetes_type2_list(
          data.frame(InternalID = diabetesID, Date = date_to)
          ),
        -1
      )

      diabetes_list <- diabetes_list %>>%
        dplyr::mutate(
          DiabetesType = dplyr::case_when(
            InternalID %in% intID_type1 &
              !(InternalID %in% intID_type2) ~ "Type 1",
            InternalID %in% intID_type2 &
              !(InternalID %in% intID_type1) ~ "Type 2",
            TRUE ~ as.character(NA)
            # note that if marked as 'type 1' AND 'type 2' then
            # is marked as 'not available'
          )
        )
    }

    fluvaxList <- self$dM$influenzaVax_obs(diabetesID,
      date_from = ifelse(ignoreOld,
        NA,
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then influenza_vax will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns InternalID, FluVaxName, FluvaxDate

    HbA1CList <- self$dM$HbA1C_obs(diabetesID,
      date_from = ifelse(
        ignoreOld,
        NA,
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then HbA1C will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns dataframe of InternalID, HbA1CDate, HbA1CValue, HbA1CUnits

    BPList <- self$dM$BloodPressure_obs(diabetesID,
      date_from = ifelse(
        ignoreOld,
        NA,
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then BP will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns dataframe of InternalID, BPDate, BPValue

    diabetes_list <- diabetes_list %>>%
      dplyr::left_join(HbA1CList,
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dplyr::mutate(HbA1CValue = as.double(HbA1CValue)) %>>%
      # was a character. can't be converted to double within the MSSQL query
      dplyr::left_join(fluvaxList,
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dplyr::left_join(BPList,
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dMeasureQIM::add_demographics(self$dM, date_to) %>>%
      dplyr::select(-c(DOB))

    if (store) {
      self$qim_diabetes_list <- diabetes_list
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(diabetes_list)
})
.reactive_event(
  dMeasureQIM, "qim_diabetes_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$contact_diabetes_listR(),
        self$dM$appointments_filteredR(),
        self$qim_contactR(),
        self$qim_ignoreOldR(),
        self$qim_diabetes_showTypeR()
      ), {
        self$list_qim_diabetes(lazy = TRUE)
      }
    )
  )
)

.active(dMeasureQIM, "qim_diabetes_measureTypes", function(value) {
  if (!missing(value)) {
    warning("$qim_diabetes_measureTypes is read-only.")
  } else {
    return(c("HbA1C", "Influenza", "BP"))
    # vector of valid QIM measures for diabetes (for QIM reporting)
    # QIM 01 - HbA1C
    # QIM 05 - Influenza
    # QIM 10 - Blood pressure
  }
})

.private(dMeasureQIM, ".qim_diabetes_showType", TRUE)
.active(dMeasureQIM, "qim_diabetes_showType", function(value) {
  # add diabetes type ("Type 1", "Type 2", or NA) to list diabetes list?
  if (missing(value)) {
    return(private$.qim_diabetes_showType)
  }
  if (is.logical(value)) {
    private$.qim_diabetes_showType <- value
    private$set_reactive(self$qim_diabetes_showTypeR, value)
  } else {
    warning("$qim_diabetes_showType only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasureQIM, "qim_diabetes_showTypeR", TRUE)

.private_init(dMeasureQIM, ".qim_diabetes_measure", quote(self$qim_diabetes_measureTypes))
.active(dMeasureQIM, "qim_diabetes_measure", function(value) {
  # which measures to include
  # this is ignored by dMeasure qim_diabetes_list, but used by qim_diabetes_report
  # the GPstat! frontend uses this to filter the results of qim_diabetes_list
  if (missing(value)) {
    return(private$.qim_diabetes_measure)
  }
  value <- intersect(value, self$qim_diabetes_measureTypes)
  # only valid measure types allowed
  private$.qim_diabetes_measure <- value
  private$set_reactive(self$qim_diabetes_measureR, value)
})
.reactive(dMeasureQIM, "qim_diabetes_measureR", quote(self$qim_diabetes_measureTypes))

.public(
  dMeasureQIM, "qim_diabetes_list_appointments",
  data.frame(
    Patient = character(),
    RecordNo = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(0),
    Provider = character(0),
    Status = character(0),
    Age10 = integer(0),
    Sex = character(0),
    Indigenous = character(0),
    Ethnicity = character(0),
    MaritalStatus = character(0),
    Sexuality = character(0),
    HbA1CDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    HbA1CValue = double(0),
    HbA1CUnits = character(0),
    FluvaxDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    FluvaxName = character(),
    BPDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    BP = character(0),
    stringsAsFactors = FALSE
  )
)

# filtered by chosen dates and clinicians and number of contacts)
#' List of diabetics, with Quality Improvement Measures, in contact list plus appointment details
#'
#' Filtered by date, and chosen clinicians.
#'  Note that a 'contact' could potentially be defined as something else other than
#'  an appointment! (e.g. billing, or record of visit)
#'
#' QIM 01 - HbA1C - most recent. the QIM measure is within last twelve months
#'
#' QIM 05 - Influenza immunization - most recent. the QIM measure is within last 15 months
#'
#' QIM 10 - Blood pressure - most recent. the QIM measure is within the last six months
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasureQIM_obj dMeasureQIM R6 object
#' @param contact patient list. default is $qim_contact.
#'     TRUE chooses the 'contact' system $list_contact_diabetes ('active' patients) from $dMeasure object.
#'     FALSE chooses the 'appointment' system $diabetes_list from dMeasure object.
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact at most max_date. default is $contact_maxDate
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param type_diabetes add type of diabetes?
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#' @param store keep result in self$qim_diabetes_list_appointments
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_diabetes_appointments <- function(dMeasureQIM_obj,
                                           contact = NA,
                                           date_from = NA,
                                           date_to = NA,
                                           clinicians = NA,
                                           min_contact = NA,
                                           min_date = NA,
                                           max_date = NA,
                                           type_diabetes = NA,
                                           contact_type = NA,
                                           ignoreOld = NA,
                                           lazy = FALSE,
                                           store = TRUE) {
  dMeasureQIM_obj$list_qim_diabetes_appointments(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    type_diabetes, ignoreOld,
    lazy, store
  )
}
.public(dMeasureQIM, "list_qim_diabetes_appointments", function(contact = NA,
                                                                date_from = NA,
                                                                date_to = NA,
                                                                clinicians = NA,
                                                                min_contact = NA,
                                                                min_date = NA,
                                                                max_date = NA,
                                                                contact_type = NA,
                                                                type_diabetes = NA,
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
  if (is.na(type_diabetes)) {
    type_diabetes <- self$qim_diabetes_showType
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  appointments <- self$qim_diabetes_list_appointments

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "diabetes_qim_appointments",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      appointments <- self$list_qim_diabetes(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date,
        contact_type, type_diabetes, ignoreOld,
        lazy, store
      )
      self$dM$filter_appointments_time(date_from, date_to, clinicians,
        lazy = lazy
      )
    } else {
      appointments <- self$qim_diabetes_list
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

    self$qim_diabetes_list_appointments <- appointments

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(appointments)
})
.reactive_event(
  dMeasureQIM, "qim_diabetes_list_appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_diabetes_listR(),
        self$dM$appointments_filtered_timeR()
      ), {
        self$list_qim_diabetes_appointments(lazy = TRUE)
        # re-calculates the appointments
      }
    )
  )
)

.public(
  dMeasureQIM, "qim_diabetes_report",
  data.frame(NULL,
    stringsAsFactors = FALSE
  )
)
# empty data frame, number of columns dynamically change

#' Diabetes Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 01 - HbA1C - most recent. the QIM measure is within last twelve months
#' QIM 05 - Influenza immunization - most recent. the QIM measure is within last 15 months
#' QIM 10 - Blood pressure - most recent. the QIM measure is within the last six monthe
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
#' @param measure measures to report
#'  if not supplied, reads $qim_diabetes_measure
#'  list of available measures in $qim_diabetes_measureTypes
#'  currently 'HbA1C', 'Influenza' and 'BP'
#' @param type_diabetes separate diabetes by type?
#' @param ignoreOld ignore results/observatioins that don't qualify
#'  for quality improvement measures.
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#' @param store keep result in self$qim_diabetes_report?
#'
#' @return dataframe of Patient (name), demographics, measure (done or not),
#'  InternalID, Count, Proportion, Proportion_demographic.
#'  if type_diabetes set to 'TRUE' then add type_diabetes
#' @export
report_qim_diabetes <- function(dMeasureQIM_obj,
                                contact = NA,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                contact_type = NA,
                                min_date = NA,
                                max_date = NA,
                                demographic = NA,
                                measure = NA,
                                type_diabetes = NA,
                                ignoreOld = NA,
                                lazy = FALSE,
                                store = TRUE) {
  dMeasureQIM_obj$report_qim_diabetes(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    demographic, measure, type_diabetes,
    ignoreOld, lazy, store = TRUE
  )
}
.public(dMeasureQIM, "report_qim_diabetes", function(contact = NA,
                                                     date_from = NA,
                                                     date_to = NA,
                                                     clinicians = NA,
                                                     min_contact = NA,
                                                     min_date = NA,
                                                     max_date = NA,
                                                     contact_type = NA,
                                                     demographic = NA,
                                                     measure = NA,
                                                     type_diabetes = NA,
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
  if (is.na(measure)) {
    measure <- self$qim_diabetes_measure
  }
  if (is.na(type_diabetes)) {
    type_diabetes <- self$qim_diabetes_showType
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  report <- self$qim_diabetes_report # default

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "qim_diabetes_report",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (type_diabetes) {
      demographic <- c(demographic, "DiabetesType")
      # diabetes type becomes a 'grouping' demographic
    }

    measure <- dplyr::recode(measure,
      "HbA1C" = "HbA1CDone",
      "Influenza" = "InfluenzaDone",
      "BP" = "BPDone"
    )
    report_groups <- c(demographic, measure)
    # report_groups <- c(demographic, measure, "")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!
    # (dummy string not required?, both group_by_at accepts NULL)

    if (!lazy) {
      report <- self$list_qim_diabetes(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        ignoreOld, lazy, store
      )
    } else {
      report <- self$qim_diabetes_list
    }

    report <- report %>>%
      dplyr::mutate(
        HbA1CDone = !is.na(HbA1CDate),
        InfluenzaDone = !is.na(FluvaxDate),
        BPDone = !is.na(BPDate)
      ) %>>%
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
      self$qim_diabetes_report <- report
    }

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(report)
})
.reactive_event(
  dMeasureQIM, "qim_diabetes_reportR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_diabetes_listR(),
        self$qim_demographicGroupR(),
        self$qim_diabetes_measureR()
      ), {
        # or change in demographic grouping
        # or change in measures
        # or the diabetes list
        self$report_qim_diabetes(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)
