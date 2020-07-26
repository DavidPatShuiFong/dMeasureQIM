##### QIM 15+ ##################################################################

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureQIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @name qim_15plus
#' @title dMeasure Quality Improvement Measures - 15 plus screening
#'
#' @include QualityImprovementMeasures.R
NULL

##### QIM 15+ fields ###########################################################
.public(
  dMeasureQIM, "qim_15plus_list",
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
    WeightDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    WeightValue = numeric(),
    HeightDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    HeightValue = numeric(),
    BMIDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    BMIValue = numeric(),
    BMIClass = character(),
    WaistDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    WaistValue = numeric(),
    SmokingDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    SmokingStatus = character(),
    AlcoholDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    NonDrinker = character(),
    AlcoholDaysPerWeek = numeric(),
    AlcoholDrinksPerDay = numeric(),
    AlcoholDescription = character(),
    PastAlcoholLevel = character(),
    YearStarted = integer(),
    YearStopped = integer(),
    AlcoholComment = character(),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### QIM 15+ methods ##########################################################
#' List of diabetics, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 02 - Proportion of patients with a smoking status result
#' QIM 03 - Proportion of patients with a weight classification (12 months)
#'   * height must be taken with a client minimum age of 15 years
#'   * height must be done within the previous 12 months if 15-24 years (inclusive)
#'   * weight must be done within the previous 12 months
#'   * (to do) exclude client if 18 or more years, and <0.914 or >2.108 metres height
#' QIM 07 - Proportion of patients with an alcohol consumption status
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
#'
#' @return dataframe of Patient (name), InternalID, measures
#' @export
list_qim_15plus <- function(dMeasureQIM_obj,
                            contact = NA,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            min_date = NA,
                            max_date = NA,
                            contact_type = NA,
                            ignoreOld = NA,
                            lazy = FALSE) {
  dMeasureQIM_obj$list_qim_15plus(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy
  )
}

#' add BMI and BMIclass to datatable
#'
#' calculation of BMIClass is Age and Sex specific
#'
#' see PIP-QI-Technical-Specifications v1.1 (Australian Government)
#' https://www1.health.gov.au/internet/main/publishing.nsf/
#'   Content/46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-Technical-Specifications.pdf
#'
#' @param df dataframe
#'   needs DOB, Sex
#'     HeightDate, HeightValue, WeightValue
#' @param dM dMeasure object
#' @param reference_date date to calculate age
#'
#' @return dataframe with added
#'     BMIValue, BMIDate, BMIClass
#' @export
add_BMI <- function(df, dM, reference_date) {

  df <- df %>>%
    dplyr::mutate(
      Age15 = dplyr::if_else(
        !is.na(DOB) > 0,
        dMeasure::add_age(DOB, 15),
        as.Date(NA)
      ),
      Age25 = dplyr::if_else(
        !is.na(DOB) > 0,
        dMeasure::add_age(DOB, 25),
        as.Date(NA)
      ),
      AgeYears = dMeasure::calc_age(DOB, reference_date)) %>>%
    dplyr::mutate(
      # deal with 'old' or invalid height measurements
      # ('old' weight observations have been dealt with
      #   previously)
      # only height values if done after age of 15
      HeightValue = dplyr::if_else(
        HeightDate < Age15,
        as.numeric(NA),
        as.numeric(HeightValue)
      ),
      HeightDate = dplyr::if_else(
        HeightDate < Age15,
        as.Date(NA),
        as.Date(HeightDate)
      )
    ) %>>%
    dplyr::mutate(
      # if age of height less than age 25, then height needs to
      # be taken with the previous year and the patient
      # age must be less than 25 at end of 'survey' period
      HeightValue = dplyr::if_else(
        HeightDate < Age25 &
          (
            dMeasure::add_age(HeightDate, 1, by = "year") < reference_date |
              Age25 < reference_date
            # patient older than 25 at end of survey period
          ),
        as.numeric(NA),
        as.numeric(HeightValue)
      ),
      HeightDate = dplyr::if_else(
        HeightDate < Age25 &
          (
            dMeasure::add_age(HeightDate, 1, by = "year") < reference_date |
              Age25 < reference_date
            # patient is aged more than 25 at end of survey period
          ),
        as.Date(NA),
        as.Date(HeightDate)
      ),
    ) %>>%
    dplyr::mutate(
      BMIValue = dplyr::if_else(
        is.na(BMIValue) & !is.na(HeightValue) & !is.na(WeightValue),
        WeightValue / (HeightValue / 100)^2,
        BMIValue, as.double(NA)
      ),
      # calculate 'missing' BMI values, if valid height and weight values
      # are available
      BMIDate = dplyr::if_else(
        is.na(BMIDate) & !is.na(HeightDate) & !is.na(WeightDate),
        pmax(HeightDate, WeightDate), # max() is not vectorized
        BMIDate, as.Date(NA)
      ),
      # take the date of the BMI value as the maximum (latest)
      # of the relevant measurements
      BMIClass = dplyr::case_when(
        is.na(BMIValue) ~ as.character(NA),
        Sex == "Male" ~ dplyr::case_when(
          AgeYears == 15 ~ dplyr::case_when(
            BMIValue >= 28.60 ~ "Obese",
            BMIValue >= 23.60 ~ "Overweight",
            BMIValue >= 17.26 ~ "Healthy",
            TRUE ~ "Underweight"
          ),
          AgeYears == 16 ~ dplyr::case_when(
            BMIValue >= 29.14 ~ "Obese",
            BMIValue >= 24.19 ~ "Overweight",
            BMIValue >= 17.80 ~ "Healthy",
            TRUE ~ "Underweight"
          ),
          AgeYears == 17 ~ dplyr::case_when(
            BMIValue >= 29.41 ~ "Obese",
            BMIValue >= 24.73 ~ "Overweight",
            BMIValue >= 18.05 ~ "Healthy",
            TRUE ~ "Underweight"
          ),
          TRUE ~ dplyr::case_when(
            # age 18 and over
            BMIValue >= 30 ~ "Obese",
            BMIValue >= 25 ~ "Overweight",
            BMIValue >= 18.5 ~ "Healthy",
            TRUE ~ "Underweight"
          )
        ),
        Sex == "Female" ~ dplyr::case_when(
          AgeYears == 15 ~ dplyr::case_when(
            BMIValue >= 29.29 ~ "Obese",
            BMIValue >= 24.17 ~ "Overweight",
            BMIValue >= 17.69 ~ "Healthy",
            TRUE ~ "Underweight"
          ),
          AgeYears == 16 ~ dplyr::case_when(
            BMIValue >= 29.56 ~ "Obese",
            BMIValue >= 24.54 ~ "Overweight",
            BMIValue >= 17.91 ~ "Healthy",
            TRUE ~ "Underweight"
          ),
          AgeYears == 17 ~ dplyr::case_when(
            BMIValue >= 29.84 ~ "Obese",
            BMIValue >= 24.85 ~ "Overweight",
            BMIValue >= 18.38 ~ "Healthy",
            TRUE ~ "Underweight"
          ),
          TRUE ~ dplyr::case_when(
            # age 18 years and over
            # same as 'Male'
            BMIValue >= 30 ~ "Obese",
            BMIValue >= 25 ~ "Overweight",
            BMIValue >= 18.5 ~ "Healthy",
            TRUE ~ "Underweight"
          )
        )
      )
    ) %>>%
    dplyr::select(-c(Age15, Age25, AgeYears))

  return(df)
}

.public(dMeasureQIM, "list_qim_15plus", function(contact = NA,
                                                 date_from = NA,
                                                 date_to = NA,
                                                 clinicians = NA,
                                                 min_contact = NA,
                                                 min_date = NA,
                                                 max_date = NA,
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

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "fifteenplus_qim",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      # from contact list
      if (!lazy) {
        self$dM$list_contact_15plus(
          date_from, date_to, clinicians,
          min_contact, min_date, max_date,
          contact_type,
          lazy
        )
      }
      fifteen_plus_list <- self$dM$contact_15plus_list %>>%
        dplyr::select(-c(Count, Latest)) # don't need these fields
      fifteen_plusID <- fifteen_plus_list %>>%
        dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty list, bad for SQL filter!
    } else {
      if (!lazy) {
        self$dM$filter_appointments()
      }
      fifteen_plusID <- c(self$dM$fifteenplus_list(), -1)
      fifteen_plus_list <- self$dM$db$patients %>>%
        dplyr::filter(InternalID %in% fifteen_plusID) %>>%
        dplyr::select(Firstname, Surname, InternalID) %>>%
        dplyr::collect() %>>%
        dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
        dplyr::select(Patient, InternalID)
      # derived from self$appointments_filtered
    }
    smokingList <- self$dM$smoking_obs(
      fifteen_plusID,
      date_from = as.Date(
        ifelse(
          ignoreOld,
          seq.Date(date_to, length.out = 2, by = "-1 year")[[2]],
          -Inf
        ), origin = "1970-01-01"),
      # if ignoreOld
      # calculate date_from as one year before date_to
      date_to = date_to
    )

    measure_cols <- c(
      BMIDate = as.Date(NA),
      BMIValue = as.numeric(NA),
      WeightDate = as.Date(NA),
      WeightValue = as.numeric(NA),
      HeightDate = as.Date(NA),
      HeightValue = as.numeric(NA),
      WaistDate = as.Date(NA),
      WaistValue = as.numeric(NA)
    )
    # a list of columns which might (or might not) be auto-generated
    # from the observations table

    fifteen_plus_list <- fifteen_plus_list %>>%
      dMeasureQIM::add_demographics(self$dM, date_to) %>>%
      dplyr::left_join(
        self$dM$db$observations %>>%
          dplyr::filter(
            InternalID %in% fifteen_plusID,
            ObservationCode %in% c(9, 7, 8, 17),
            # 9 is 'BMI', 7 is 'Height',
            # 8 is 'Weight' and  17 is'Waist'
            # the string is in 'ObservationName'
            ObservationDate <= date_to
          ) %>>%
          dplyr::group_by(InternalID, ObservationCode) %>>%
          dplyr::filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>>%
          # the most recent observation by InternalID and ObservationCode
          dplyr::filter(ObservationTime == max(ObservationTime, na.rm = TRUE)) %>>%
          dplyr::ungroup() %>>%
          dplyr::collect() %>>%
          dplyr::mutate(ObservationDate = as.Date(ObservationDate)) %>>%
          # convert to R's 'standard' date format
          # didn't work before collect()
          {
            if (ignoreOld && nrow(.) > 0) {
              # if ignoring results that don't qualify for QIM
              dplyr::filter(., (dMeasure::calc_age(ObservationDate, date_to) == 0) |
                              (ObservationName == "Height"))
            }
            # throw out results which are more than twelve months old
            # except for height, which is valid if taken after age 15 years
            # (however, height measurements must be less than one year old if
            # person is less than 25 years old, and be taken at age of
            # twenty-five years if person is more than 25 years old. this is
            # dealt with later in this code)
            else {
              .
            }
          } %>>%
          dplyr::select(InternalID, ObservationName,
                        Date = ObservationDate, Value = ObservationValue
          ) %>>%
          dplyr::mutate(
            Value = as.double(Value),
            Date = as.double(Date)
          ) %>>%
          # converting the date to double avoids the warning during 'gather'
          #  "Warning message:
          #   attributes are not identical across measure variables;
          #   they will be dropped"
          # because of trying to put a Date and double into the same column
          tidyr::gather(variable, content, -(InternalID:ObservationName)) %>>%
          # this should result in InternalID, ObservationName, variable, content
          # where variable is one of 'Date' and 'Value'
          # and value is the values of Date or Value
          tidyr::unite(temp, ObservationName, variable, sep = "") %>>%
          # this should result in InternalID, temp and content
          # temp will be names like BMIDate and HeightValue
          tidyr::spread(temp, content) %>>% {
            tibble::add_column(., !!!measure_cols[!names(measure_cols) %in% names(.)])
          } %>>%
          # add missing columns, because not all possible variations may have been added
          dplyr::mutate(
            BMIDate = as.Date(BMIDate, origin = "1970-01-01"),
            HeightDate = as.Date(HeightDate, origin = "1970-01-01"),
            WeightDate = as.Date(WeightDate, origin = "1970-01-01"),
            WaistDate = as.Date(WaistDate, origin = "1970-01-01"),
            BMIValue = as.double(BMIValue),
            HeightValue = as.double(HeightValue),
            WeightValue = as.double(WeightValue),
            WaistValue = as.double(WaistValue)
          ),
        # this should result in InternalID, (... HeightDate, WaistValue etc.)
        by = "InternalID",
        copy = TRUE
      )

    fifteen_plus_list <- fifteen_plus_list %>>%
      dMeasureQIM::add_BMI(dM, date_to) %>>%
      dplyr::left_join(
        smokingList,
        by = "InternalID",
        copy = TRUE
      ) %>>%
      dplyr::left_join(
        self$dM$db$alcohol %>>%
          dplyr::filter(
            InternalID %in% fifteen_plusID,
            Updated <= date_to
          ) %>>%
          dplyr::rename(
            AlcoholDate = Updated,
            AlcoholDescription = Description,
            AlcoholComment = Comment,
            AlcoholDaysPerWeek = DaysPerweek,
            AlcoholDrinksPerDay = DrinksPerday
          ) %>>%
          dplyr::collect() %>>%
          dplyr::mutate(AlcoholDate = as.Date(AlcoholDate)) %>>%
          # convert to R's standard date format
          dplyr::mutate(AlcoholDate = dplyr::if_else(
            NonDrinker == "No" &
              AlcoholDaysPerWeek == 0 &
              AlcoholDrinksPerDay == 0,
            as.Date(-Inf, origin = "1970-01-01"),
            as.Date(AlcoholDate)
          )) %>>%
          # if not marked as a 'non-drinker', but no drinks recorded
          # then this is actually a 'blank' entry
          {
            if (ignoreOld && nrow(.) > 0) {
              # if ignoring observations that don't qualify for QIM
              dplyr::filter(., dMeasure::calc_age(AlcoholDate, date_to) < 1)
            }
            # throw out observations which are twelve months or older
            else {
              .
            }
          },
        by = "InternalID",
        copy = TRUE
      ) %>>%

      dplyr::select(
        Patient, InternalID, RecordNo, Sex, Ethnicity, Indigenous,
        MaritalStatus, Sexuality, Age10,
        HeightDate, HeightValue, WeightDate, WeightValue, BMIDate, BMIValue, BMIClass,
        WaistDate, WaistValue, SmokingDate, SmokingStatus,
        AlcoholDate, NonDrinker, AlcoholDaysPerWeek, AlcoholDrinksPerDay,
        AlcoholDescription, PastAlcoholLevel, YearStarted, YearStopped, AlcoholComment
      )

    self$qim_15plus_list <- fifteen_plus_list

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(self$qim_15plus_list)
})
.reactive_event(
  dMeasureQIM, "qim_15plus_listR",
  quote(
    shiny::eventReactive(
      c(
        self$dM$contact_15plus_listR(),
        self$dM$appointments_filteredR(),
        self$qim_contactR(),
        self$qim_ignoreOldR()
      ), {
        # update if reactive version of contact, appointment lists
        # or change in list type (contact VS appoiontment)
        # or change in choosing to ignore old measures
        self$list_qim_15plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)


.active(dMeasureQIM, "qim_15plus_measureTypes", function(value) {
  if (!missing(value)) {
    warning("$qim_15plus_measureTypes is read-only.")
  } else {
    return(c("Smoking", "Weight", "Alcohol"))
    # vector of valid QIM measures for 15 plus (for QIM reporting)
    # QIM 02 - Proportion of patients with a smoking status result
    # QIM 03 - Proportion of patients with a weight classification (12 months)
    # QIM 07 - Proportion of patients with an alcohol consumption status
  }
})

.private_init(dMeasureQIM, ".qim_15plus_measure", quote(self$qim_15plus_measureTypes))
.active(dMeasureQIM, "qim_15plus_measure", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_15plus_measure)
  }
  value <- intersect(value, self$qim_15plus_measureTypes)
  # only valid measure types allowed
  private$.qim_15plus_measure <- value
  private$set_reactive(self$qim_15plus_measureR, value)
})
.reactive(dMeasureQIM, "qim_15plus_measureR", quote(self$qim_15plus_measureTypes))


.public(
  dMeasureQIM, "qim_15plus_list_appointments",
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
    WeightDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    WeightValue = numeric(),
    HeightDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    HeightValue = numeric(),
    BMIDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    BMIValue = numeric(),
    BMIClass = character(),
    WaistDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    WaistValue = numeric(),
    SmokingDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    SmokingStatus = character(),
    AlcoholDate = as.Date(
      integer(0),
      origin = "1970-01-01"
    ),
    NonDrinker = character(),
    AlcoholDaysPerWeek = numeric(),
    AlcoholDrinksPerDay = numeric(),
    AlcoholDescription = character(),
    PastAlcoholLevel = character(),
    YearStarted = integer(),
    YearStopped = integer(),
    AlcoholComment = character(),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

#' List of diabetics, with Quality Improvement Measures, in the contact list, with appoinments
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 02 - Proportion of patients with a smoking status result
#' QIM 03 - Proportion of patients with a weight classification (12 months)
#' QIM 07 - Proportion of patients with an alcohol consumption status
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
#'
#' @return dataframe of Patient (name), InternalID, appointment details and measures
#' @export
list_qim_15plus_appointments <- function(dMeasureQIM_obj,
                                         contact = NA,
                                         date_from = NA,
                                         date_to = NA,
                                         clinicians = NA,
                                         min_contact = NA,
                                         min_date = NA,
                                         max_date = NA,
                                         contact_type = NA,
                                         ignoreOld = NA,
                                         lazy = FALSE) {
  dMeasureQIM_obj$list_qim_15plus_appointments(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    ignoreOld,
    lazy
  )
}

.public(dMeasureQIM, "list_qim_15plus_appointments", function(contact = NA,
                                                              date_from = NA,
                                                              date_to = NA,
                                                              clinicians = NA,
                                                              min_contact = NA,
                                                              min_date = NA,
                                                              max_date = NA,
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

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$dM$emr_db$is_open()) {
    # only if EMR database is open
    if (self$dM$Log) {
      log_id <- self$dM$config_db$write_log_db(
        query = "fifteenplus_qim_appointments",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_qim_15plus(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date,
        contact_type, ignoreOld,
        lazy
      )
      self$dM$filter_appointments_time(
        date_from, date_to, clinicians,
        lazy = lazy
      )
    }

    self$qim_15plus_list_appointments <- self$qim_15plus_list %>>%
      dplyr::left_join(
        self$dM$appointments_filtered_time,
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

  return(self$qim_15plus_list_appointments)
})
.reactive_event(
  dMeasureQIM, "qim_15plus_list_appointmentsR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_15plus_listR(),
        self$dM$appointments_filtered_timeR()
      ), {
        # update if change in 15plus_list or
        # appointments_filtered_time list
        self$list_qim_15plus_appointments(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)

.public(
  dMeasureQIM, "qim_15plus_report",
  data.frame(NULL,
             stringsAsFactors = FALSE
  )
)
# empty data frame, number of columns dynamically change

#' Age 15 plus Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 02 - Proportion of patients with a smoking status result
#' QIM 03 - Proportion of patients with a weight classification (12 months)
#' QIM 07 - Proportionof patients with an alcohol consumption status
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
#'  if not supplied, reads $qim_15plus_measure
#'  list of available measures in $qim_15plus_measureTypes
#'  currently 'Smoking', 'Weight', 'Alcohol'
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#' @param store keep result in
#'
#' @return dataframe of Patient (name), demographics, measures (done or not),
#'  Count (n), Proportion, Proportion_Demographic
#' @export
report_qim_15plus <- function(dMeasureQIM_obj,
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
                              ignoreOld = NA,
                              lazy = FALSE,
                              store = TRUE) {
  dMeasureQIM_obj$report_qim_15plus(
    contact, date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    demographic, measure,
    ignoreOld, lazy, store
  )
}
.public(dMeasureQIM, "report_qim_15plus", function(contact = NA,
                                                   date_from = NA,
                                                   date_to = NA,
                                                   clinicians = NA,
                                                   min_contact = NA,
                                                   min_date = NA,
                                                   max_date = NA,
                                                   contact_type = NA,
                                                   demographic = NA,
                                                   measure = NA,
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
  if (is.na(measure)) {
    measure <- self$qim_15plus_measure
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
        query = "qim_15plus_report",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_qim_15plus(
        contact, date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        ignoreOld, lazy
      )
    }

    measure <- dplyr::recode(
      measure,
      "Smoking" = "SmokingStatus",
      "Weight" = "BMIClass",
      "Alcohol" = "AlcoholDone"
    )

    report_groups <- c(demographic, measure)
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    report <- self$qim_15plus_list %>>%
      dplyr::mutate(
        AlcoholDone = !(is.na(AlcoholDate) | AlcoholDate == -Inf)
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

    if (store) {
      self$qim_15plus_report <- report
    }

    # proportion (an alternative would be proportion = n / sum(n))

    if (self$dM$Log) {
      self$dM$config_db$duration_log_db(log_id)
    }
  }

  return(report)
})
.reactive_event(
  dMeasureQIM, "qim_15plus_reportR",
  quote(
    shiny::eventReactive(
      c(
        self$qim_15plus_listR(),
        self$qim_demographicGroupR(),
        self$qim_15plus_measureR()
      ), {
        # or change in demographic grouping
        # or change in measures
        # or change in 15plus list
        self$report_qim_15plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)
