#' @name qim_report
#' @title dMeasure Quality Improvement Measures - reports for QIM
#'
#' @include QualityImprovementMeasures.R
NULL

#' getReport
#'
#' @md
#'
#' @description create reports in a 'standard' format
#'   from the 10 QIM measures
#'
#' gets report from 'report_function'
#' then adds extra 'DiabetesType' column if necessary
#' adds more columns describing the measurement
#' renames the 'result' column to 'State'
#'
#' small cell suppression supported
#'   section 2.4 'Small cell suppression' of
#'   "Practice Incentives Program Quality Improvement Measures User Guide"
#'   https://www1.health.gov.au/internet/main/publishing.nsf/Content/
#'   46506AF50A4824B6CA25848600113FFF/$File/PIP%20QI%20-%20User%20Guide.pdf
#'
#' @param report_function the dMQIM method to call
#' @param date_from start date
#' @param date_to end date
#' @param contact_type types of contacts e.g. Services,
#' @param min_contact minimum number of contacts to be 'valid'
#' @param min_date most recent contact must be at least min_date
#' @param max_date most recent contact at most max_date
#' @param progress object for displaying progress. Leave as `NA` if
#'   not required
#' @param progress_detail message to report in the 'progress' window
#' @param measure some (but not all) `report_function` can
#'   choose the 'measure' if not required, then use `NA`
#' @param require_type_diabetes diabetes methods may give option
#'   to specify diabetes types
#' @param qim_name the name of the QIM e.g. 'QIM 01'
#' @param measure_name the name of the measure e.g. 'HbA1C'
#' @param state_variable_name the name of the column in the return
#'   from `report_function` which becomes the 'State' column
#' @param small_cell_suppression 'suppress' (return zero, or remove
#'   row entirely) if count is less than 5.
#' @param include_all_demographic_groups include all possible
#'   demographic groups/subgroups, even if there was no count
#'
#' @export
getReport <- function(
  report_function,
  date_from, date_to,
  contact_type, min_contact,
  min_date = as.Date("2000-01-01"),
  max_date = Sys.Date(),
  # no effective limitations on min/max date by default
  progress = NA,
  progress_detail,
  measure = NA,
  require_type_diabetes,
  qim_name, measure_name,
  state_variable_name,
  small_cell_suppression = FALSE,
  include_all_demographic_groups = TRUE
) {
  if (is.environment(progress)) { # a OOP object (as opposed to 'NA')
    progress$inc(amount = 1, detail = progress_detail)
  }

  function_args <- list(
    contact = TRUE, date_from = date_from, date_to = date_to,
    min_contact = min_contact,
    # 'default' is clinician list chosen in right panel
    min_date = min_date, max_date = max_date,
    contact_type = contact_type,
    demographic = c("Age10", "Sex", "Indigenous"),
    ignoreOld = TRUE, lazy = FALSE, store = FALSE
  )
  if (!is.na(measure)) {
    function_args <- append(
      function_args,
      list(measure = measure)
    )
  }
  if (require_type_diabetes) {
    function_args <- append(
      function_args,
      list(type_diabetes = TRUE)
    )
  }
  qim_report <- do.call(report_function, function_args) %>>%
    dplyr::mutate(
      QIM = qim_name, Measure = measure_name
    ) %>>% {
      if (require_type_diabetes) {
        . # 'DiabetesType' should already exit
      } else {
        dplyr::mutate(., DiabetesType = "")
        # create a 'blank'
      }
    } %>>%
    dplyr::rename(
      State = !!state_variable_name,
      ProportionDemographic = Proportion_Demographic
    ) %>>% {
      if (small_cell_suppression) {
        dplyr::filter(., n >= 5)
        # 'suppress' the cell (removed completely!) if < 5
      } else {
        .
      }
    } %>>%
    dplyr::select(
      QIM, Age10, Sex, Indigenous, DiabetesType,
      Measure, State, n, ProportionDemographic
    )
  # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
  # n, Proportion_Demographic

  if (include_all_demographic_groups) {
    qim_report <- dMeasureQIM::fill_demographics(qim_report)
  }

  return(qim_report)
}


#' writeReportJSON
#'
#' @md
#'
#' @description write reports to PIP QI Eligible Data Set JSON Specification 1.1
#'
#' @param d data
#' @param date_to chosen end date
#' @param author_id identifier from source software of the authenticated user who
#'   initiated the extract
#' @param practice_id an identifier for the practice
#' @param product the name of the software that produced the file
#' @param report Must be "PIP QI"
#' @param version version number of the specification used to generate the JSON
#' @param small_cell_suppression 'suppress' (return NA, or remove
#'   row entirely) if count is denominator less than 5
#'
#'   This measure is explictly allowed under '[Practice Incentives Program Quality Improvement Incentives Quality Improvement Measures User Guide for General Practices (2020)](https://www1.health.gov.au/internet/main/publishing.nsf/Content/46506AF50A4824B6CA25848600113FFF/$File/PIP-QI-User-Guide-Practices.pdf)',
#'   Section 2.7 'Is the data de-identified?', page 8.
#'
#' @param group_identification_suppression numerator 2 or less
#'   OR difference between numerator and denominator is less than or equal to 2.
#'   This is a simple probability-based disclosure suppression
#'   (l-diversity) against homogeneity attack e.g. ALL or NONE of the
#'   patients in a sub-group have a specified state.
#'
#'   This measure is not explicitly mentioned under the PIP QIM User Guide, but
#'   is consistent with avoiding disclosure of information about individual
#'   patients.
#' @param indigenous_aggregate simplify indigenous groups to
#'   INDIGENOUS, NON-INDIGENOUS, NOT STATED
#' @param sex_aggregate simplify sex groups to
#'   MALE, FEMALE, INDETERMINATE/INTERSEX/UNSPECIFIED/NOT STATED/INADEQUATELY DESCRIBED
#'
#' @return JSON string
#'
#' @export
writeReportJSON <- function(
  d, date_to,
  author_id = "bpsrawdata",
  practice_id,
  product = "GPstat",
  report = "PIP QI",
  version = "1.2",
  small_cell_suppression = TRUE,
  group_identification_suppression = FALSE,
  indigenous_aggregate = TRUE, sex_aggregate = TRUE) {

  options(digits.secs = 3) # print second component to 3 decimal places
  x <- paste(date_to, "22:59:59.999") # one hour before midnight
  attr(x, "tzone") <- "UTC" # convert from local timezone to UTC
  extraction_datetime <- as.character(x)

  d <- d %>>%
    dplyr::filter(DateTo == date_to) %>>% # just keep the required date information
    dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType, State, n)

  if (indigenous_aggregate) {
    # INDIGENOUS, NON-INDIGENSOUS or 'Not stated'
    d <- d %>>%
      dplyr::mutate(
        Indigenous = dplyr::if_else(
          Indigenous %in% c("Aboriginal", "Both Aboriginal and Torres Strait Islander", "Torres Strait Islander"),
          "INDIGENOUS", # all indigenous groups combined to 'INDIGENOUS'
          Indigenous
        )
      ) %>>%
      dplyr::mutate(
        Indigenous = dplyr::if_else(
          Indigenous == "Neither",
          "NON-INDIGENOUS", # re-code 'not indigenous'
          Indigenous
        )
      ) %>>%
      dplyr::group_by(QIM, Age10, Sex, Indigenous, DiabetesType, State) %>>%
      dplyr::summarize(n = sum(n)) %>>% # aggregate indigenous classifications
      dplyr::ungroup()
  }

  if (sex_aggregate) {
    d <- d %>>%
      dplyr::mutate(
        Sex = dplyr::if_else(
          Sex %in% c("Not stated", "X"), # these two are combined
          "INDETERMINATE/INTERSEX/UNSPECIFIED/NOT STATED/INADEQUATELY DESCRIBED",
          Sex
        )
      ) %>>%
      dplyr::group_by(QIM, Age10, Sex, Indigenous, DiabetesType, State) %>>%
      dplyr::summarize(n = sum(n)) %>>% # aggregate sex classifications
      dplyr::ungroup()
  }

  if (small_cell_suppression | group_identification_suppression) {
    # 'suppress' small groups to zero. helps with de-identification
    suppress_small_cells <- function(d) {
      return(
        d %>>%
          dplyr::mutate(
            numerator = dplyr::if_else(
              denominator != 0 & denominator < 5 & small_cell_suppression,
              # potentially identifying information if group size < 5
              # avoid 'suppression' if the denominator is zero!
              as.integer(NA),
              as.integer(numerator)
            )
          ) %>>%
          dplyr::mutate(
            numerator = dplyr::if_else(
              denominator != 0 & (numerator <= 2 | (denominator - numerator) <= 2) & group_identification_suppression,
              # numerator is 2 or less, or very close to the denominator
              # this is a type of probability-based disclosure suppression
              as.integer(NA),
              as.integer(numerator)
            )
          )
      )
    }
  } else {
    suppress_small_cells <- function(d) {
      # no suppression, just return the current dataframe
      return(d)
    }
  }

  d_denominators <- d %>>%
    dplyr::group_by(QIM, Age10, Sex, Indigenous, DiabetesType) %>>%
    dplyr::summarise(denominator = sum(n)) %>>% # the total in each sub-group
    dplyr::ungroup()

  d <- d %>>%
    dplyr::left_join(
      d_denominators,
      by = c("QIM", "Age10", "Sex", "Indigenous", "DiabetesType")
    ) %>>% # add denominators to each group
    dplyr::filter(State != "FALSE") %>>% # these are excluded from the report!
    dplyr::rename(sex = Sex, age_group = Age10,
                  indigenous_status = Indigenous, diabetes_type = DiabetesType,
                  numerator = n) %>>%
    dplyr::mutate(sex = toupper(sex),
                  indigenous_status = toupper(indigenous_status))

  qim_df <- list(
    extraction_datetime = jsonlite::unbox(extraction_datetime),
    author_id = jsonlite::unbox(author_id),
    practice_id = jsonlite::unbox(practice_id),
    product = jsonlite::unbox(product),
    report = jsonlite::unbox(report),
    version = jsonlite::unbox(version)
  )
  # all these single elements are 'unboxed'
  # an alternative is to define as dataframe
  #  in which case all the 'qim' names below will need to be
  #  defined as lists instead of dataframes
  #  (appending '%>>% list()' is enough)

  qim_measures <- unique(d %>>% dplyr::pull(QIM))
  # the measures included in the report

  if ("QIM 01" %in% qim_measures) {
    qim_df[["qim01-diabetes"]] <- d %>>%
      dplyr::filter(QIM == "QIM 01") %>>%
      dplyr::mutate(hba1c_result = "RECORDED") %>>%
      dplyr::select(sex, age_group, indigenous_status, diabetes_type,
                    hba1c_result, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 02" %in% qim_measures) {
    qim_df[["qim02a-smoking"]] <- d %>>%
      dplyr::filter(QIM == "QIM 02") %>>%
      dplyr::mutate(smoking_status = "RECORDED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    smoking_status, State, numerator, denominator) %>>%
      dplyr::mutate(
        State = dplyr::if_else(
          State %in% c("Ex smoker", "Non smoker", "Smoker"), # these three are combined
          TRUE,
          NA
        )
      ) %>>%
      dplyr::filter(!is.na(State)) %>>%
      dplyr::group_by(sex, age_group, indigenous_status,
                      smoking_status, State, denominator) %>>%
      dplyr::summarize(numerator = sum(numerator)) %>>%
      dplyr::ungroup() %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    smoking_status, numerator, denominator) %>>%
      suppress_small_cells()

    d_subset <- d %>>%
      dplyr::filter(QIM == "QIM 02") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    smoking_status = State, numerator, denominator) %>>%
      dplyr::filter(smoking_status %in% c("Ex smoker", "Non smoker", "Smoker")) %>>%
      dplyr::mutate(
        smoking_status = dplyr::case_when(
          smoking_status == "Ex smoker" ~ "EX",
          smoking_status == "Non smoker" ~ "NEVER",
          smoking_status == "Smoker" ~ "CURRENT"
        )
      )

    qim_df[["qim02b-smoking"]] <- d_subset %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    smoking_status, numerator) %>>%
      # re-calculate the denominator (does not include the NA 'not recorded')
      dplyr::left_join(
        d_subset %>>%
          dplyr::group_by(sex, age_group, indigenous_status) %>>%
          dplyr::summarize(denominator = sum(numerator)) %>>%
          dplyr::ungroup() %>>%
          dplyr::select(sex, age_group, indigenous_status, denominator),
        by = c("sex", "age_group", "indigenous_status")
      ) %>>%
      suppress_small_cells()
  }

  if ("QIM 03" %in% qim_measures) {
    qim_df[["qim03a-bmi"]] <- d %>>%
      dplyr::filter(QIM == "QIM 03") %>>%
      dplyr::mutate(bmi = "RECORDED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    bmi, State, numerator, denominator) %>>%
      dplyr::mutate(
        State = dplyr::if_else(
          State %in% c("Healthy", "Obese", "Overweight", "Underweight"), # these three are combined
          TRUE,
          NA
        )
      ) %>>%
      dplyr::filter(!is.na(State)) %>>%
      dplyr::group_by(sex, age_group, indigenous_status,
                      bmi, State, denominator) %>>%
      dplyr::summarize(numerator = sum(numerator)) %>>%
      dplyr::ungroup() %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    bmi, numerator, denominator) %>>%
      suppress_small_cells()

    d_subset <- d %>>%
      dplyr::filter(QIM == "QIM 03") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    bmi = State, numerator, denominator) %>>%
      dplyr::filter(bmi %in% c("Healthy", "Obese", "Overweight", "Underweight")) %>>%
      dplyr::mutate(bmi = toupper(bmi))

    qim_df[["qim03b-bmi"]] <- d_subset %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    bmi, numerator) %>>%
      # re-calculate the denominator (does not include the NA 'not recorded')
      dplyr::left_join(
        d_subset %>>%
          dplyr::group_by(sex, age_group, indigenous_status) %>>%
          dplyr::summarize(denominator = sum(numerator)) %>>%
          dplyr::ungroup() %>>%
          dplyr::select(sex, age_group, indigenous_status, denominator),
        by = c("sex", "age_group", "indigenous_status")
      ) %>>%
      suppress_small_cells()
  }

  if ("QIM 04" %in% qim_measures) {
    qim_df[["qim04-influenza_65years"]] <- d %>>%
      dplyr::filter(QIM == "QIM 04") %>>%
      dplyr::mutate(influenza_immun_status = "IMMUNISED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    influenza_immun_status, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 05" %in% qim_measures) {

    d_subset <- d %>>%
      dplyr::filter(QIM == "QIM 05") %>>%
      dplyr::mutate(influenza_immun_status = "IMMUNISED") %>>%
      dplyr::select(sex, age_group, indigenous_status, diabetes_type,
                    influenza_immun_status, numerator, denominator)

    qim_df[["qim05-influenza_diabetes"]] <- d_subset %>>%
      dplyr::group_by(sex, age_group, indigenous_status,
                      influenza_immun_status) %>>%
      # this QIM does not actually have diabetes sub-groups
      dplyr::summarize(numerator = sum(numerator)) %>>%
      dplyr::ungroup() %>>%
      dplyr::left_join(
        d_subset %>>%
          dplyr::group_by(sex, age_group, indigenous_status) %>>%
          dplyr::summarize(denominator = sum(denominator)) %>>%
          # denominator includes those who are not immunized
          dplyr::ungroup(),
        by = c("sex", "age_group", "indigenous_status")
      ) %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    influenza_immun_status, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 06" %in% qim_measures) {
    qim_df[["qim06-influenza_copd"]] <- d %>>%
      dplyr::filter(QIM == "QIM 06") %>>%
      dplyr::mutate(influenza_immun_status = "IMMUNISED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    influenza_immun_status, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 07" %in% qim_measures) {
    qim_df[["qim07-alcohol"]] <- d %>>%
      dplyr::filter(QIM == "QIM 07") %>>%
      dplyr::mutate(alcohol_status = "RECORDED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    alcohol_status, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 08" %in% qim_measures) {
    qim_df[["qim08-cvd"]] <- d %>>%
      dplyr::filter(QIM == "QIM 08") %>>%
      dplyr::mutate(cvd_risk = "RECORDED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    cvd_risk, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 09" %in% qim_measures) {
    qim_df[["qim09-cervical"]] <- d %>>%
      dplyr::filter(QIM == "QIM 09") %>>%
      dplyr::mutate(cervical_screen_status = "SCREENED") %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    cervical_screen_status, numerator, denominator) %>>%
      suppress_small_cells()
  }

  if ("QIM 10" %in% qim_measures) {

    d_subset <- d %>>%
      dplyr::filter(QIM == "QIM 10") %>>%
      dplyr::mutate(bp_result = "RECORDED") %>>%
      dplyr::select(sex, age_group, indigenous_status, diabetes_type,
                    bp_result, numerator, denominator)

    qim_df[["qim10-diabetes_bp"]] <- d_subset %>>%
      dplyr::group_by(sex, age_group, indigenous_status,
                      bp_result) %>>%
      # this QIM does not actually have diabetes sub-groups
      dplyr::summarize(numerator = sum(numerator)) %>>%
      dplyr::ungroup() %>>%
      dplyr::left_join(
        d_subset %>>%
          dplyr::group_by(sex, age_group, indigenous_status) %>>%
          dplyr::summarize(denominator = sum(denominator)) %>>%
          # denominator includes those who are not immunized
          dplyr::ungroup(),
        by = c("sex", "age_group", "indigenous_status")
      ) %>>%
      dplyr::select(sex, age_group, indigenous_status,
                    bp_result, numerator, denominator) %>>%
      suppress_small_cells()
  }

  return(jsonlite::toJSON(qim_df, na = "string", pretty = TRUE))
  # 'NA' values have been small cell suppressed
}
