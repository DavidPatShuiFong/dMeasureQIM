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
#' @param age_min minimum age for 'all demographic' option
#' @param age_max manimum age for 'all demographic' option
#' @param states states for 'all demographic' option
#' @export
getReport <- function(
  report_function,
  date_from, date_to,
  contact_type, min_contact,
  progress = NA,
  progress_detail,
  measure = NA,
  require_type_diabetes,
  qim_name, measure_name,
  state_variable_name,
  small_cell_suppression = FALSE,
  include_all_demographic_groups = FALSE,
  age_min = 0, age_max = 65,
  states = c(FALSE, TRUE)
) {
  if (!is.na(progress)) {
    progress$inc(amount = 1, detail = progress_detail)
  }

  function_args <- list(
    contact = TRUE, date_from = date_from, date_to = date_to,
    min_contact = min_contact,
    # 'default' is clinician list chosen in right panel
    min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
    # no limitations on min/max date essentially
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
    qim_report <- dMeasureQIM::complete_demographics(
      qim_report,
      qim_name = qim_name,
      age_min = age_min, age_max = age_max,
      include_diabetes = require_type_diabetes,
      measure = measure_name,
      states = states
    )
  }

  return(qim_report)
}
