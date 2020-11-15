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
  include_all_demographic_groups = FALSE
) {
  if (is.environment(progress)) { # a OOP object (as opposed to 'NA')
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
    qim_report <- dMeasureQIM::fill_demographics(qim_report)
  }

  return(qim_report)
}


#' writeReportJSON
#'
#' @md
#'
#' @description write reports to PIP QI Eligible Data SEt JSON Specification 1.1
#'
#' @param d data
#' @param date_to chosen end date
#' @param author_id identifier from source software of the authenticated user who
#'   initiated the extract
#' @param practice_id an identifier for the practice
#' @param product the name of the software that produced the file
#' @param report Must be "PIP QI"
#' @param version version number of the specification used to generate the JSON
#' @param small_cell_suppression 'suppress' (return zero, or remove
#'   row entirely) if count is less than 5. helps de-identifiability
#' @param indigenous_aggregate simplify indigenous groups to
#'   INDIGENOUS, NON-INDIGENOUS, NOT STATED
#' @param sex_aggregate simplify sex groups to
#'   MALE, FEMALE, INDETERMINATE/INTERSEX/UNSPECIFIED/NOT STATED/INADEQUATELY DESCRIBED
#' @export
writeReportJSON <- function(
  d, date_to,
  author_id = "bpsrawdata",
  practice_id,
  product = "GPstat",
  report = "PIP QI",
  version = "1.1",
  small_cell_suppression = TRUE,
  indigenous_aggregate = TRUE, sex_aggregate = TRUE) {

  options(digits.secs = 3) # print second component to 3 decimal places
  x <- paste(date_to, "22:59:59.999") # one hour before midnight
  attr(x, "tzone") <- "UTC" # convert from local timezone to UTC
  extraction_datetime <- as.character(x)

  d <- d %>>%
    dplyr::filter(DateTo == date_to) %>>% # just keep the required date information
    dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType, State, n)

  if (indigenous_aggregate) {
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
      ) # another possible option is 'Not stated'
  }

  if (sex_aggregate) {
    d <- d %>>%
      dplyr::mutate(
        Sex = dplyr::if_else(
          Sex %in% c("Not stated", "X"), # these two are combined
          "INDETERMINATE/INTERSEX/UNSPECIFIED/NOT STATED/INADEQUATELY DESCRIBED",
          Sex
        )
      )
  }

  if (small_cell_suppression) {
    # 'suppress' small groups to zero. helps with de-identification
    d <- d %>>% dplyr::mutate(
      n = dplyr::if_else(
        n < 5,
        0L, # force to integer
        n
      )
    )
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
    dplyr::rename(sex = Sex, age_group = Age10, indigenous_status = Indigenous,
                  numerator = n) %>>%
    dplyr::mutate(sex = toupper(sex), indigenous_status = toupper(indigenous_status))

  return(jsonlite::toJSON(d, pretty = TRUE))

}
