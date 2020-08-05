# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @name qim_reportChart
#' @title dMeasure Quality Improvement Measures - charts for QIM UI
#'
#' need definitions including 'measure_names'
#'
#' @include QualityImprovementMeasures.R
NULL

data_categories <- c(
  "State", "Age", "Sex", "Indigenous",
  "Diabetes"
)
stack_categories <- data_categories[data_categories != "Age"]

#' @export
qim_reportCharter_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinydashboard::tabBox(
          id = ns("charter_leftpanel"),
          width = 12,
          shiny::tabPanel(
            title = "Filters",
            shiny::wellPanel(
              style = "height:35em",
              shinyWidgets::pickerInput(
                inputId = ns("qim_chosen"),
                label = "QIM",
                choices = measure_names,
                selected = measure_names[1]
              ),
              shinyWidgets::sliderTextInput(
                inputId = ns("age_range"),
                label = "Age range",
                choices = c(0, 5, 15, 25, 35, 45, 55, 65),
                selected = c(0, 65),
                # an age 'range'
              ),
              shinyWidgets::pickerInput(
                inputId = ns("sex_chosen"),
                label = "Sex",
                choices = sex_choices,
                selected = sex_choices,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("ethnicity_chosen"),
                label = "Ethnicity",
                choices = ethnicity_choices,
                selected = ethnicity_choices,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("diabetes_chosen"),
                label = "Diabetes",
                choices = diabetes_choices,
                selected = diabetes_choices,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              )
            )
          ),
          shiny::tabPanel(
            title = "Charting",
            shiny::wellPanel(
              height = "35em",
              shinyWidgets::pickerInput(
                inputId = ns("series_chosen"),
                label = "Series",
                choices = data_categories,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("category_chosen"),
                label = "Category",
                choices = c("None"),
                # initially no choices, but will be expanded
                # when input$series_chosen has selections
                selected = "None",
                multiple = FALSE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("stack_chosen"),
                label = "Stack",
                choices = "None",
                # initially no choices, but will be expanded
                # when input$series_chosen has selections
                selected = "None",
                multiple = FALSE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                # the category group of stream show on the negative (left)
                # side of a pyramid
                # e.g. if 'Female' is to be shown on the left,
                # then 'Sex' is the category
                inputId = ns("mirror_chosen"),
                label = "Mirror",
                choices = "None",
                selected = "None",
                multiple = FALSE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::pickerInput(
                inputId = ns("mirror_group"),
                label = "Mirror group",
                choices = NULL,
                # initially no choice
                # but if a category group e.g. 'Sex' is chosen
                # in 'mirror_chosen', then this is filled with
                # choices to be shown on the negative (left) side
                # of the pyramid e.g. 'Male/Female/X/Not available'
                selected = NULL,
                multiple = TRUE,
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                )
              ),
              shinyWidgets::switchInput(
                inputId = ns("proportion"),
                label = "Proportion",
                value = FALSE,
                labelWidth = "10 em"
              ),
              shiny::hr(),
              shiny::actionButton(
                inputId = ns("show_grouped_values"),
                label = "Show grouped values"
              )
            )
          ),
          shiny::tabPanel(
            title = "Restore/Load",
            shiny::wellPanel(
              style = "height:23em",
              shiny::tags$h5("Load report"),
              shiny::fileInput(
                ns("loadCSVFile"),
                "Choose GPstat! QIMReport .CSV file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values, text/plain",
                  ".csv"
                )
              ),
              shiny::br(),
              shiny::hr(),
              shiny::actionButton(
                inputId = ns("show_report_values"),
                label = "Show report"
              )
            )
          )
        )
      ),
      shiny::column(
        width = 9
      )
    )
  )
}

#' Quality Improvement report charter - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'   access to appointments lists, results, correspondence and EMR database
#' @param report a list returned by qim_reportCreator
#'   should contain $report_values(), which is a dataframe
#'
#' @return none
#'
#' @export
qim_reportCharter <- function(input, output, session, dMQIM, report) {
  ns <- session$ns

  report_values <- shiny::reactiveVal()
  # where .CSV files are first stored,
  # or dataframes transferred from qim_reportCreator module
  # via 'report'
  report_filled <- shiny::reactiveVal()
  # report_values filtered, and then 'filled' with
  # 'missing' demographic rows (where 'n' = 0)
  report_grouped <- shiny::reactiveVal()
  # report_filled grouped, according to chart options

  ##### data series choices #################################

  # modify choices for input$category_chosen
  shiny::observeEvent(
    c(input$series_chosen, input$stack_chosen, input$mirror_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'stack' or 'mirror' (except 'None'!)
      choices <- choices[choices != input$stack_chosen]
      choices <- choices[choices != input$mirror_chosen]
      choices <- c("None", choices)
      # retain previous choice
      chosen <- input$category_chosen

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "category_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )

  # modify choices for input$stack_chosen
  shiny::observeEvent(
    c(input$series_chosen, input$category_chosen, input$mirror_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'category' or 'mirror' (except 'None'!)
      choices <- choices[choices != input$category_chosen]
      choices <- choices[choices != input$mirror_chosen]
      # cannot be 'age'
      choices <- choices[choices != "Age"]
      # can still be 'None'
      choices <- c("None", choices)
      chosen <- input$stack_chosen

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "stack_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )

  shiny::observeEvent(
    c(input$series_chosen, input$category_chosen, input$stack_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      # MUST be one of input$series_chosen
      choices <- input$series_chosen
      # cannot be chosen in 'category' or stack' (except 'None'!)
      choices <- choices[choices != input$category_chosen]
      choices <- choices[choices != input$stack_chosen]
      # cannot be 'age'
      choices <- choices[choices != "Age"]
      # can still be 'None'
      choices <- c("None", choices)
      chosen <- input$mirror_chosen # previous choice

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "mirror_chosen",
        choices = choices,
        selected = chosen
      )
    }
  )

  shiny::observeEvent(
    c(input$mirror_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      if (is.null(input$mirror_chosen) || input$mirror_chosen == "None") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_chosen",
          choices = NULL,
          selected = NULL
        )
      } else if (input$mirror_chosen == "Sex") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_chosen",
          choices = sex_choices,
          selected = NULL
        )
      } else if (input$mirror_chosen == "Indigenous") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_chosen",
          choices = ethnicity_choices,
          selected = NULL
        )
      } else if (input$mirror_chosen == "Diabetes") {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_chosen",
          choices = diabetes_choices,
          selected = NULL
        )
      } else if (input$mirror_chosen == "State") {
        # complex, this depends on the chosen QIM
        if (input$qim_chosen == measure_names[1]) {
          choices <- c("HbA1C done", "HbA1C not done")
        } else if (input$qim_chosen == measure_names[2]) {
          choices <- c("Not defined", "Non smoker", "Ex smoker", "Smoker")
        } else if (input$qim_chosen == measure_names[3]) {
          choices <- c("Not defined",
                       "Underweight", "Healthy", "Overweight", "Obese")
        } else if (input$qim_chosen == measure_names[4]) {
          choices <- c("Influenza done", "Influenza not done")
        } else if (input$qim_chosen == measure_names[5]) {
          choices <- c("Influenza done", "Influenza not done")
        } else if (input$qim_chosen == measure_names[6]) {
          choices <- c("Influenza done", "Influenza not done")
        } else if (input$qim_chosen == measure_names[7]) {
          choices <- c("Alcohol done", "Alcohol not done")
        } else if (input$qim_chosen == measure_names[8]) {
          choices <- c("CVD Risk done", "CVD Risk not done")
        } else if (input$qim_chosen == measure_names[9]) {
          choices <- c("CST done", "CST not done")
        } else if (input$qim_chosen == measure_names[10]) {
          choices <- c("BP done", "BP not done")
        } else {
          warning("Invalid QIM choice")
          choices <- NULL
        }
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "mirror_group",
          choices = choices,
          selected = NULL # default is no choice
        )
      }
    }
  )

  output$charting_mirror <- shiny::renderUI({
    shiny::tagList(

    )
  })

  ##### create summary table ####################################################

  shiny::observeEvent(
    c(report_filled(),
      input$series_chosen),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      browser()
      shiny::req(report_filled())

      report <- report_filled %>>%
        dplyr::group_by(!!!dplyr::sums(input$series_chosen)) %>>%
        dplyr::summarise(n = sum(n)) %>>%
        dplyr::ungroup() %>>%
        dplyr::mutate(
          series_name = paste(!!!syms(input$series_chosen), sep = " + ")
        )

      report_grouped(report)

    }
  )

  shiny::observeEvent(
    input$show_grouped_values,
    ignoreInit = TRUE, {
      shiny::showModal(shiny::modalDialog(
        title = "Report",
        DT::renderDataTable({
          DT::datatable(
            data = report_grouped(),
            extensions = c("Buttons", "Responsive"),
            options = list(scrollX = TRUE,
              dom = "frtiBp",
              buttons = I("colvis"))
          )
        }),
        easyClose = TRUE,
        size = "l",
        footer = NULL
      ))
    }
  )

  ##### Restore/Load CSV report #################################################
  # includes restoration from

  shiny::observeEvent(
    report$report_values(),
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(report$report_values())

      if (nrow(report$report_values()) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Detected report creation",
            "Use created report in 'Report Charter'?",
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("ok_copy_report"), "OK")
            )
          )
        )

        shiny::observeEvent(
          input$ok_copy_report,
          ignoreInit = TRUE, ignoreNULL = TRUE, {
            report_values(report$report_values())
            # copy the dataframe
            shiny::removeModal()
          }
        )
      }
    }
  )

  shiny::observeEvent(
    input$loadCSVFile,
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(input$loadCSVFile)

      inFile <- input$loadCSVFile

      data <- read.csv(
        inFile$datapath,
        stringsAsFactors = FALSE,
        na.strings = "NA"
      )

      if (
        !all(
          c("QIM", "Age10", "Sex", "Indigenous",
            "DiabetesType", "Measure", "State", "n",
            "DateFrom", "DateTo")
          %in% names(data)
        )
      ) {
        # absolute minimum columns are not present
        shinytoastr::toastr_error(
          message = paste(
            "Not a valid GPstat QIM report"
          ),
          position = "bottom-left",
          closeButton = TRUE,
          timeOut = 0
        )
      } else {
        # all required columns are present
        data <- data %>>%
          dplyr::mutate(
            Age10 = as.numeric(Age10),
            n = as.numeric(n),
            DateFrom = as.Date(DateFrom),
            DateFrom = as.Date(DateTo),
          )
        report_values(data)
      }
    })

  shiny::observeEvent(
    c(report_values(), input$qim_chosen,
      input$sex_chosen, input$ethnicity_chosen,
      input$diabetes_chosen),
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(report_values)

      if (nrow(report_values()) > 0) {
        report <- report_values() %>>%
          # just the necessary columns
          dplyr::select(
            QIM, Age10, Sex, Indigenous, DiabetesType,
            Measure, State, n, DateFrom, DateTo
          ) %>>%
          dplyr::filter(
            # just the QIM that is chosen
            as.numeric(stringi::stri_sub(QIM, -2, -1)) ==
              which(measure_names == input$qim_chosen)
          ) %>>%
          # fill in the 'missing' demographic possiblities/rows ('n' = 0)
          dMeasureQIM::fill_demographics() %>>%
          tidyr::replace_na(
            # replace 'NA' not available with various
            # textual descriptions
            list(Sex = "Not stated", Indigenous = "Not stated",
                 DiabetesType = "Not stated",
                 State = "Not defined")
          ) %>>%
          # then filter by age, sex, ethnicity and diabetestype
          # (if specified by the user)
          dplyr::filter(
            dplyr::between(Age10, input$age_range[1], input$age_range[2]),
            Sex %in% input$sex_chosen,
            Indigenous %in% input$ethnicity_chosen,
            DiabetesType %in% input$diabetes_chosen
          ) %>>%
          dplyr::select(
            QIM, Age10, Sex, Indigenous, DiabetesType,
            Measure, State, n, DateFrom, DateTo
          ) # may need to 're-order' columns

        report_filled(report)
      }
    }
  )

shiny::observeEvent(
  input$show_report_values,
  ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      title = "Report",
      DT::renderDataTable({
        DT::datatable(
          data = report_values(),
          extensions = c("Buttons", "Responsive"),
          options = list(scrollX = TRUE,
                         dom = "frtiBp",
                         buttons = I("colvis"))
        )
      }),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  }
)

}
