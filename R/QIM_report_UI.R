# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @export
qim_reportCreator_UI <- function(id) {
  ns <- shiny::NS(id)

  measure_names <- c(
    "QIM 01 - Diabetes HbA1C results",
    "QIM 02 - 15+ smoking status",
    "QIM 03 - 15+ weight classification",
    "QIM 04 - 65+ influenza immunization",
    "QIM 05 - Diabetes influenza immunization",
    "QIM 06 - COPD influenza immunization",
    "QIM 07 - 15+ alcohol consumption status",
    "QIM 08 - Cardiovascular risk assessable",
    "QIM 09 - Cervical screening up-to-date",
    "QIM 10 - Diabetes blood pressure recording"
  )

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:15em",
          shiny::tags$h5("Contact period"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::dateInput(
                inputId = ns("report_endDate"),
                label = "Up to:",
                format = "D dd/M/yyyy",
                min = Sys.Date() - 9000,
                value = Sys.Date()
              )
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns("report_duration_n"),
                label = "Duration",
                choices = 1:30,
                multiple = FALSE,
                selected = "24"
              ),
              shinyWidgets::pickerInput(
                inputId = ns("report_duration_unit"),
                label = "",
                choices = c("Days", "Weeks", "Months"),
                multiple = FALSE,
                selected = "Months"
              )
            )
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:15em",
          shiny::tags$h5("Minimum contacts"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns("report_contact_type"),
                label = "Contact types",
                choices = c("Appointments", "Visits", "Services"),
                selected = "Services",
                options = list(
                  style = "btn-primary",
                  `actions-box` = TRUE
                ),
                multiple = TRUE
              )
            ),
            shiny::column(
              width = 6,
              shinyWidgets::sliderTextInput(
                inputId = ns("report_min_contact"),
                label = "Minimum number of contacts",
                choices = c(1:10),
                grid = TRUE,
                selected = 3
              )
            )
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:15em",
          shiny::tags$h5("QIM Measures"),
          shinyWidgets::pickerInput(
            inputId = ns("report_qim_chosen"),
            label = "",
            choices = measure_names,
            selected = measure_names,
            # consult choices initially selected
            options = list(
              style = "btn-primary",
              `actions-box` = TRUE
            ),
            multiple = TRUE
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:15em",
          shiny::tags$h5("Create report"),
          shiny::actionButton(
            inputId = ns("report_createReport"),
            label = "Go!"
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:15em",
          shiny::tags$h5("Number of reports"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shinyWidgets::sliderTextInput(
                inputId = ns("report_number"),
                label = "Number",
                choices = c(1:10),
                grid = TRUE,
                selected = 1
              )
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns("report_spacing_n"),
                label = "Spacing",
                choices = 1:30,
                multiple = FALSE,
                selected = "1"
              ),
              shinyWidgets::pickerInput(
                inputId = ns("report_spacing_unit"),
                label = "",
                choices = c("Days", "Weeks", "Months"),
                multiple = FALSE,
                selected = "Months"
              )
            )
          )
        )
      )
    )
  )
}


#' Quality Improvement report creation - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#'
#' @return none
#'
#' @export
qim_reportCreator <- function(input, output, session, dMQIM) {
  ns <- session$ns

  duration_warning <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(
    c(input$report_duration_n, input$report_duration_unit),
    ignoreInit = TRUE, {
      if ((input$report_duration_n != 24 ||
           input$report_duration_unit != "Months") &&
          !duration_warning()) {
        shinytoastr::toastr_warning(
          message = paste(
            "'Standard' QIM report contact duration is 24 months"
          ),
          position = "bottom-center",
          closeButton = TRUE,
          timeOut = 0
        )
        duration_warning(TRUE)
      }
    }
  )

  contact_type_warning <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(
    c(input$report_contact_type, input$report_min_contact),
    ignoreInit = TRUE, ignoreNULL = FALSE, {
      if ((input$report_contact_type != "Services" ||
           input$report_min_contact != 3) &&
          !contact_type_warning()) {
        shinytoastr::toastr_warning(
          message = paste(
            "'Standard' QIM report contact type is minimum",
            "of three (3) services."
          ),
          position = "bottom-center",
          closeButton = TRUE,
          timeOut = 0
        )
        contact_type_warning(TRUE)
      }
    }
  )

  n_report_warning <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(
    input$report_number,
    ignoreInit = TRUE, {
      if (input$report_number != 1 &&
          !n_report_warning()) {
        shinytoastr::toastr_warning(
          message = paste(
            "Each report could take a long time",
            "to create!"
          ),
          position = "bottom-center",
          closeButton = TRUE,
          timeOut = 0
        )
        n_report_warning(TRUE)
      }
    }
  )


}
