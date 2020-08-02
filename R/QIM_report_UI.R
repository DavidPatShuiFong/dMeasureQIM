# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

#' @export
qim_reportCreator_UI <- function(id) {
  ns <- shiny::NS(id)

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
          style = "height:20em",
          shiny::tags$h5("Create report"),
          shiny::actionButton(
            inputId = ns("report_createReport"),
            label = "Go!"
          ),
          shiny::hr(),
          shiny::textInput(
            inputId = ns("filename"),
            label = "Name for file",
            value = paste0("QIMReport-", Sys.Date(), ".csv")
          ),
          shiny::downloadButton(
            outputId = ns("download_button"),
            label = "Download"
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:20em",
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

  report_values <- reactiveVal(NULL)
  shiny::observeEvent(
    report_values(),
    ignoreInit = FALSE, ignoreNULL = FALSE, {
      if (is.null(report_values()) || nrow(report_values()) == 0) {
        # disable download button if nothing to download
        shinyjs::disable("download_button")
      } else {
        shinyjs::enable("download_button")
      }
    }
  )
  shiny::observeEvent(
    input$report_createReport,
    ignoreInit = TRUE, {
      shiny::req(length(input$report_qim_chosen) > 0)
      # need to have at least one report to write

      # prepare a data frame
      report_values <- data.frame(
        QIM = character(),
        Age10 = numeric(),
        Sex = character(),
        Indigenous = character(),
        DiabetesType = character(),
        Measure = character(),
        State = character(),
        n = numeric(),
        ProportionDemographic = numeric(),
        DateFrom = character(),
        DateTo = character()
      )

      date_to <- input$report_endDate
      date_from <- seq.Date(
        date_to,
        by = paste0(
          "-", as.numeric(input$report_duration_n),
          " ",
          stringi::stri_sub(
            tolower(input$report_duration_unit), 1, -2
          )
        ),
        length.out = 2
      )[[2]] # only take the 'end' of the sequence
      min_contact = input$report_min_contact
      contact_type = input$report_contact_type

      if (input$report_number > 1) {
        # create progress bar
        report_progress <- shiny::Progress$new(
          session, min = 0, max = input$report_number
        )
        # close progress bar when function exits
        on.exit(report_progress$close())
        # initialize
        report_progress$set(
          message = "Calculating report",
          detail = "1",
          value = 0
        )
      }

      for (i in 1:input$report_number) {
        # in this case, we can be confident that input$report_number
        #  is not a negative number or zero!

        # create progress bar
        progress <- shiny::Progress$new(
          session, min = 0, max = length(input$report_qim_chosen)
        )
        # initialize
        progress$set(
          message = "Calculating measure",
          value = 0
        )

        qim <- data.frame(
          QIM = character(),
          Age10 = numeric(),
          Sex = character(),
          Indigenous = character(),
          DiabetesType = character(),
          Measure = character(),
          State = character(),
          n = numeric(),
          ProportionDemographic = numeric(),
          DateFrom = character(),
          DateTo = character()
        )

        # gets report from 'report_function'
        # then adds extra 'DiabetesType' column if necessary
        # adds more columns describing the measurement
        # renames the 'result' column to 'State'
        #
        # called to create reports in a 'standard' format
        # from the 10 QIM measures
        getReport <- function(
          report_function, # the dMQIM method to call
          progress_detail, # the message to report in the 'progress' dialog
          measure = NA,
          # some (but not all) 'report_function' can choose the 'measure'
          # if not required, then use 'NA'
          require_type_diabetes,
          # diabetes methods may give option to specify diabetes types
          qim_name, measure_name,
          # the overall name of the QIM, and the name of the measure
          state_variable_name
          # the name of the column in the return from 'report_function'
          # which becomes the 'State' column
        ) {
          progress$inc(amount = 1, detail = progress_detail)

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
                dplyr::mutate(.,
                  DiabetesType = "" # create a 'blank'
                )
              }
            } %>>%
            dplyr::rename(
              State = !!state_variable_name,
              ProportionDemographic = Proportion_Demographic
            ) %>>%
            dplyr::select(
              QIM, Age10, Sex, Indigenous, DiabetesType,
              Measure, State, n, ProportionDemographic
            )
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic

          return(qim_report)
        }

        if (measure_names[[1]] %in% input$report_qim_chosen) {
          qim01 <- getReport(
            dMQIM$report_qim_diabetes,
            progress_detail = "QIM 01 - Diabetes HbA1C",
            measure = "HbA1C", require_type_diabetes = TRUE,
            qim_name = "QIM 01", measure_name = "HbA1C",
            state_variable_name = dplyr::quo(HbA1CDone)
          )
          qim <- rbind(qim, qim01)
        }

        if (measure_names[[2]] %in% input$report_qim_chosen) {
          qim02 <- getReport(
            dMQIM$report_qim_15plus,
            progress_detail = "QIM 02 - 15+ smoking",
            measure = "Smoking", require_type_diabetes = FALSE,
            qim_name = "QIM 02", measure_name = "Smoking",
            state_variable_name = dplyr::quo(SmokingStatus)
          )
          qim <- rbind(qim, qim02)
        }

        if (measure_names[[3]] %in% input$report_qim_chosen) {
          qim03 <- getReport(
            dMQIM$report_qim_15plus,
            progress_detail = "QIM 03 - 15+ BMI Class",
            measure = "Weight", require_type_diabetes = FALSE,
            qim_name = "QIM 03", measure_name = "BMIClass",
            state_variable_name = dplyr::quo(BMIClass)
          )
          qim <- rbind(qim, qim03)
        }

        if (measure_names[[4]] %in% input$report_qim_chosen) {
          qim04 <- getReport(
            dMQIM$report_qim_65plus,
            progress_detail = "QIM 04 - 64+ Influenza",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 04", measure_name = "InfluenzaDone",
            state_variable_name = dplyr::quo(InfluenzaDone)
          )
          qim <- rbind(qim, qim04)
        }

        if (measure_names[[5]] %in% input$report_qim_chosen) {
          qim05 <- getReport(
            dMQIM$report_qim_diabetes,
            progress_detail = "QIM 05 - Diabetes Influenza",
            measure = "Influenza", require_type_diabetes = TRUE,
            qim_name = "QIM 05", measure_name = "InfluenzaDone",
            state_variable_name = dplyr::quo(InfluenzaDone)
          )
          qim <- rbind(qim, qim05)
        }

        if (measure_names[[6]] %in% input$report_qim_chosen) {
          qim06 <- getReport(
            dMQIM$report_qim_copd,
            progress_detail = "QIM 06 - COPD Influenza",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 06", measure_name = "InfluenzaDone",
            state_variable_name = dplyr::quo(InfluenzaDone)
          )
          qim <- rbind(qim, qim06)
        }

        if (measure_names[[7]] %in% input$report_qim_chosen) {
          qim07 <- getReport(
            dMQIM$report_qim_15plus,
            progress_detail = "QIM 07 - 15+ Alcohol",
            measure = "Alcohol", require_type_diabetes = FALSE,
            qim_name = "QIM 07", measure_name = "AlcoholDone",
            state_variable_name = dplyr::quo(AlcoholDone)
          )
          qim <- rbind(qim, qim07)
        }

        if (measure_names[[8]] %in% input$report_qim_chosen) {
          qim08 <- getReport(
            dMQIM$report_qim_cvdRisk,
            progress_detail = "QIM 08 - CVD Risk",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 08", measure_name = "CVD Risk Done",
            state_variable_name = dplyr::quo(CVDriskDone)
          )
          qim <- rbind(qim, qim08)
        }

        if (measure_names[[9]] %in% input$report_qim_chosen) {
          qim09 <- getReport(
            dMQIM$report_qim_cst,
            progress_detail = "QIM 09 - Cervical screening",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 09", measure_name = "CST Done",
            state_variable_name = dplyr::quo(CSTDone)
          )
          qim <- rbind(qim, qim09)        }

        if (measure_names[[10]] %in% input$report_qim_chosen) {
          qim10 <- getReport(
            dMQIM$report_qim_diabetes,
            progress_detail = "QIM 10 - Diabetes BP",
            measure = "BP", require_type_diabetes = TRUE,
            qim_name = "QIM 10", measure_name = "BP Done",
            state_variable_name = dplyr::quo(BPDone)
          )
          qim <- rbind(qim, qim10)
        }

        report_values(
          rbind(
            report_values(),
            qim %>>%
              dplyr::arrange(QIM, Age10, Sex, Indigenous, DiabetesType, State) %>>%
              dplyr::mutate(
                DateFrom = date_from, DateTo = date_to,
                ContactType = paste(contact_type, collapse = ", "),
                MinContact = min_contact,
                Clinicians = paste(dMQIM$dM$clinicians, collapse = ", ")
              )
          )
        )
        # close QIM progress bar
        progress$close()

        if (input$report_number > 1) {
          # update progress bar
          report_progress$inc(
            amount = 1,
            detail = as.character(min(i + 1, input$report_number))
          )
          # update dates for next loop
          report_spacing <- paste0(
            "-", as.numeric(input$report_spacing_n),
            " ",
            stringi::stri_sub(
              tolower(input$report_spacing_unit), 1, -2
            )
          )
          date_from <- seq.Date(
            date_from,
            by = report_spacing,
            length.out = 2
          )[[2]]
          date_to <- seq.Date(
            date_to,
            by = report_spacing,
            length.out = 2
          )[[2]]
        }
      }
    })

  output$download_button <- shiny::downloadHandler(
    filename = function() {
      input$filename
    },
    content = function(file) {
      write.csv(report_values(), file = file, row.names = FALSE)
    }
  )

  # standard QIM report is generated from
  # 3 services within a 24-month period
  #
  # warn if non-standard contact period is being chosen
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

  # warn if non-standard 'contact' type and number being chosen
  contact_type_warning <- shiny::reactiveVal(FALSE)
  contact_appointment_warning <- shiny::reactiveVal(FALSE)
  contact_visits_warning <- shiny::reactiveVal(FALSE)
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

      if ("Appointments" %in% input$report_contact_type &&
          !contact_appointment_warning()) {
        shinytoastr::toastr_warning(
          message = paste(
            "Choose valid appointment 'status' from the right sidebar.",
            "e.g. Invoiced, Waiting, Booked..."
          ),
          position = "bottom-center",
          closeButton = TRUE,
          timeOut = 20000
        )
        contact_appointment_warning(TRUE)
      }

      if ("Visits" %in% input$report_contact_type &&
          !contact_visits_warning()) {
        shinytoastr::toastr_warning(
          message = paste(
            "Choose valid visit 'types' from the right sidebar.",
            "e.g. Surgery, Telephone, Telhealth, SMS, Email..."
          ),
          position = "bottom-center",
          closeButton = TRUE,
          timeOut = 20000
        )
        contact_visits_warning(TRUE)
      }
    }
  )


  # warn if more than one report being generated
  # could take a long time!
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
