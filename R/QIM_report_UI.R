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

      for (i in 1:input$report_number) {
        # in this case, we can be confident that input$report_number
        #  is not a negative number or zero!

        if (measure_names[[1]] %in% input$report_qim_chosen) {
          qim01 <- dMQIM$report_qim_diabetes(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            measure = "HbA1C", type_diabetes = TRUE,
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 01",
                          Measure = "HbA1C",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = HbA1CDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim01
            )
          )
        }

        if (measure_names[[2]] %in% input$report_qim_chosen) {
          qim02 <- dMQIM$report_qim_15plus(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            measure = "Smoking",
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 02",
                          Measure = "Smoking",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = SmokingStatus,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim02
            )
          )
        }

        if (measure_names[[3]] %in% input$report_qim_chosen) {
          qim03 <- dMQIM$report_qim_15plus(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            measure = "Weight",
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 03",
                          Measure = "BMIClass",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = BMIClass,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim03
            )
          )
        }

        if (measure_names[[4]] %in% input$report_qim_chosen) {
          qim04 <- dMQIM$report_qim_65plus(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 04",
                          Measure = "InfluenzaDone",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = InfluenzaDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim04
            )
          )
        }
        if (measure_names[[5]] %in% input$report_qim_chosen) {
          qim05 <- dMQIM$report_qim_diabetes(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            measure = "Influenza", type_diabetes = TRUE,
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 05",
                          Measure = "InfluenzaDone",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = InfluenzaDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim05
            )
          )
        }

        if (measure_names[[6]] %in% input$report_qim_chosen) {
          qim06 <- dMQIM$report_qim_copd(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 06",
                          Measure = "InfluenzaDone",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = InfluenzaDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim06
            )
          )
        }

        if (measure_names[[7]] %in% input$report_qim_chosen) {
          qim07 <- dMQIM$report_qim_15plus(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            measure = "Alcohol",
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 07",
                          Measure = "AlcoholDone",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = AlcoholDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim07
            )
          )
        }

        if (measure_names[[8]] %in% input$report_qim_chosen) {
          qim08 <- dMQIM$report_qim_cvdRisk(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 08",
                          Measure = "CVD Risk Done",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = CVDriskDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim08
            )
          )
        }

        if (measure_names[[9]] %in% input$report_qim_chosen) {
          qim09 <- dMQIM$report_qim_cst(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 09",
                          Measure = "CST Done",
                          DiabetesType = "",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = CSTDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim09
            )
          )
        }

        if (measure_names[[10]] %in% input$report_qim_chosen) {
          qim10 <- dMQIM$report_qim_diabetes(
            contact = TRUE, date_from = date_from, date_to = date_to,
            min_contact = min_contact,
            # 'default' is clinician list chosen in right panel
            min_date = as.Date("2000-01-01"), max_date = Sys.Date(),
            # no limitations on min/max date essentially
            contact_type = contact_type,
            demographic = c("Age10", "Sex", "Indigenous"),
            measure = "BP", type_diabetes = TRUE,
            ignoreOld = TRUE, lazy = FALSE, store = FALSE
          ) %>>%
            dplyr::mutate(QIM = "QIM 10",
                          Measure = "BPDone",
                          DateFrom = as.character(date_from),
                          DateTo = as.character(date_to)) %>>%
            dplyr::rename(State = BPDone,
                          ProportionDemographic = Proportion_Demographic) %>>%
            dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType,
                          Measure, State, n, ProportionDemographic,
                          DateFrom, DateTo)
          # keep Age10, Sex, Indigenous, DiabetesType, HbA1CDone,
          # n, Proportion_Demographic
          report_values(
            rbind(
              report_values(),
              qim10
            )
          )
        }


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
