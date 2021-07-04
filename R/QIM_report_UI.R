# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @name qim_report_UI
#' @title dMeasure Quality Improvement Measures - reports for QIM UI
#'
#' need definitions including 'measure_names'
#'
#' @include QualityImprovementMeasures.R
NULL

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
          style = "height:25em",
          shiny::tags$h5("Create report"),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::actionButton(
                inputId = ns("report_createReport"),
                label = "Go!"
              )
            ),
            shiny::column(
              width = 9,
              shinyWidgets::awesomeCheckboxGroup(
                inputId = ns("report_filter_options"),
                label = "",
                choices = c(
                  "Small cell suppression",
                  "Include all demographics groups"
                ),
                selected = "Small cell suppression"
              )
            )
          ),
          shiny::hr(),
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
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          style = "height:25em",
          shiny::tags$h5("Report store/save"),
          shiny::hr(),
          shiny::tags$h6("CSV 'spreadsheet' download"),
          shiny::icon("download"),
          shinyjs::disabled(
            # enable when there is something to save...
            shinyFiles::shinySaveButton(
              id = ns("csv_filename"),
              label = "Download CSV file",
              title = "Download CSV file - choose CSV filename",
              filename = paste0("QIMReport-", Sys.Date()),
              filetype = list(spreadsheet = c('csv'))
            )
          ),
          shiny::hr(),
          shiny::tags$h6("JSON 'PIP' download"),
          shiny::icon("download"),
          shinyjs::disabled(
            # enable when there is something to save...
            shinyFiles::shinySaveButton(
              id = ns("json_filename"),
              label = "Download JSON file",
              title = "Download JSON file - choose JSON filename",
              filename = paste0("QIMReport-", Sys.Date()),
              filetype = list(json = c('json'))
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
#' @param report a list returned by qim_reportCharter
#'   should contain $report_values(), which is a dataframe
#'
#' @return list with following components
#' \describe{
#'  \item{report_values}{dataframe of report}
#' }
#'
#' @export
qim_reportCreator <- function(input, output, session, dMQIM, report) {
  ns <- session$ns

  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileSave(
    input, id = "csv_filename", roots = volumes, session = session,
    restrictions = system.file(package = "base")
  )
  shinyFiles::shinyFileSave(
    input, id = "json_filename", roots = volumes, session = session,
    restrictions = system.file(package = "base")
  )

  empty_result <- data.frame(
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
  report_values <- reactiveVal(empty_result)
  shiny::observeEvent(
    report_values(),
    ignoreInit = FALSE, ignoreNULL = FALSE, {
      if (is.null(report_values()) || nrow(report_values()) == 0) {
        # disable download button if nothing to download
        shinyjs::disable("csv_filename")
        shinyjs::disable("json_filename")
      } else {
        shinyjs::enable("csv_filename")
        shinyjs::enable("json_filename")
      }
    }
  )
  shiny::observeEvent(
    input$report_createReport,
    ignoreInit = TRUE, {
      shiny::req(length(input$report_qim_chosen) > 0)
      # need to have at least one report to write

      # prepare a data frame
      report_values(empty_result)

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

      small_cell_suppression <- "Small cell suppression" %in%
        input$report_filter_options
      include_all_demographic_groups <- "Include all demographics groups" %in%
        input$report_filter_options

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

        qim <- empty_result
        if (measure_names[[1]] %in% input$report_qim_chosen) {
          qim01 <- getReport(
            dMQIM$report_qim_diabetes,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 01 - Diabetes HbA1C",
            measure = "HbA1C", require_type_diabetes = TRUE,
            qim_name = "QIM 01", measure_name = "HbA1C",
            state_variable_name = dplyr::quo(HbA1CDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim01)
        }

        if (measure_names[[2]] %in% input$report_qim_chosen) {
          qim02 <- getReport(
            dMQIM$report_qim_15plus,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 02 - 15+ smoking",
            measure = "Smoking", require_type_diabetes = FALSE,
            qim_name = "QIM 02", measure_name = "Smoking",
            state_variable_name = dplyr::quo(SmokingStatus),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim02)
        }

        if (measure_names[[3]] %in% input$report_qim_chosen) {
          qim03 <- getReport(
            dMQIM$report_qim_15plus,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 03 - 15+ BMI Class",
            measure = "Weight", require_type_diabetes = FALSE,
            qim_name = "QIM 03", measure_name = "BMIClass",
            state_variable_name = dplyr::quo(BMIClass),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim03)
        }

        if (measure_names[[4]] %in% input$report_qim_chosen) {
          qim04 <- getReport(
            dMQIM$report_qim_65plus,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            min_date = dMeasure::add_age(date_to, 1, "-15 month"),
            # according to PIP QI Improvement Measures Technical Specifications V1.2 (22102020)
            # QIM 04, page 16
            #
            # Exclude clients from the calculation if they:
            #  - did not have the immunisation due to documented medical reasons (e.g. allergy),
            #    system reasons (vaccine not available),or patient reasons (e.g. refusal);
            #  - or had results from measurements conducted outside of the service which were not available to the service
            #    and had not visited the service in the previous 15 months.
            #
            # by the second exclusion criteria, the most recent contact must be within 15 months
            # of the 'date_to' report date (although 'date_from' could be different to that, as
            # by default the period for contact calculation is 2 years before 'date_to')
            #
            # presumably if a person *was* vaccinated at the clinic within the past 15 months,
            # then the person has also visited the clinic within the last 15 months
            progress = progress, progress_detail = "QIM 04 - 65+ Influenza",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 04", measure_name = "InfluenzaDone",
            state_variable_name = dplyr::quo(InfluenzaDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim04)
        }

        if (measure_names[[5]] %in% input$report_qim_chosen) {
          qim05 <- getReport(
            dMQIM$report_qim_diabetes,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 05 - Diabetes Influenza",
            measure = "Influenza", require_type_diabetes = TRUE,
            qim_name = "QIM 05", measure_name = "InfluenzaDone",
            state_variable_name = dplyr::quo(InfluenzaDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim05)
        }

        if (measure_names[[6]] %in% input$report_qim_chosen) {
          qim06 <- getReport(
            dMQIM$report_qim_copd,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 06 - COPD Influenza",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 06", measure_name = "InfluenzaDone",
            state_variable_name = dplyr::quo(InfluenzaDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim06)
        }

        if (measure_names[[7]] %in% input$report_qim_chosen) {
          qim07 <- getReport(
            dMQIM$report_qim_15plus,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 07 - 15+ Alcohol",
            measure = "Alcohol", require_type_diabetes = FALSE,
            qim_name = "QIM 07", measure_name = "AlcoholDone",
            state_variable_name = dplyr::quo(AlcoholDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim07)
        }

        if (measure_names[[8]] %in% input$report_qim_chosen) {
          qim08 <- getReport(
            dMQIM$report_qim_cvdRisk,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 08 - CVD Risk",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 08", measure_name = "CVDRiskDone",
            state_variable_name = dplyr::quo(CVDriskDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim08)
        }

        if (measure_names[[9]] %in% input$report_qim_chosen) {
          qim09 <- getReport(
            dMQIM$report_qim_cst,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 09 - Cervical screening",
            measure = NA, require_type_diabetes = FALSE,
            qim_name = "QIM 09", measure_name = "CSTDone",
            state_variable_name = dplyr::quo(CSTDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim09)        }

        if (measure_names[[10]] %in% input$report_qim_chosen) {
          qim10 <- getReport(
            dMQIM$report_qim_diabetes,
            date_from = date_from, date_to = date_to,
            contact_type = contact_type, min_contact = min_contact,
            progress = progress, progress_detail = "QIM 10 - Diabetes BP",
            measure = "BP", require_type_diabetes = TRUE,
            qim_name = "QIM 10", measure_name = "BPDone",
            state_variable_name = dplyr::quo(BPDone),
            small_cell_suppression = small_cell_suppression,
            include_all_demographic_groups = include_all_demographic_groups
          )
          qim <- rbind(qim, qim10)
        }

        report_values(
          rbind(
            report_values(),
            qim %>>%
              dplyr::select(QIM, Age10, Sex, Indigenous, DiabetesType, Measure,
                            State, n, ProportionDemographic) %>>%
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

  shiny::observeEvent(
    input$csv_filename,
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(input$csv_filename) # can't be NULL/empty

      datapath <- shinyFiles::parseSavePath(volumes, input$csv_filename) %>>%
        dplyr::pull(datapath)
      if (length(datapath)) {
        # if length 0 (i.e. datapath == character(0), then return empty string)
        write.csv(report_values(), file = datapath, row.names = FALSE)
      }
    }
  )

  # if json_filename clicked,
  # then ask various questions about which data to export
  # and author/practice ID and small cell suppression
  shiny::observeEvent(
    input$json_filename,
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(input$json_filename) # can't be NULL/empty
      datapath <- shinyFiles::parseSavePath(volumes, input$json_filename) %>>%
        dplyr::pull(datapath)

      unique_DateTo <- unique(report_values()$DateTo)

      if (length(datapath)) {
        # if length 0 (i.e. datapath == character(0), then return empty string)

        shiny::showModal(shiny::modalDialog(
          title = "Practice Incentive Program JSON attributes",
          shiny::selectInput(
            inputId = ns("json_DateTo"),
            label = "'Date To' of period",
            choices = unique_DateTo
          ),
          shiny::textInput(
            inputId = ns("json_author_id"),
            label = "Author ID",
            value = "bpsrawdata"
          ),
          shiny::textInput(
            inputId = ns("json_practice_id"),
            label = "Practice ID",
            value = "",
            placeholder = "Your practice ID"
          ),
          shiny::checkboxInput(
            inputId = ns("json_small_cell_suppression"),
            label = "Small cell suppression",
            value = TRUE
          ),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(inputId = ns("json_ok"), "Save JSON")
          )
        ))
      }
    })

  # if 'Save JSON' button clicked in write JSON modal dialog
  # then create JSON string and output to file
  shiny::observeEvent(
    input$json_ok,
    ignoreInit = TRUE, ignoreNULL = TRUE, {

      datapath <- shinyFiles::parseSavePath(volumes, input$json_filename) %>>%
        dplyr::pull(datapath)

      json_string <- writeReportJSON(
        d = report_values(),
        date_to = input$json_DateTo,
        author_id = input$json_author_id,
        practice_id = input$json_practice_id,
        small_cell_suppression = input$json_small_cell_suppression,
      )
      write(json_string, file = datapath)

      shiny::removeModal()
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
          timeOut = 20000
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
          timeOut = 10000
        )
        n_report_warning(TRUE)
      }
    }
  )

  # if report
  shiny::observeEvent(
    report$report_values(),
    ignoreInit = TRUE, ignoreNULL = TRUE, {
      shiny::req(report$report_values())

      if (nrow(report$report_values()) > 0) {
        report_values(
          report$report_values() %>>%
            dplyr::mutate(DateTo = as.character(DateTo))
          # co-erce to character (instead of numeric)
        )
        # copy the dataframe
        shinytoastr::toastr_info(
          "Copying report to Report Creator, can be saved as JSON",
          closeButton = TRUE,
          position = "bottom-left", title = "PIP report"
        )
      }
    }
  )

  return(list(report_values = reactive({report_values()})))
}
