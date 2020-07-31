# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Interface elements of dMeasure Billings
#'
#' requires R6 methods from QualityImprovementMeasures.R
#'
#' @include QualityImprovementMeasures.R
NULL

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  x <- list(
    shinydashboard::sidebarMenu(
      .list = list(
        shinydashboard::menuItem(
          "PIP Quality Improvement",
          tabName = "qimRept",
          icon = shiny::icon("chart-line")
        ),
        shinydashboard::menuItem(
          "QIM Appointment",
          tabName = "qimAppt",
          icon = shiny::icon("chart-line")
        )
      )
    )
  )
  return(x)
}

#' center panel description
#'
#' @name dMeasureShinytabItems
#'
#' @return shinytabItems
#'
#' @export
dMeasureShinytabItems <- function() {
  x <- c(
    list(shinydashboard::tabItem(
      tabName = "qimRept",
      shiny::fluidRow(column(
        width = 12, align = "center",
        h2("Quality Improvement Measure Reporting")
      )),
      shiny::fluidRow(column(
        width = 12,
        dMeasureQIM::datatableUI("qimRept")
      ))
    )),
    list(shinydashboard::tabItem(
      tabName = "qimAppt",
      shiny::fluidRow(column(
        width = 12, align = "center",
        h2("Quality Improvement Measure Appointment View")
      )),
      shiny::fluidRow(column(
        width = 12,
        dMeasureQIM::datatableUI("qimAppt")))
    ))
  )
  return(x)
}

#' QIM module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @name datatableUI
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
#'
#' @export
datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("qim_datatable_wrapper"),
    shiny::fluidRow(
      shiny::tagList(
        shinydashboard::tabBox(
          id = ns("tab_qim"),
          title = shiny::tagList(
            shiny::div(
              style = "display:inline-block",
              shiny::uiOutput(ns("settings_group"))
            ),
            shiny::div(
              style = "display:inline-block",
              "Quality Improvement Measures"
            )
          ),
          width = 12,
          height = "85vh",
          shiny::tabPanel(
            title = "Active",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_active_UI(ns("qim_active"))
          ),
          shiny::tabPanel(
            title = "Diabetes",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_diabetes_UI(ns("qim_diabetes"))
          ),
          shiny::tabPanel(
            title = "Cervical Screening",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_cst_UI(ns("qim_cst"))
          ),
          shiny::tabPanel(
            title = "15+",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_15plus_UI(ns("qim_15plus"))
          ),
          shiny::tabPanel(
            title = "65+",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_65plus_UI(ns("qim_65plus"))
          ),
          shiny::tabPanel(
            title = "COPD (Lung Disease)",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_copd_UI(ns("qim_copd"))
          ),
          shiny::tabPanel(
            title = "Cardiovascular risk",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_cvdRisk_UI(ns("qim_cvdRisk"))
          ),
          shiny::tabPanel(
            title = "Report creator",
            width = 12,
            shiny::br(),
            dMeasureQIM::qim_reportCreator_UI(ns("qim_reportCreator"))
          )
        )
      )
    )
  )
}

#' @export
qim_active_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shinyWidgets::pickerInput(
          inputId = ns("list_view"),
          choices = c("Report", "List"),
          choicesOpt = list(icon = c(
            "fa fa-book-reader",
            "fa fa-clipboard-list"
          )),
          options = list(`icon-base` = ""),
          width = "15em"
        )
      )
    ),
    DT::DTOutput(ns("active_qim_table"))
  )
}

#' @export
qim_diabetes_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(ns("list_group"))
      ),
      shiny::column(
        width = 2,
        offset = 7, # note that total 'column' width = 12
        shiny::uiOutput(ns("measure_group"))
      )
    ),
    DT::DTOutput(ns("diabetes_qim_table"))
  )
}

#' @export
qim_cst_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(ns("list_group"))
      )
    ),
    DT::DTOutput(ns("cst_qim_table"))
  )
}

#' @export
qim_15plus_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(ns("list_group"))
      ),
      shiny::column(
        width = 2,
        offset = 7, # note that total 'column' width = 12
        shiny::uiOutput(ns("measure_group"))
      )
    ),
    DT::DTOutput(ns("fifteenplus_qim_table"))
  )
}

#' @export
qim_65plus_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(ns("list_group"))
      )
    ),
    DT::DTOutput(ns("sixtyfiveplus_qim_table"))
  )
}

#' @export
qim_copd_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(ns("list_group"))
      )
    ),
    DT::DTOutput(ns("copd_qim_table"))
  )
}


#' @export
qim_cvdRisk_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::uiOutput(ns("list_group"))
      ),
      shiny::column(
        width = 2,
        offset = 6, # note that total 'column' width = 12
        shiny::uiOutput(ns("groups"))
      )
    ),
    DT::DTOutput(ns("cvdRisk_qim_table"))
  )
}

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
            inputId = ns("report_visit_type"),
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

#' QIM module - server for DailyMeasure/GPstat! web UI
#'
#' @name datatableServer
#'
#' @param id id
#' @param dMQIM dMeasure QIM R6 object
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
datatableServer <- function(id, dMQIM, contact) {
  if (contact == FALSE) {
    dMQIM$qim_contact <- FALSE
    # uses appointment list, not contact list
    dMQIM$qim_demographicGroup <- c("")
    # by default, the 'appointment' module does not show QIM aggregate groups
  }

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # result management
    callModule(dMeasureQIM::qim_active, "qim_active", dMQIM, contact)
    callModule(dMeasureQIM::qim_diabetes, "qim_diabetes", dMQIM, contact)
    callModule(dMeasureQIM::qim_cst, "qim_cst", dMQIM, contact)
    callModule(dMeasureQIM::qim_15plus, "qim_15plus", dMQIM, contact)
    callModule(dMeasureQIM::qim_65plus, "qim_65plus", dMQIM, contact)
    callModule(dMeasureQIM::qim_copd, "qim_copd", dMQIM, contact)
    callModule(dMeasureQIM::qim_cvdRisk, "qim_cvdRisk", dMQIM, contact)

    if (!contact) {
      # only show the 'Active' panel if contact list
      #
      #  'Active' is whether the patient qualifies as 'active' depending
      #  on criteria such as appointments, visits (recordings in the file)
      #  or billings over a defined time period, and a certain number of
      #  times
      #
      # e.g. one definition of 'active' is 3 'visits' over 2 years
      # though an alternative definition could be three 'billings' over
      # three years

      # if an 'appointment' list is being used
      # then don't show the 'Active' panel
      shiny::removeTab(inputId = "tab_qim", target = "Active", session = session)
      # for some reason the above line doesn't remove the tab...
      shiny::updateTabsetPanel(session = session, inputId = "tab_qim", selected = "Diabetes")
    }
    initial_demographic <- dMQIM$qim_demographicGroup
    # this is an unusual kludge, for some reason specifying dMQIM$qim_demographicGroup
    # in the 'choices' of checkboxGroupButtons does not work

    demographic_chosen <- shiny::reactiveVal(
      initial_demographic
    )
    output$settings_group <- shiny::renderUI({
      shinyWidgets::dropMenu(
        shiny::actionButton(
          inputId = ns("qim_dropdown"),
          icon = shiny::icon("gear"),
          label = "Settings"
        ),
        shiny::tags$div(
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("ignore_old"),
            label = "Measurements",
            checkIcon = list(
              yes = shiny::icon("calendar-times"),
              no = shiny::icon("calendar-alt")
            ),
            choices = c("Ignore old measurements"),
            selected = c("Ignore old measurements"),
            status = "primary"
          ),
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("demographic_chosen"),
            label = "Demographic grouping",
            choices = dMQIM$qim_demographicGroupings,
            selected = demographic_chosen(),
            status = "primary",
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          )
        )
      )
    })
    shiny::observeEvent(
      input$qim_dropdown_dropmenu,
      ignoreInit = TRUE, {
        # this is triggered when shinyWidgets::dropMenu is opened/closed
        # tag is derived from the first tag in dropMenu, adding '_dropmenu'
        if (!input$qim_dropdown_dropmenu) {
          # only if closing the 'dropmenu' modal
          # unfortunately, is also triggered during Init (despite the ignoreInit)
          demographic_chosen(input$demographic_chosen)
        }
      }
    )

    shiny::observeEvent(demographic_chosen(), ignoreNULL = FALSE, {
      # change the filter depending on the dropdown
      dMQIM$qim_demographicGroup <- demographic_chosen()
    })
    shiny::observeEvent(input$ignore_old, ignoreNULL = FALSE, {
      # if selected, will filter out appointments older than current date
      dMQIM$qim_ignoreOld <- ("Ignore old measurements" %in% input$ignore_old)
    })
  })
}

#' Quality Improvement 'active' list - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_active <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  qim_active_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_active_listR(),
      dMQIM$qim_active_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view,
        dMQIM$qim_active_listR()
      )
      if (input$list_view == "List") {
        datatable_styled(
          dMQIM$qim_active_listR() %>>%
            dplyr::select(
              Patient, RecordNo,
              Age10, Sex, Indigenous, Ethnicity,
              MaritalStatus, Sexuality, Count
            ) %>>%
            # re-orders the fields
            {
              remove_demographic <- setdiff(
                dMQIM$qim_demographicGroupings,
                dMQIM$qim_demographicGroup
              )
              # finds the demographics that were NOT chosen
              dplyr::select(., -remove_demographic)
            },
          columnDefs = list(list(targets = 1:2, visible = FALSE))
          # Patient Name and RecordNo hidden by default
          # can be shown again with 'colVis' button
        )
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_active_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <- "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0 && !is.na(df[1, 2])) {
          # not an empty dataframe, or a 'NA' in the first proportion row
          dt <- dt %>>%
            DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3) %>>%
            DT::formatRound(which(names(df) %in% c("% of demographic")), digits = 1)
        }
        return(dt)
      }
    }
  )

  output$active_qim_table <- DT::renderDT({
    qim_active_datatable()
  },
  server = TRUE
  )
}

#' Quality Improvement diabetes - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_diabetes <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c(
        "fa fa-book-reader",
        "fa fa-clipboard-list"
      ))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em"
    )
  })

  output$measure_group <- shiny::renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("measure_group_dropdown"),
        icon = shiny::icon("gear"),
        label = "Diabetes Measures"
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("measure_chosen"),
          label = "Measures Chosen",
          choices = dMQIM$qim_diabetes_measureTypes,
          selected = dMQIM$qim_diabetes_measureTypes,
          # initially all chosen
          status = "primary"
        ),
        shiny::hr(),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("showType"),
          label = "Show Diabetes Types",
          checkIcon = list(
            yes = shiny::icon("check"),
            no = shiny::icon("times")
          ),
          choices = c("Show Types"),
          selected = ifelse(
            dMQIM$qim_diabetes_showType,
            "Show Types", # this should be the default
            NULL
          ),
          status = "primary"
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$measure_group_dropdown_dropmenu,
    ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$measure_group_dropdown_dropmenu) {
        dMQIM$qim_diabetes_measure <- input$measure_chosen
        dMQIM$qim_diabetes_showType <- !is.null(input$showType)
      }
    })

  qim_diabetes_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_diabetes_listR(),
      dMQIM$qim_diabetes_list_appointmentsR(),
      dMQIM$qim_diabetes_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view, # this might not be defined on first run!
        dMQIM$qim_diabetes_listR()
      )
      if (input$list_view == "List") {
        df <- dMQIM$qim_diabetes_listR() %>>%
          {
            dplyr::select(
              .,
              !!(intersect(
                c("Patient", "RecordNo",
                  "Age10", "Sex", "Indigenous", "Ethnicity",
                  "MaritalStatus", "Sexuality", "DiabetesType",
                  "HbA1CDate", "HbA1CValue", "HbA1CUnits",
                  "FluvaxDate", "FluvaxName",
                  "BPDate", "BP"),
                names(.)
              ))
            )
          } %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          } %>>% {
            if ("HbA1C" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(HbA1CDate, HbA1CValue, HbA1CUnits))
            }
          } %>>% {
            if ("Influenza" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(FluvaxDate, FluvaxName))
            }
          } %>>% {
            if ("BP" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(BPDate, BP))
            }
          }
        datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          columnDefs = list(list(
            targets =
              which(
                names(df) %in% c("Patient", "RecordNo")
                # needs name by index as columns might be removed
                # by demographic filters above
              ),
            visible = FALSE
          )),
          # Patient Name and RecordNo hidden by default
          scrollX = TRUE
        ) # this is a wide table
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_diabetes_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <-
          "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3) %>>%
            DT::formatRound(which(names(df) %in% c("% of demographic")), digits = 1)
        }
        return(dt)
      } else if (input$list_view == "Appointments") {
        df <- dMQIM$qim_diabetes_list_appointmentsR() %>>%
          {
            dplyr::select(
              .,
              !!(intersect(
                c("Patient", "RecordNo",
                  "AppointmentDate", "AppointmentTime",
                  "Provider", "Status",
                  "Age10", "Sex", "Indigenous", "Ethnicity",
                  "MaritalStatus", "Sexuality", "DiabetesType",
                  "HbA1CDate", "HbA1CValue", "HbA1CUnits",
                  "FluvaxDate", "FluvaxName",
                  "BPDate", "BP"),
                names(.)
              ))
            )
          } %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          } %>>% {
            if ("HbA1C" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(HbA1CDate, HbA1CValue, HbA1CUnits))
            }
          } %>>% {
            if ("Influenza" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(FluvaxDate, FluvaxName))
            }
          } %>>% {
            if ("BP" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(BPDate, BP))
            }
          }
        datatable_styled(
          df,
          extensions = c("Buttons", "Scroller")
        ) %>>% {
          if ("HbA1C" %in% input$measure_chosen) {
            DT::formatStyle(
              ., "HbA1CDate",
              backgroundcolor = DT::styleInterval(
                as.Date(Sys.Date() - 365), c("ffeeee", "eeffee")
              )
            )
          } else {
            .
          }
        }
      }
    }
  )

  output$diabetes_qim_table <- DT::renderDT({
    qim_diabetes_datatable()
  },
  server = TRUE
  )
}


#' Quality Improvement cervical screening test - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_cst <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c(
        "fa fa-book-reader",
        "fa fa-clipboard-list"
      ))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em"
    )
  })

  qim_cst_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_cst_listR(),
      dMQIM$qim_cst_list_appointmentsR(),
      dMQIM$qim_cst_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view,
        dMQIM$qim_cst_listR()
      )
      if (input$list_view == "List") {
        df <- dMQIM$qim_cst_listR() %>>%
          dplyr::select(
            Patient, RecordNo,
            Age10, Sex, Indigenous, Ethnicity,
            MaritalStatus, Sexuality,
            CSTDate, CSTName
          ) %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          }
        datatable_styled(
          df,
          columnDefs = list(list(
            targets =
              which(names(df) %in% c("Patient", "RecordNo")),
            # Patient Name and RecordNo hidden by default
            # needs name by index as columns might be removed
            # by demographic filters above
            visible = FALSE
          ))
        )
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_cst_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <-
          "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(
              which(names(df) %in% c("Proportion")),
              digits = 3
            ) %>>%
            DT::formatRound(
              which(names(df) %in% c("% of demographic")),
              digits = 1
            )
        }
        return(dt)
      } else if (input$list_view == "Appointments") {
        df <- dMQIM$qim_cst_list_appointmentsR() %>>%
          dplyr::select(
            Patient, RecordNo,
            AppointmentDate, AppointmentTime, Provider, Status,
            Age10, Sex, Indigenous, Ethnicity,
            MaritalStatus, Sexuality,
            CSTDate, CSTName
          ) %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          }
        datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          scrollX = TRUE
        ) # this is a wide table
      }
    }
  )

  output$cst_qim_table <- DT::renderDT({
    qim_cst_datatable()
  },
  server = TRUE
  )
}

##### Quality Improvement Measures 15+ ###############################################
#' Quality Improvement fifteen plus (15+ years) - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_15plus <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c(
        "fa fa-book-reader",
        "fa fa-clipboard-list"
      ))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em"
    )
  })
  output$measure_group <- shiny::renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("measure_group_dropdown"),
        icon = shiny::icon("gear"),
        label = "15+ Measures"
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("measure_chosen"), label = "Measures Chosen",
          choices = dMQIM$qim_15plus_measureTypes,
          selected = dMQIM$qim_15plus_measureTypes,
          # initially all chosen
          status = "primary"
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$measure_group_dropdown_dropmenu, ignoreInit = TRUE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$measure_group_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        dMQIM$qim_15plus_measure <- input$measure_chosen
      }
    })

  qim_15plus_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_15plus_listR(),
      dMQIM$qim_15plus_list_appointmentsR(),
      dMQIM$qim_15plus_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view,
        dMQIM$qim_15plus_listR()
      )
      if (input$list_view == "List") {
        df <- dMQIM$qim_15plus_listR() %>>% {
          remove_demographic <- setdiff(
            dMQIM$qim_demographicGroupings,
            dMQIM$qim_demographicGroup
          )
          # finds the demographics that were NOT chosen
          dplyr::select(., -c(remove_demographic, InternalID))
        } %>>% {
          if ("Smoking" %in% input$measure_chosen) {
            .
          }
          else {
            dplyr::select(., -c(SmokingDate, SmokingStatus))
          }
        } %>>% {
          if ("Weight" %in% input$measure_chosen) {
            .
          }
          else {
            dplyr::select(., -c(
              HeightDate, HeightValue, WeightDate, WeightValue,
              BMIDate, BMIValue, BMIClass,
              WaistDate, WaistValue
            ))
          }
        } %>>% {
          if ("Alcohol" %in% input$measure_chosen) {
            .
          }
          else {
            dplyr::select(., -c(
              AlcoholDate, NonDrinker, AlcoholDaysPerWeek,
              AlcoholDrinksPerDay, AlcoholDescription,
              PastAlcoholLevel, YearStarted, YearStopped,
              AlcoholComment
            ))
          }
        }
        dt <- datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          columnDefs = list(list(
            targets =
              which(names(df) %in%
                      c(
                        "Patient", "RecordNo", "HeightDate", "HeightValue",
                        "WeightDate", "WeightValue", "AlcoholDescription",
                        "PastAlcoholLevel", "YearStarted", "YearStopped",
                        "AlcoholComment"
                      )),
            # needs name by index as columns might be removed
            # by demographic filters above
            visible = FALSE
          )),
          # Patient Name and RecordNo hidden by default,
          # as well as various alcohol details etc.
          scrollX = TRUE
        )
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(
              columns = which(names(df) %in% c("BMIValue")),
              digits = 1
            )
        }
        return(dt)
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_15plus_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <-
          "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3) %>>%
            DT::formatRound(which(names(df) %in% c("% of demographic")), digits = 1)
        }
        return(dt)
      } else if (input$list_view == "Appointments") {
        df <- dMQIM$qim_15plus_list_appointmentsR() %>>%
          dplyr::select(
            Patient, RecordNo,
            AppointmentDate, AppointmentTime, Provider, Status,
            Age10, Sex, Indigenous, Ethnicity,
            MaritalStatus, Sexuality,
            SmokingDate, SmokingStatus,
            HeightDate, HeightValue, WeightDate, WeightValue,
            BMIDate, BMIValue, BMIClass,
            WaistDate, WaistValue,
            AlcoholDate, NonDrinker, AlcoholDaysPerWeek,
            AlcoholDrinksPerDay, AlcoholDescription,
            PastAlcoholLevel, YearStarted, YearStopped,
            AlcoholComment
          ) %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          } %>>% {
            if ("Smoking" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(SmokingDate, SmokingStatus))
            }
          } %>>% {
            if ("Weight" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(., -c(
                HeightDate, HeightValue, WeightDate, WeightValue,
                BMIDate, BMIValue, BMIClass,
                WaistDate, WaistValue
              ))
            }
          } %>>% {
            if ("Alcohol" %in% input$measure_chosen) {
              .
            }
            else {
              dplyr::select(
                .,
                -c(
                  AlcoholDate, NonDrinker, AlcoholDaysPerWeek,
                  AlcoholDrinksPerDay, AlcoholDescription,
                  PastAlcoholLevel, YearStarted, YearStopped,
                  AlcoholComment
                )
              )
            }
          }
        dt <- datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          scrollX = TRUE
        )
        return(dt) # this is a wide table
      }
    }
  )

  output$fifteenplus_qim_table <- DT::renderDT({
    qim_15plus_datatable()
  },
  server = TRUE
  )
}

##### Quality Improvement Measures 65+ ###############################################
#' Quality Improvement 65+ plus years - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_65plus <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c(
        "fa fa-book-reader",
        "fa fa-clipboard-list"
      ))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em"
    )
  })

  qim_65plus_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_65plus_listR(),
      dMQIM$qim_65plus_list_appointmentsR(),
      dMQIM$qim_65plus_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view,
        dMQIM$qim_65plus_listR()
      )
      if (input$list_view == "List") {
        df <- dMQIM$qim_65plus_listR() %>>% {
          remove_demographic <- setdiff(
            dMQIM$qim_demographicGroupings,
            dMQIM$qim_demographicGroup
          )
          # finds the demographics that were NOT chosen
          dplyr::select(., -c(remove_demographic, InternalID))
        }
        return(datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          columnDefs = list(list(
            targets =
              which(names(df) %in%
                      c("Patient", "RecordNo")),
            # needs name by index as columns might be removed
            # by demographic filters above
            visible = FALSE
          )),
          # Patient Name and RecordNo hidden by default
          scrollX = TRUE
        ))
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_65plus_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <-
          "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3) %>>%
            DT::formatRound(which(names(df) %in% c("% of demographic")), digits = 1)
        }
        return(dt)
      } else if (input$list_view == "Appointments") {
        df <- dMQIM$qim_65plus_list_appointmentsR() %>>%
          dplyr::select(
            Patient, RecordNo,
            AppointmentDate, AppointmentTime, Provider, Status,
            Age10, Sex, Indigenous, Ethnicity,
            MaritalStatus, Sexuality,
            FluvaxDate, FluvaxName
          ) %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          }
        datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          scrollX = TRUE
        ) # this is a wide table
      }
    }
  )

  output$sixtyfiveplus_qim_table <- DT::renderDT({
    qim_65plus_datatable()
  },
  server = TRUE
  )
}

##### Quality Improvement Measures COPD ###############################################
#' Quality Improvement COPD - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_copd <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c(
        "fa fa-book-reader",
        "fa fa-clipboard-list"
      ))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em"
    )
  })

  qim_copd_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_copd_listR(),
      dMQIM$qim_copd_list_appointmentsR(),
      dMQIM$qim_copd_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view,
        dMQIM$qim_copd_listR()
      )
      if (input$list_view == "List") {
        df <- dMQIM$qim_copd_listR() %>>% {
          remove_demographic <- setdiff(
            dMQIM$qim_demographicGroupings,
            dMQIM$qim_demographicGroup
          )
          # finds the demographics that were NOT chosen
          dplyr::select(., -c(remove_demographic, InternalID))
        }
        return(datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          columnDefs = list(list(
            targets =
              which(names(df) %in%
                      c("Patient", "RecordNo")),
            # needs name by index as columns might be removed
            # by demographic filters above
            visible = FALSE
          )),
          # Patient Name and RecordNo hidden by default
          scrollX = TRUE
        ))
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_copd_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <-
          "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3) %>>%
            DT::formatRound(which(names(df) %in% c("% of demographic")), digits = 1)
        }
        return(dt)
      } else if (input$list_view == "Appointments") {
        df <- dMQIM$qim_copd_list_appointmentsR() %>>%
          dplyr::select(
            Patient, RecordNo,
            AppointmentDate, AppointmentTime, Provider, Status,
            Age10, Sex, Indigenous, Ethnicity,
            MaritalStatus, Sexuality,
            FluvaxDate, FluvaxName
          ) %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          }
        datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          scrollX = TRUE
        ) # this is a wide table
      }
    }
  )

  output$copd_qim_table <- DT::renderDT({
    qim_copd_datatable()
  },
  server = TRUE
  )
}


##### Quality Improvement Measures cardiovascular risk ###############################################
#' Quality Improvement cardiovascular risk - server
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMQIM dMeasure QIM R6 object
#'  access to appointments lists, results, correspondence and EMR database
#' @param contact (logical) TRUE if using 'contact' list
#'     'contact' list uses active contact methods
#'     FALSE if using 'appointment' list
#'
#' @return none
#'
#' @export
qim_cvdRisk <- function(input, output, session, dMQIM, contact) {
  ns <- session$ns

  output$list_group <- shiny::renderUI({
    # chooses between Report, List and Appointment views
    # only Report, List view available in 'contact' list mode
    # only Appointment view available in 'appointment' list mode
    if (contact) {
      choices <- c("Report", "List")
      choicesOpt <- list(icon = c(
        "fa fa-book-reader",
        "fa fa-clipboard-list"
      ))
    } else {
      choices <- c("Appointments")
      choicesOpt <- list(icon = c("fa fa-calendar-check"))
    }
    shinyWidgets::pickerInput(
      inputId = ns("list_view"),
      choices = choices,
      choicesOpt = choicesOpt,
      options = list(`icon-base` = ""),
      width = "15em"
    )
  })
  output$groups <- shiny::renderUI({
    shinyWidgets::dropMenu(
      shiny::actionButton(
        inputId = ns("measure_group_dropdown"),
        icon = shiny::icon("gear"),
        label = "Inclusions/Exclusions"
      ),
      shiny::tags$div(
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("groups_chosen"), label = "Groups chosen",
          choices = dMQIM$qim_cvdRisk_measureTypes,
          selected = dMQIM$qim_cvdRisk_measureTypes,
          # initially all chosen, which includes choices to
          #  'include' ATSI 35-44 years,
          # and 'exclude'
          #  those with known cardiovascular disease and age 75
          status = "primary"
        ),
        shiny::br(),
        shiny::em("Close to confirm")
      ),
      placement = "bottom-end"
    )
  })
  shiny::observeEvent(
    input$measure_group_dropdown_dropmenu, ignoreInit = FALSE, {
      # this is triggered when shinyWidgets::dropMenu is opened/closed
      # tag is derived from the first tag in dropMenu, adding '_dropmenu'
      if (!input$measure_group_dropdown_dropmenu) {
        # only if closing the 'dropmenu' modal
        # unfortunately, is also triggered during Init (despite the ignoreInit)
        dMQIM$qim_cvdRisk_measure <- input$groups_chosen
      }
    })

  qim_cvdRisk_datatable <- shiny::eventReactive(
    c(
      input$list_view,
      dMQIM$qim_cvdRisk_listR(),
      dMQIM$qim_cvdRisk_list_appointmentsR(),
      dMQIM$qim_cvdRisk_reportR(),
      dMQIM$qim_demographicGroupR()
    ), {
      shiny::req(
        input$list_view,
        dMQIM$qim_cvdRisk_listR()
      )
      if (input$list_view == "List") {
        df <- dMQIM$qim_cvdRisk_listR() %>>% {
          remove_demographic <- setdiff(
            dMQIM$qim_demographicGroupings,
            dMQIM$qim_demographicGroup
          )
          # finds the demographics that were NOT chosen
          dplyr::select(., -c(remove_demographic, InternalID))
        } %>>%
          dplyr::mutate(
            CholHDLRatio = round(CholHDLRatio, 3),
            frisk = round(frisk, 3)
          )
        return(datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          columnDefs = list(list(
            targets =
              which(names(df) %in%
                      c("Patient", "RecordNo")),
            # needs name by index as columns might be removed
            # by demographic filters above
            visible = FALSE
          )),
          # Patient Name and RecordNo hidden by default
          scrollX = TRUE
        ))
      } else if (input$list_view == "Report") {
        df <- dMQIM$qim_cvdRisk_reportR()
        df[, which(colnames(df) == "Proportion_Demographic")] <-
          df[, which(colnames(df) == "Proportion_Demographic")] * 100
        colnames(df)[which(colnames(df) == "Proportion_Demographic")] <-
          "% of demographic"
        dt <- datatable_styled(df)
        if (dim(df)[[2]] > 0) {
          # not an empty dataframe
          dt <- dt %>>%
            DT::formatRound(which(names(df) %in% c("Proportion")), digits = 3) %>>%
            DT::formatRound(which(names(df) %in% c("% of demographic")), digits = 1)
        }
        return(dt)
      } else if (input$list_view == "Appointments") {
        df <- dMQIM$qim_cvdRisk_list_appointmentsR() %>>%
          dplyr::select(
            Patient, RecordNo,
            AppointmentDate, AppointmentTime, Provider, Status,
            Age10, Sex, Indigenous, Ethnicity,
            MaritalStatus, Sexuality,
            CardiovascularDisease, Diabetes,
            SmokingDate, SmokingStatus,
            UrineAlbuminDate, UrineAlbuminValue, UrineAlbuminUnits,
            PersistentProteinuria,
            eGFRDate, eGFRValue, eGFRUnits,
            FamilialHypercholesterolaemia, LVH,
            CholesterolDate, Cholesterol, HDL, LDL,
            Triglycerides, CholHDLRatio,
            BPDate, BP,
            frisk, friskHI
          ) %>>%
          dplyr::mutate(
            CholHDLRatio = round(CholHDLRatio, 3),
            frisk = round(frisk, 3)
          ) %>>%
          # re-orders the fields
          {
            remove_demographic <- setdiff(
              dMQIM$qim_demographicGroupings,
              dMQIM$qim_demographicGroup
            )
            # finds the demographics that were NOT chosen
            dplyr::select(., -remove_demographic)
          }
        datatable_styled(
          df,
          extensions = c("Buttons", "Scroller"),
          scrollX = TRUE
        ) # this is a wide table
      }
    }
  )

  output$cvdRisk_qim_table <- DT::renderDT({
    qim_cvdRisk_datatable()
  },
  server = TRUE
  )
}

