# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Interface elements of dMeasure Billings
#'
#' requires R6 methods from QualityImprovementMeasures.R
#'
#' @include QualityImprovementMeasures.R
#'
#' require UI (and server) functions from sub-modules
#'
#' @include QIM_15plus_UI.R
#' @include QIM_65plus_UI.R
#' @include QIM_active_UI.R
#' @include QIM_copd_UI.R
#' @include QIM_cst_UI.R
#' @include QIM_cvdRisk_UI.R
#' @include QIM_diabetes_UI.R
#' @include QIM_report_UI.R
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
