# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
