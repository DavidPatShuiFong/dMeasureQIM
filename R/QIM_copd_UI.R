# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
