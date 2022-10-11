# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
